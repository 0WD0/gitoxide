use std::ops::Range;

use bstr::{BStr, ByteSlice};

/// One side of a materialized textual conflict.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Term<'a> {
    /// Optional text following the term's marker.
    pub label: Option<&'a BStr>,
    /// Exact bytes between this term's marker and the next structural marker.
    pub content: &'a [u8],
}

/// One complete Git merge or diff3 marker block.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Conflict<'a> {
    /// Exact byte range of the complete block, including outer marker lines.
    pub source: Range<usize>,
    /// Number of repeated marker characters.
    pub marker_size: usize,
    /// Current/ours term.
    pub current: Term<'a>,
    /// Optional common-ancestor term for diff3 materialization.
    pub ancestor: Option<Term<'a>>,
    /// Other/theirs term.
    pub other: Term<'a>,
}

#[derive(Clone, Copy)]
struct Line {
    start: usize,
    content_end: usize,
    end: usize,
}

fn lines(input: &[u8]) -> impl Iterator<Item = Line> + '_ {
    let mut start = 0;
    std::iter::from_fn(move || {
        if start >= input.len() {
            return None;
        }
        let newline = input[start..].iter().position(|byte| *byte == b'\n');
        let end = newline.map_or(input.len(), |offset| start + offset + 1);
        let mut content_end = newline.map_or(end, |offset| start + offset);
        if content_end > start && input[content_end - 1] == b'\r' {
            content_end -= 1;
        }
        let line = Line {
            start,
            content_end,
            end,
        };
        start = end;
        Some(line)
    })
}

fn marker<'a>(line: &'a [u8], byte: u8, minimum_size: usize, label_allowed: bool) -> Option<(usize, Option<&'a BStr>)> {
    let size = line.iter().take_while(|candidate| **candidate == byte).count();
    if size < minimum_size {
        return None;
    }
    let label = match line.get(size) {
        None => None,
        Some(b' ' | b'\t') if label_allowed => {
            let label = line[size + 1..].trim_start();
            (!label.is_empty()).then(|| label.as_bstr())
        }
        _ => return None,
    };
    Some((size, label))
}

/// Parse complete Git merge/diff3 marker blocks from `input`.
///
/// This is a strict grammar parser, not a conflict-authority check. Callers
/// should establish that the owning index records the path as unmerged before
/// treating returned blocks as repository conflicts.
pub fn conflicts(input: &[u8], minimum_marker_size: usize) -> Vec<Conflict<'_>> {
    #[derive(Clone, Copy)]
    struct Active<'a> {
        source_start: usize,
        marker_size: usize,
        current_label: Option<&'a BStr>,
        current_start: usize,
        current_end: Option<usize>,
        ancestor_label: Option<&'a BStr>,
        ancestor_start: Option<usize>,
        ancestor_end: Option<usize>,
        other_start: Option<usize>,
    }

    let mut out = Vec::new();
    let mut active: Option<Active<'_>> = None;
    for line in lines(input) {
        let line_content = &input[line.start..line.content_end];
        if let Some((marker_size, label)) = marker(line_content, b'<', minimum_marker_size, true) {
            active = Some(Active {
                source_start: line.start,
                marker_size,
                current_label: label,
                current_start: line.end,
                current_end: None,
                ancestor_label: None,
                ancestor_start: None,
                ancestor_end: None,
                other_start: None,
            });
            continue;
        }
        let Some(mut current) = active.take() else {
            continue;
        };
        if let Some((size, label)) = marker(line_content, b'|', current.marker_size, true)
            && size == current.marker_size
            && current.other_start.is_none()
            && current.ancestor_start.is_none()
        {
            current.current_end = Some(line.start);
            current.ancestor_label = label;
            current.ancestor_start = Some(line.end);
            active = Some(current);
            continue;
        }
        if let Some((size, _)) = marker(line_content, b'=', current.marker_size, false)
            && size == current.marker_size
            && current.other_start.is_none()
        {
            if current.ancestor_start.is_some() {
                current.ancestor_end = Some(line.start);
            } else {
                current.current_end = Some(line.start);
            }
            current.other_start = Some(line.end);
            active = Some(current);
            continue;
        }
        if let Some((size, other_label)) = marker(line_content, b'>', current.marker_size, true)
            && size == current.marker_size
        {
            if let (Some(current_end), Some(other_start)) = (current.current_end, current.other_start) {
                let ancestor = current
                    .ancestor_start
                    .zip(current.ancestor_end)
                    .map(|(start, end)| Term {
                        label: current.ancestor_label,
                        content: &input[start..end],
                    });
                out.push(Conflict {
                    source: current.source_start..line.end,
                    marker_size: current.marker_size,
                    current: Term {
                        label: current.current_label,
                        content: &input[current.current_start..current_end],
                    },
                    ancestor,
                    other: Term {
                        label: other_label,
                        content: &input[other_start..line.start],
                    },
                });
            }
            continue;
        }
        active = Some(current);
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_merge_diff3_crlf_and_materialized_sizes() {
        let input = b"before\r\n<<< ours\r\nleft\r\n||| base\r\nold\r\n===\r\nright\r\n>>> theirs\r\nafter\r\n";
        let parsed = conflicts(input, 1);
        assert_eq!(parsed.len(), 1);
        let conflict = &parsed[0];
        assert_eq!(
            &input[conflict.source.clone()],
            b"<<< ours\r\nleft\r\n||| base\r\nold\r\n===\r\nright\r\n>>> theirs\r\n"
        );
        assert_eq!(conflict.marker_size, 3);
        assert_eq!(
            conflict.current,
            Term {
                label: Some("ours".into()),
                content: b"left\r\n"
            }
        );
        assert_eq!(
            conflict.ancestor,
            Some(Term {
                label: Some("base".into()),
                content: b"old\r\n"
            })
        );
        assert_eq!(
            conflict.other,
            Term {
                label: Some("theirs".into()),
                content: b"right\r\n"
            }
        );
    }

    #[test]
    fn ignores_jj_snapshot_grammar_and_incomplete_blocks() {
        let input =
            b"<<<<<<< conflict\n+++++++ side 1\nleft\n+++++++ side 2\nright\n>>>>>>> end\n<<<<<<< incomplete\nleft\n";
        assert!(conflicts(input, 7).is_empty());
    }
}
