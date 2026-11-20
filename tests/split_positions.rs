//! An operator followed directly by a prefix operator (`a*-b`) is lexed as one token and
//! split by the parser; both parts keep their own positions.

use stack_maxima_parser::parser::{MPNode, MPNodeType, StackMaximaParser};

fn find_prefix(n: &MPNode) -> Option<&MPNode> {
    match &n.value {
        MPNodeType::PrefixOperation(..) => Some(n),
        MPNodeType::Operation(l, _, r, _) => find_prefix(l).or_else(|| find_prefix(r)),
        MPNodeType::Root(s, _, _) => s.iter().find_map(find_prefix),
        MPNodeType::Statement(x, _) => find_prefix(x),
        _ => None,
    }
}

#[test]
fn the_prefix_operator_starts_where_it_is_written() {
    for (src, start) in [("a*-b;", 2), ("a * -b;", 4), ("a^-b;", 2)] {
        let mut p = StackMaximaParser::new_no_insertions();
        let root = p.parse(src.to_string()).expect(src);
        let pre = find_prefix(&root).expect("a prefix operation");
        assert_eq!(pre.position.startbyte, start, "{src}: {:?}", pre.position);
        assert_eq!(&src[pre.position.startbyte..pre.position.endbyte], &src[start..src.len() - 1], "{src}");
    }
}

#[test]
fn split_parts_have_their_own_columns_bytes_and_lengths() {
    use stack_maxima_parser::lexer::{StackMaximaParserTokenType as T, StackMaximaToken};
    // `^-` at columns 2..3 (inclusive) and bytes 1..3 (exclusive), as in `a^-b`.
    let combo = StackMaximaToken {
        startline: 1,
        startcolumn: 2,
        length: 2,
        endline: 1,
        endcolumn: 3,
        startbyte: 1,
        endbyte: 3,
        value: T::SYMBOL("^ -".into()),
    };
    let (a, b) = combo.split();
    assert_eq!((a.startcolumn, a.endcolumn, a.startbyte, a.endbyte, a.length), (2, 2, 1, 2, 1));
    assert_eq!((b.startcolumn, b.endcolumn, b.startbyte, b.endbyte, b.length), (3, 3, 2, 3, 1));
}
