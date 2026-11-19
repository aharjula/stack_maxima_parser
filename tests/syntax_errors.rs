//! Code that does not parse says why: what was found where (lines and columns from 1), or
//! that the input ended too early.

use stack_maxima_parser::parser::StackMaximaParser;

fn reason(src: &str, insert: bool) -> Vec<String> {
    let mut p = if insert {
        StackMaximaParser::new_with_insert_semicolons()
    } else {
        StackMaximaParser::new_no_insertions()
    };
    assert!(p.parse(src.to_string()).is_none(), "{src} parses");
    p.errors.clone()
}

#[test]
fn incomplete_code() {
    for src in ["a: [1, 2", "f(x", "if a then"] {
        for insert in [false, true] {
            assert_eq!(reason(src, insert), ["the input ends too early"], "{src}");
        }
    }
}

#[test]
fn unexpected_tokens() {
    assert_eq!(reason("a: 1 +;", true), ["unexpected `;` at line 1, column 7"]);
    assert_eq!(reason("a: (1));", true), ["unexpected `)` at line 1, column 7"]);
    assert_eq!(reason("x: 1 2;", false), ["unexpected `2` at line 1, column 6"]);
    assert_eq!(reason("a: 1;\nb: [1,, 2];", true), ["unexpected `,` at line 2, column 7"]);
}

#[test]
fn lexer_errors_count_columns_from_one() {
    let r = reason("a: \"abc", true);
    assert_eq!(r.len(), 1);
    assert!(r[0].ends_with("at line 1, column 4"), "{r:?}");
}
