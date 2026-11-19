//! The lexer covers the whole input: its tokens are contiguous from the first byte to the
//! last, whatever the input, so nothing is dropped or read twice. An unknown character is
//! an error token, not the end of the input.

use stack_maxima_parser::lexer::{StackMaximaLexer, StackMaximaParserTokenType};
use stack_maxima_parser::parser::StackMaximaParser;

const CHARS: &[char] = &[
    'a', 'e', 'x', '1', '0', '.', ',', ';', '$', '+', '-', '*', '/', '^', '!', '#', '%', '_',
    '\'', '"', '\\', '?', ':', '=', '<', '>', '(', ')', '[', ']', '{', '}', '&', '`', '|', '@',
    '~', ' ', '\t', '\n', '\r', '\u{a0}', 'π', '变', '×', '−', '😀', '\u{feff}',
];

fn covers(src: &str) -> Result<(), String> {
    let mut l = StackMaximaLexer::new(src.to_string());
    let mut at = 0;
    while let Some(t) = l.next_token() {
        if t.startbyte != at {
            return Err(format!("{src:?}: a token starts at byte {} after {at}", t.startbyte));
        }
        at = t.endbyte;
    }
    if at != src.len() {
        return Err(format!("{src:?}: the tokens end at byte {at} of {}", src.len()));
    }
    Ok(())
}

#[test]
fn tokens_cover_every_short_input() {
    let mut bad = Vec::new();
    let mut src = String::new();
    fn all(src: &mut String, depth: usize, bad: &mut Vec<String>) {
        if !src.is_empty() {
            if let Err(e) = covers(src) {
                bad.push(e);
            }
        }
        if depth == 0 {
            return;
        }
        for &c in CHARS {
            src.push(c);
            all(src, depth - 1, bad);
            src.pop();
        }
    }
    all(&mut src, 3, &mut bad);
    assert!(bad.is_empty(), "{} inputs, e.g. {:?}", bad.len(), &bad[..bad.len().min(10)]);
}

#[test]
fn unknown_characters_are_errors_with_a_reason() {
    for (src, c) in [("a: 1 \\ b: 2;", '\\'), ("a & b;", '&'), ("x: 2 × 3;", '×'), ("\u{feff}x;", '\u{feff}')] {
        let mut p = StackMaximaParser::new_with_insert_semicolons();
        assert!(p.parse(src.to_string()).is_none(), "{src:?} parses");
        assert!(p.errors[0].starts_with(&format!("unexpected character `{c}`")), "{:?}", p.errors);
    }
}

#[test]
fn whitespace_and_letters_as_in_maxima() {
    // Windows line endings, non-breaking spaces, and letters of any script.
    for src in ["a: 1;\r\nb: 2;\r\n", "a:\u{a0}1;", "变量: 1;", "xاy: 2;"] {
        let mut p = StackMaximaParser::new_no_insertions();
        assert!(p.parse(src.to_string()).is_some(), "{src:?}: {:?}", p.errors);
    }
    let mut l = StackMaximaLexer::new("变量".to_string());
    assert!(matches!(l.next_token().unwrap().value, StackMaximaParserTokenType::ID(ref v) if v == "变量"));
}
