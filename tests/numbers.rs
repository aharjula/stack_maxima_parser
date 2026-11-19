//! Numbers as Maxima reads them (checked with `tools/lexer_oracle` in the generator).

use stack_maxima_parser::lexer::{StackMaximaLexer, StackMaximaParserTokenType as T};

fn tokens(src: &str) -> Vec<T> {
    let mut l = StackMaximaLexer::new(src.to_string());
    std::iter::from_fn(|| l.next_token().map(|t| t.value)).collect()
}

fn int(v: &str, orig: &str) -> T {
    T::INT(v.into(), orig.into())
}

fn float(v: &str, orig: &str) -> T {
    T::FLOAT(v.into(), orig.into())
}

#[test]
fn a_trailing_point_makes_an_integer() {
    assert_eq!(tokens("12."), [int("12", "12.")]);
    assert_eq!(tokens("1.5"), [float("1.5", "1.5")]);
    assert_eq!(tokens(".5"), [float(".5", ".5")]);
    assert_eq!(tokens("1.e3"), [float("1.e3", "1.e3")]);
    // `1..x` is `1. . x`.
    assert_eq!(tokens("1..x"), [int("1", "1."), T::SYMBOL(".".into()), T::ID("x".into())]);
}

#[test]
fn a_point_before_a_letter_is_the_dot_operator() {
    assert_eq!(tokens("a.e"), [T::ID("a".into()), T::SYMBOL(".".into()), T::ID("e".into())]);
    assert_eq!(tokens("x.e1"), [T::ID("x".into()), T::SYMBOL(".".into()), T::ID("e1".into())]);
}

#[test]
fn exponent_markers() {
    assert_eq!(tokens("1e3"), [float("1e3", "1e3")]);
    assert_eq!(tokens("1E-3"), [float("1E-3", "1E-3")]);
    for m in ["d", "f", "s", "l", "D", "F", "S", "L"] {
        assert_eq!(tokens(&format!("2{m}+1")), [float("2e+1", &format!("2{m}+1"))]);
    }
    // A bigfloat is a FLOAT whose original text keeps the `b`.
    assert_eq!(tokens("1b5"), [float("1e5", "1b5")]);
    // Too large for a double: left to the users of the parser.
    assert_eq!(tokens("1e500"), [float("1e500", "1e500")]);
}

#[test]
fn an_exponent_needs_digits() {
    for src in ["1e", "1e+", "2.5E", "1ex"] {
        assert!(matches!(&tokens(src)[0], T::ERROR(e) if e.contains("has no digits")), "{src}");
    }
}
