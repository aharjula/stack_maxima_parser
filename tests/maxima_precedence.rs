//! Operator grouping for every pair of binary operators, prefix operators in front and the
//! postfix ones, as Maxima itself groups them with STACK's operator declarations loaded.
//! The expectations were checked against Maxima by
//! `stack-parser-generator-utils/tools/maxima_oracle`; rerun that after grammar changes.

use stack_maxima_parser::parser::{MPNode, MPNodeType, StackMaximaParser};

fn sx(n: &MPNode) -> String {
	match &n.value {
		MPNodeType::Identifier(s) => s.clone(),
		MPNodeType::Integer(s, _) | MPNodeType::Float(s, _) => s.clone(),
		MPNodeType::Operation(l, op, r, _) => format!("(\"{}\" {} {})", op.trim(), sx(l), sx(r)),
		MPNodeType::PrefixOperation(op, x) => format!("(\"pre{}\" {})", op.trim(), sx(x)),
		MPNodeType::PostfixOperation(x, op) => format!("(\"post{}\" {})", op.trim(), sx(x)),
		MPNodeType::Group(xs) if xs.len() == 1 => sx(&xs[0]),
		MPNodeType::Statement(x, _) => sx(x),
		MPNodeType::Root(s, _, _) if s.len() == 1 => sx(&s[0]),
		_ => format!("<{}>", n.type_short()),
	}
}

#[test]
fn grouping_matches_maxima() {
	let data = include_str!("data/maxima_precedence.txt");
	let mut wrong = Vec::new();
	let mut count = 0;
	for line in data.lines().filter(|l| !l.starts_with('#') && !l.is_empty()) {
		let (src, want) = line.split_once('\t').expect("input<TAB>tree");
		let mut p = StackMaximaParser::new_no_insertions();
		let got = p.parse(format!("{src};")).map(|r| sx(&r)).unwrap_or_else(|| "SYNTAXERROR".into());
		if got != want {
			wrong.push(format!("{src}\n    got  {got}\n    want {want}"));
		}
		count += 1;
	}
	assert!(count > 1000, "only {count} cases");
	assert!(wrong.is_empty(), "{} of {count} wrong:\n{}", wrong.len(), wrong.join("\n"));
}
