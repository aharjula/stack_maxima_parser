//! The parser fails on bad input instead of panicking or looping: it runs in WASM, where
//! a panic aborts the module and a loop freezes the page.

use stack_maxima_parser::parser::StackMaximaParser;

#[test]
fn lexer_errors_are_parse_failures() {
	for (src, what) in [("a: \"abc", "STRING NOT TERMINATED"), ("/* x", "COMMENT NOT TERMINATED")] {
		let mut p = StackMaximaParser::new_no_insertions();
		assert!(p.parse(src.to_string()).is_none(), "{src}");
		assert!(p.errors.iter().any(|e| e.contains(what)), "{src}: {:?}", p.errors);
		// The errors belong to the last parse only.
		assert!(p.parse("a: 1;".to_string()).is_some());
		assert!(p.errors.is_empty(), "{:?}", p.errors);
	}
}

#[test]
fn unknown_tokens_and_the_end_of_input_fail_cleanly() {
	for src in ["+[thru \\<>=", "while \\.", "1??lambda([x],lambda([x],{'|+]then ", "while thru \\{!nounaddelse and"] {
		let mut p = StackMaximaParser::new_no_insertions();
		assert!(p.parse(src.to_string()).is_none(), "{src}");
	}
}

#[test]
fn semicolon_insertion_terminates() {
	// Inserting `;` before a token that still does not fit used to repeat for ever.
	for src in ["else", "then 1", "elseif not \"x\"", "do do do"] {
		let mut p = StackMaximaParser::new_with_insert_semicolons();
		let _ = p.parse(src.to_string());
	}
}

/// Random token soup in all modes, deterministic (a fixed linear congruential generator).
#[test]
fn random_input_never_panics() {
	let toks = [
		"a", "1", "2.5", "\"s\"", "(", ")", "[", "]", "{", "}", ",", ";", "$", ":", ":=", "+", "-", "*",
		"/", "^", "^-", ".", "!", "=", "#", "<", "and", "or", "not ", "nand", "if ", "then ", "else ",
		"do ", "for ", "in ", "thru ", "while ", "'", "?", "@", "#pm#", "nounadd", "nounsub ", " ",
		"/* c */", "f(", "block([x],", "lambda([x],", "|", "\"", "/*", "\\", "~", "ä",
	];
	let mut seed: u64 = 12345;
	let mut next = || {
		seed = seed.wrapping_mul(6364136223846793005).wrapping_add(1442695040888963407);
		(seed >> 33) as usize
	};
	for _ in 0..5000 {
		let n = 1 + next() % 20;
		let src: String = (0..n).map(|_| toks[next() % toks.len()]).collect();
		for mut p in [
			StackMaximaParser::new_no_insertions(),
			StackMaximaParser::new_with_insert_semicolons(),
			StackMaximaParser::new_with_insert_stars(),
		] {
			let s = src.clone();
			let r = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| p.parse(s)));
			assert!(r.is_ok(), "panicked on {src:?}");
		}
	}
}
