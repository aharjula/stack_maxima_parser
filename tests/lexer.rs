use stack_maxima_parser::lexer::{StackMaximaLexer, StackMaximaToken, StackMaximaParserTokenType};


#[test]
fn basic_int() {
	let test = String::from("123-456(789)");

	let mut lexer = StackMaximaLexer::new(test);

	let token0: StackMaximaToken = lexer.next_token().expect("Should have 6 tokens.");

	assert_eq!(token0.startline, 1);
	assert_eq!(token0.startcolumn, 1);
	assert_eq!(token0.length, 3);
	assert_eq!(token0.endline, 1);
	assert_eq!(token0.endcolumn, 3);
	if let StackMaximaParserTokenType::INT(translated, raw) = token0.value {
		assert_eq!(translated, "123");
		assert_eq!(raw, "123");
	} else {
		assert!(false, "Expected INT got something else.");
	}

	let token1: StackMaximaToken = lexer.next_token().expect("Should have 6 tokens.");

	assert_eq!(token1.startline, 1);
	assert_eq!(token1.startcolumn, 4);
	assert_eq!(token1.length, 1);
	assert_eq!(token1.endline, 1);
	assert_eq!(token1.endcolumn, 4);
	if let StackMaximaParserTokenType::SYMBOL(raw) = token1.value {
		assert_eq!(raw, "-");
	} else {
		assert!(false, "Expected SYMBOL got something else.");
	}

	let token2: StackMaximaToken = lexer.next_token().expect("Should have 6 tokens.");

	assert_eq!(token2.startline, 1);
	assert_eq!(token2.startcolumn, 5);
	assert_eq!(token2.length, 3);
	assert_eq!(token2.endline, 1);
	assert_eq!(token2.endcolumn, 7);
	if let StackMaximaParserTokenType::INT(translated, raw) = token2.value {
		assert_eq!(translated, "456");
		assert_eq!(raw, "456");
	} else {
		assert!(false, "Expected INT got something else.");
	}

	let token3: StackMaximaToken = lexer.next_token().expect("Should have 6 tokens.");

	assert_eq!(token3.startline, 1);
	assert_eq!(token3.startcolumn, 8);
	assert_eq!(token3.length, 1);
	assert_eq!(token3.endline, 1);
	assert_eq!(token3.endcolumn, 8);
	if let StackMaximaParserTokenType::SYMBOL(raw) = token3.value {
		assert_eq!(raw, "(");
	} else {
		assert!(false, "Expected SYMBOL got something else.");
	}

	let token4: StackMaximaToken = lexer.next_token().expect("Should have 6 tokens.");

	assert_eq!(token4.startline, 1);
	assert_eq!(token4.startcolumn, 9);
	assert_eq!(token4.length, 3);
	assert_eq!(token4.endline, 1);
	assert_eq!(token4.endcolumn, 11);
	if let StackMaximaParserTokenType::INT(translated, raw) = token4.value {
		assert_eq!(translated, "789");
		assert_eq!(raw, "789");
	} else {
		assert!(false, "Expected INT got something else.");
	}

	let token5: StackMaximaToken = lexer.next_token().expect("Should have 6 tokens.");

	assert_eq!(token5.startline, 1);
	assert_eq!(token5.startcolumn, 12);
	assert_eq!(token5.length, 1);
	assert_eq!(token5.endline, 1);
	assert_eq!(token5.endcolumn, 12);
	if let StackMaximaParserTokenType::SYMBOL(raw) = token5.value {
		assert_eq!(raw, ")");
	} else {
		assert!(false, "Expected SYMBOL got something else.");
	}

	// Then no more tokens.
	assert_eq!(lexer.next_token(), None);
}


#[test]
fn basic_float() {
	let test = String::from("1.23  4e56-7.8E-90");

	let mut lexer = StackMaximaLexer::new(test);

	let token0: StackMaximaToken = lexer.next_token().expect("Should have 5 tokens.");

	assert_eq!(token0.startline, 1);
	assert_eq!(token0.startcolumn, 1);
	assert_eq!(token0.length, 4);
	assert_eq!(token0.endline, 1);
	assert_eq!(token0.endcolumn, 4);
	if let StackMaximaParserTokenType::FLOAT(translated, raw) = token0.value {
		assert_eq!(translated, "1.23");
		assert_eq!(raw, "1.23");
	} else {
		assert!(false, "Expected FLOAT got something else.");
	}

	let token1: StackMaximaToken = lexer.next_token().expect("Should have 5 tokens.");

	assert_eq!(token1.startline, 1);
	assert_eq!(token1.startcolumn, 5);
	assert_eq!(token1.length, 2);
	assert_eq!(token1.endline, 1);
	assert_eq!(token1.endcolumn, 6);
	if let StackMaximaParserTokenType::WS(raw) = token1.value {
		assert_eq!(raw, "  ");
	} else {
		assert!(false, "Expected WS got something else.");
	}

	let token2: StackMaximaToken = lexer.next_token().expect("Should have 5 tokens.");

	assert_eq!(token2.startline, 1);
	assert_eq!(token2.startcolumn, 7);
	assert_eq!(token2.length, 4);
	assert_eq!(token2.endline, 1);
	assert_eq!(token2.endcolumn, 10);
	if let StackMaximaParserTokenType::FLOAT(translated, raw) = token2.value {
		assert_eq!(translated, "4e56");
		assert_eq!(raw, "4e56");
	} else {
		assert!(false, "Expected FLOAT got something else.");
	}

	let token3: StackMaximaToken = lexer.next_token().expect("Should have 5 tokens.");

	assert_eq!(token3.startline, 1);
	assert_eq!(token3.startcolumn, 11);
	assert_eq!(token3.length, 1);
	assert_eq!(token3.endline, 1);
	assert_eq!(token3.endcolumn, 11);
	if let StackMaximaParserTokenType::SYMBOL(raw) = token3.value {
		assert_eq!(raw, "-");
	} else {
		assert!(false, "Expected SYMBOL got something else.");
	}

	let token4: StackMaximaToken = lexer.next_token().expect("Should have 5 tokens.");

	assert_eq!(token4.startline, 1);
	assert_eq!(token4.startcolumn, 12);
	assert_eq!(token4.length, 7);
	assert_eq!(token4.endline, 1);
	assert_eq!(token4.endcolumn, 18);
	if let StackMaximaParserTokenType::FLOAT(translated, raw) = token4.value {
		assert_eq!(translated, "7.8E-90");
		assert_eq!(raw, "7.8E-90");
	} else {
		assert!(false, "Expected FLOAT got something else.");
	}

	// Then no more tokens.
	assert_eq!(lexer.next_token(), None);
}

#[test]
fn basic_float_dec_sep() {
	let test = String::from("1.23 4,5e6+7,89");

	let mut lexer = StackMaximaLexer::new(test);
	lexer.decimal_sep = ',';
	lexer.list_sep = ';';

	let token0: StackMaximaToken = lexer.next_token().expect("Should have 7 tokens.");

	assert_eq!(token0.startline, 1);
	assert_eq!(token0.startcolumn, 1);
	assert_eq!(token0.length, 1);
	assert_eq!(token0.endline, 1);
	assert_eq!(token0.endcolumn, 1);
	if let StackMaximaParserTokenType::INT(translated, raw) = token0.value {
		assert_eq!(translated, "1");
		assert_eq!(raw, "1");
	} else {
		assert!(false, "Expected INT got something else.");
	}

	let token1: StackMaximaToken = lexer.next_token().expect("Should have 7 tokens.");

	assert_eq!(token1.startline, 1);
	assert_eq!(token1.startcolumn, 2);
	assert_eq!(token1.length, 1);
	assert_eq!(token1.endline, 1);
	assert_eq!(token1.endcolumn, 2);
	if let StackMaximaParserTokenType::SYMBOL(raw) = token1.value {
		assert_eq!(raw, ".");
	} else {
		assert!(false, "Expected SYMBOL got something else.");
	}

	let token2: StackMaximaToken = lexer.next_token().expect("Should have 7 tokens.");

	assert_eq!(token2.startline, 1);
	assert_eq!(token2.startcolumn, 3);
	assert_eq!(token2.length, 2);
	assert_eq!(token2.endline, 1);
	assert_eq!(token2.endcolumn, 4);
	if let StackMaximaParserTokenType::INT(translated, raw) = token2.value {
		assert_eq!(translated, "23");
		assert_eq!(raw, "23");
	} else {
		assert!(false, "Expected INT got something else.");
	}

	let token3: StackMaximaToken = lexer.next_token().expect("Should have 7 tokens.");

	assert_eq!(token3.startline, 1);
	assert_eq!(token3.startcolumn, 5);
	assert_eq!(token3.length, 1);
	assert_eq!(token3.endline, 1);
	assert_eq!(token3.endcolumn, 5);
	if let StackMaximaParserTokenType::WS(raw) = token3.value {
		assert_eq!(raw, " ");
	} else {
		assert!(false, "Expected WS got something else.");
	}

	let token4: StackMaximaToken = lexer.next_token().expect("Should have 7 tokens.");

	assert_eq!(token4.startline, 1);
	assert_eq!(token4.startcolumn, 6);
	assert_eq!(token4.length, 5);
	assert_eq!(token4.endline, 1);
	assert_eq!(token4.endcolumn, 10);
	if let StackMaximaParserTokenType::FLOAT(translated, raw) = token4.value {
		assert_eq!(translated, "4.5e6");
		assert_eq!(raw, "4,5e6");
	} else {
		assert!(false, "Expected FLOAT got something else.");
	}

	let token5: StackMaximaToken = lexer.next_token().expect("Should have 7 tokens.");

	assert_eq!(token5.startline, 1);
	assert_eq!(token5.startcolumn, 11);
	assert_eq!(token5.length, 1);
	assert_eq!(token5.endline, 1);
	assert_eq!(token5.endcolumn, 11);
	if let StackMaximaParserTokenType::SYMBOL(raw) = token5.value {
		assert_eq!(raw, "+");
	} else {
		assert!(false, "Expected SYMBOL got something else.");
	}

	let token6: StackMaximaToken = lexer.next_token().expect("Should have 7 tokens.");

	assert_eq!(token6.startline, 1);
	assert_eq!(token6.startcolumn, 12);
	assert_eq!(token6.length, 4);
	assert_eq!(token6.endline, 1);
	assert_eq!(token6.endcolumn, 15);
	if let StackMaximaParserTokenType::FLOAT(translated, raw) = token6.value {
		assert_eq!(translated, "7.89");
		assert_eq!(raw, "7,89");
	} else {
		assert!(false, "Expected FLOAT got something else.");
	}

	// Then no more tokens.
	assert_eq!(lexer.next_token(), None);
}

#[test]
fn keywords() {
	let test = String::from("if true then do while in unless thru");

	let mut lexer = StackMaximaLexer::new(test);	

	let token0: StackMaximaToken = lexer.next_token().expect("Should have 15 tokens.");

	assert_eq!(token0.startline, 1);
	assert_eq!(token0.startcolumn, 1);
	assert_eq!(token0.length, 2);
	assert_eq!(token0.endline, 1);
	assert_eq!(token0.endcolumn, 2);
	if let StackMaximaParserTokenType::KEYWORD(translated, raw) = token0.value {
		assert_eq!(translated, "if");
		assert_eq!(raw, "if");
	} else {
		assert!(false, "Expected KEYWORD got something else.");
	}

	let token1: StackMaximaToken = lexer.next_token().expect("Should have 15 tokens.");

	assert_eq!(token1.startline, 1);
	assert_eq!(token1.startcolumn, 3);
	assert_eq!(token1.length, 1);
	assert_eq!(token1.endline, 1);
	assert_eq!(token1.endcolumn, 3);
	if let StackMaximaParserTokenType::WS(raw) = token1.value {
		assert_eq!(raw, " ");
	} else {
		assert!(false, "Expected WS got something else.");
	}

	let token2: StackMaximaToken = lexer.next_token().expect("Should have 15 tokens.");

	assert_eq!(token2.startline, 1);
	assert_eq!(token2.startcolumn, 4);
	assert_eq!(token2.length, 4);
	assert_eq!(token2.endline, 1);
	assert_eq!(token2.endcolumn, 7);
	if let StackMaximaParserTokenType::BOOL(translated, raw) = token2.value {
		assert_eq!(translated, true);
		assert_eq!(raw, "true");
	} else {
		assert!(false, "Expected KEYWORD got something else.");
	}

	let token4: StackMaximaToken = lexer.next_token().expect("Should have 15 tokens.");

	assert_eq!(token4.startline, 1);
	assert_eq!(token4.startcolumn, 8);
	assert_eq!(token4.length, 1);
	assert_eq!(token4.endline, 1);
	assert_eq!(token4.endcolumn, 8);
	if let StackMaximaParserTokenType::WS(raw) = token4.value {
		assert_eq!(raw, " ");
	} else {
		assert!(false, "Expected WS got something else.");
	}

	let token5: StackMaximaToken = lexer.next_token().expect("Should have 15 tokens.");

	assert_eq!(token5.startline, 1);
	assert_eq!(token5.startcolumn, 9);
	assert_eq!(token5.length, 4);
	assert_eq!(token5.endline, 1);
	assert_eq!(token5.endcolumn, 12);
	if let StackMaximaParserTokenType::KEYWORD(translated, raw) = token5.value {
		assert_eq!(translated, "then");
		assert_eq!(raw, "then");
	} else {
		assert!(false, "Expected KEYWORD got something else.");
	}

	let token6: StackMaximaToken = lexer.next_token().expect("Should have 15 tokens.");

	assert_eq!(token6.startline, 1);
	assert_eq!(token6.startcolumn, 13);
	assert_eq!(token6.length, 1);
	assert_eq!(token6.endline, 1);
	assert_eq!(token6.endcolumn, 13);
	if let StackMaximaParserTokenType::WS(raw) = token6.value {
		assert_eq!(raw, " ");
	} else {
		assert!(false, "Expected WS got something else.");
	}
}