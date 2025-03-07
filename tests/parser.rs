use stack_maxima_parser::parser::{StackMaximaParser, MPNode, MPNodeType, StackStringUsage};


#[test]
fn basic_int() {
	let test = String::from("123 - 456 *789");
	let mut parser = StackMaximaParser::new_no_insertions();

	let result: Option<MPNode> = parser.parse(test.clone());

	// The result should be a Root object with one statement.
	if let MPNodeType::Root(statements,_,_) = result.clone().expect("Should have a result from parsing.").value {
		assert_eq!(statements.len(), 1);
		// The only statement should not have any eval-flags.
		if let MPNodeType::Statement(expr, flags) = &statements[0].value {
			assert_eq!(flags.len(), 0);
			// The expressions should be a '-' opration.
			if let MPNodeType::Operation(lhs, op, rhs, _) = &expr.value {
				assert_eq!(op, "-");
				// The lhs being an integer of 123.
				if let MPNodeType::Integer(val, _) = &lhs.value {
					assert_eq!(val, "123");
				} else {
					assert!(false, "Not an integer object was expecting one.");	
				}
				// and the rhs another operation.
				if let MPNodeType::Operation(lhs2, op2, rhs2, _) = &rhs.value {
					assert_eq!(op2, "*");
					if let MPNodeType::Integer(val, _) = &lhs2.value {
						assert_eq!(val, "456");
					} else {
						assert!(false, "Not an integer object was expecting one.");	
					}
					if let MPNodeType::Integer(val, _) = &rhs2.value {
						assert_eq!(val, "789");
					} else {
						assert!(false, "Not an integer object was expecting one.");	
					}
				}
			} else {
				assert!(false, "Not a operation object was expecting one.");	
			}
		} else {
			assert!(false, "Not a statement object was expecting one.");
		}
	} else {
		assert!(false, "Not a root object was expecting one.");
	}

	let debug: String = result.unwrap().debug_print(test);
	let debug_target = 
"123 - 456 *789
--------------
************** | Root
************** | Statement
************** | Op: -
***            | Int: 123
      ******** | Op: *
      ***      | Int: 456
           *** | Int: 789
";

	assert_eq!(debug, debug_target);
}

#[test]
fn star_insertion_test() {
	let test = String::from("1 2x");
	let mut parser = StackMaximaParser::new_with_insert_stars();

	let result: Option<MPNode> = parser.parse(test.clone());


	let debug: String = result.unwrap().debug_print(test);
	let debug_target = 
"1 2x
----
**** | Root
**** | Statement
**** | Op: * [spaces]
*    | Int: 1
  ** | Op: * [stars]
  *  | Int: 2
   * | ID: x
";

	assert_eq!(debug, debug_target);
}

#[test]
fn keyval_auto_semicolon() {
	let test = String::from("a : rand(5)
b:rand(6)+1;
f(x) := a*x+b");
	let mut parser = StackMaximaParser::new_with_insert_semicolons();

	let result: Option<MPNode> = parser.parse(test.clone());

	if let MPNodeType::Root(statements,_,_) = result.clone().expect("Should have a result from parsing.").value {
		assert_eq!(statements.len(), 3);
	} else {
		assert!(false, "Not a root object was expecting one.");
	}
}


#[test]
fn keyval_auto_semicolon_eval_flags() {
	let test = String::from("a : rand(5),simp
b:rand(6)+1;
f(x) := a*x+b,simp=false
g(x) := 1+y,y=sqrt(x)");
	let mut parser = StackMaximaParser::new_with_insert_semicolons();

	let result: Option<MPNode> = parser.parse(test.clone());

	if let MPNodeType::Root(statements,_,_) = result.clone().expect("Should have a result from parsing.").value {
		assert_eq!(statements.len(), 4);
		assert_eq!(statements[0].to_code(), "a : rand(5),simp");
		assert_eq!(statements[1].to_code(), "b : rand(6) + 1");
		assert_eq!(statements[2].to_code(), "f(x) := a * x + b,simp=false");
		assert_eq!(statements[3].to_code(), "g(x) := 1 + y,y=sqrt(x)");
	} else {
		assert!(false, "Not a root object was expecting one.");
	}
}

#[test]
fn if_statement() {
	let test = String::from("if foo then bar");
	let mut parser = StackMaximaParser::new_no_insertions();

	let result: Option<MPNode> = parser.parse(test.clone());


	let debug: String = result.unwrap().debug_print(test);
	let debug_target = 
"if foo then bar
---------------
*************** | Root
*************** | Statement
*************** | If: 1 branches
   ***          | ID: foo
            *** | ID: bar
";

	assert_eq!(debug, debug_target);
}

#[test]
fn if_statement_else() {
	let test = String::from("if (foo > 0) then 1 else 2");
	let mut parser = StackMaximaParser::new_no_insertions();

	let result: Option<MPNode> = parser.parse(test.clone());


	let debug: String = result.unwrap().debug_print(test);
	let debug_target = 
"if (foo > 0) then 1 else 2
--------------------------
************************** | Root
************************** | Statement
************************** | If: 2 branches
   *********               | Group: 1 items
    *******                | Op: >
    ***                    | ID: foo
          *                | Int: 0
                  *        | Int: 1
                         * | Int: 2
";

	assert_eq!(debug, debug_target);
}

#[test]
fn if_statement_elseif() {
	let test = String::from("if a then b elseif b<=2 then c else d");
	let mut parser = StackMaximaParser::new_no_insertions();

	let result: Option<MPNode> = parser.parse(test.clone());


	let debug: String = result.unwrap().debug_print(test);
	let debug_target = 
"if a then b elseif b<=2 then c else d
-------------------------------------
************************************* | Root
************************************* | Statement
************************************* | If: 3 branches
   *                                  | ID: a
          *                           | ID: b
                   ****               | Op: <=
                   *                  | ID: b
                      *               | Int: 2
                             *        | ID: c
                                    * | ID: d
";

	assert_eq!(debug, debug_target);
}

#[test]
fn loop_statement() {
	let test = String::from("for i: 1 thru 5 do x:x+1");
	let mut parser = StackMaximaParser::new_no_insertions();

	let result: Option<MPNode> = parser.parse(test.clone());


	let debug: String = result.unwrap().debug_print(test);
	let debug_target = 
"for i: 1 thru 5 do x:x+1
------------------------
************************ | Root
************************ | Statement
************************ | Loop
********                 | LoopBit: for
    ****                 | Op: :
    *                    | ID: i
       *                 | Int: 1
         ******          | LoopBit: thru
              *          | Int: 5
                   ***** | Op: :
                   *     | ID: x
                     *** | Op: +
                     *   | ID: x
                       * | Int: 1
";

	assert_eq!(debug, debug_target);
}


#[test]
fn precedence0() {
	let test = String::from("1*2*3*4");
	let mut parser = StackMaximaParser::new_no_insertions();

	let result: Option<MPNode> = parser.parse(test.clone());


	let debug: String = result.unwrap().debug_print(test);
	let debug_target = 
"1*2*3*4
-------
******* | Root
******* | Statement
******* | Op: *
*       | Int: 1
  ***** | Op: *
  *     | Int: 2
    *** | Op: *
    *   | Int: 3
      * | Int: 4
";
	assert_eq!(debug, debug_target);
}

#[test]
fn precedence1() {
	let test = String::from("1*2+3*4");
	let mut parser = StackMaximaParser::new_no_insertions();

	let result: Option<MPNode> = parser.parse(test.clone());


	let debug: String = result.unwrap().debug_print(test);
	let debug_target = 
"1*2+3*4
-------
******* | Root
******* | Statement
******* | Op: +
***     | Op: *
*       | Int: 1
  *     | Int: 2
    *** | Op: *
    *   | Int: 3
      * | Int: 4
";
	assert_eq!(debug, debug_target);
}


#[test]
fn precedence2() {
	let test = String::from("x+2*-x/2");
	let mut parser = StackMaximaParser::new_no_insertions();

	let result: Option<MPNode> = parser.parse(test.clone());


	let debug: String = result.unwrap().debug_print(test);
	let debug_target = 
"x+2*-x/2
--------
******** | Root
******** | Statement
******** | Op: +
*        | ID: x
  ****** | Op: *
  *      | Int: 2
    **** | PrefixOp: -
     *** | Op: /
     *   | ID: x
       * | Int: 2
";
	assert_eq!(debug, debug_target);
}

#[test]
fn precedence3() {
	let test = String::from("1-2-3-4");
	let mut parser = StackMaximaParser::new_no_insertions();

	let result: Option<MPNode> = parser.parse(test.clone());


	let debug: String = result.unwrap().debug_print(test);
	let debug_target = 
"1-2-3-4
-------
******* | Root
******* | Statement
******* | Op: -
*****   | Op: -
***     | Op: -
*       | Int: 1
  *     | Int: 2
    *   | Int: 3
      * | Int: 4
";
	assert_eq!(debug, debug_target);
}

#[test]
fn precedence4() {
	let test = String::from("1^2^3^4");
	let mut parser = StackMaximaParser::new_no_insertions();

	let result: Option<MPNode> = parser.parse(test.clone());


	let debug: String = result.unwrap().debug_print(test);
	let debug_target = 
"1^2^3^4
-------
******* | Root
******* | Statement
******* | Op: ^
*       | Int: 1
  ***** | Op: ^
  *     | Int: 2
    *** | Op: ^
    *   | Int: 3
      * | Int: 4
";
	assert_eq!(debug, debug_target);
}

#[test]
fn precedence5() {
	let test = String::from("1*2<3*4");
	let mut parser = StackMaximaParser::new_no_insertions();

	let result: Option<MPNode> = parser.parse(test.clone());


	let debug: String = result.unwrap().debug_print(test);
	let debug_target = 
"1*2<3*4
-------
******* | Root
******* | Statement
******* | Op: <
***     | Op: *
*       | Int: 1
  *     | Int: 2
    *** | Op: *
    *   | Int: 3
      * | Int: 4
";
	assert_eq!(debug, debug_target);
}

#[test]
fn precedence6() {
	let test = String::from("1 nounadd 2 + 3 nounadd 4");
	let mut parser = StackMaximaParser::new_no_insertions();

	let result: Option<MPNode> = parser.parse(test.clone());


	let debug: String = result.unwrap().debug_print(test);
	let debug_target = 
"1 nounadd 2 + 3 nounadd 4
-------------------------
************************* | Root
************************* | Statement
************************* | Op: nounadd
*                         | Int: 1
          *************** | Op: +
          *               | Int: 2
              *********** | Op: nounadd
              *           | Int: 3
                        * | Int: 4
";
	assert_eq!(debug, debug_target);
}

#[test]
fn precedence7() {
	let test = String::from("a and b or c");
	let mut parser = StackMaximaParser::new_no_insertions();

	let result: Option<MPNode> = parser.parse(test.clone());


	let debug: String = result.unwrap().debug_print(test);
	let debug_target = 
"a and b or c
------------
************ | Root
************ | Statement
************ | Op: or
*******      | Op: and
*            | ID: a
      *      | ID: b
           * | ID: c
";
	assert_eq!(debug, debug_target);
}




#[test]
fn callnesting() {
	let test = String::from("[f,g][rand([1,2])](x)");
	let mut parser = StackMaximaParser::new_no_insertions();

	let result: Option<MPNode> = parser.parse(test.clone());


	let debug: String = result.unwrap().debug_print(test);
	let debug_target = 
"[f,g][rand([1,2])](x)
---------------------
********************* | Root
********************* | Statement
********************* | FunctionCall: 1 args
******************    | Indexing
*****                 | List: 2 items
 *                    | ID: f
   *                  | ID: g
      ***********     | FunctionCall: 1 args
      ****            | ID: rand
           *****      | List: 2 items
            *         | Int: 1
              *       | Int: 2
                   *  | ID: x
";
	assert_eq!(debug, debug_target);
}


#[test]
fn comment_extraction() {
	let test = String::from("
/* Here be a function ␚⩬⬄ⷨ♯▷ⲕ */
f(x) := x^2;/* Extra comment. */
/* And another */
g(x):=block(x^4 /* A bit boring function. */);

/* Those were functions. */
");

	let mut parser = StackMaximaParser::new_no_insertions();
	// By default no comments are extracted.
	parser.collect_comments = true;

	let result: Option<MPNode> = parser.parse(test.clone());

	if let MPNodeType::Root(statements, precomments, internalcomments) = result.clone().expect("Should have a result from parsing.").value {
		assert_eq!(statements.len(), 2);
		assert_eq!(precomments[0].len(), 1);
		assert_eq!(precomments[1].len(), 2);
		assert_eq!(internalcomments[1].len(), 1);
		// The post comment is before a non existent third statement.
		assert_eq!(precomments[2].len(), 1);
		// Some extra unicode.
		if let MPNodeType::Comment(content) = &precomments[0][0].value {
			assert_eq!(content, " Here be a function ␚⩬⬄ⷨ♯▷ⲕ ");
		} else {
			assert!(false, "Expected a comment type of a node.");	
		}
		// Check order of comments.
		if let MPNodeType::Comment(content) = &precomments[1][0].value {
			assert_eq!(content, " Extra comment. ");
		} else {
			assert!(false, "Expected a comment type of a node.");	
		}
		if let MPNodeType::Comment(content) = &precomments[1][1].value {
			assert_eq!(content, " And another ");
		} else {
			assert!(false, "Expected a comment type of a node.");	
		}
		if let MPNodeType::Comment(content) = &internalcomments[1][0].value {
			assert_eq!(content, " A bit boring function. ");
		} else {
			assert!(false, "Expected a comment type of a node.");	
		}
		if let MPNodeType::Comment(content) = &precomments[2][0].value {
			assert_eq!(content, " Those were functions. ");
		} else {
			assert!(false, "Expected a comment type of a node.");	
		}

		// Check positioning. If we slice out all the precomments we should get this
		let target = String::from("

f(x) := x^2;

g(x):=block(x^4 /* A bit boring function. */);


");
		let mut sliced_test: String = String::new();
		sliced_test.push_str(&test[0..precomments[0][0].position.startbyte]);
		sliced_test.push_str(&test[precomments[0][0].position.endbyte..precomments[1][0].position.startbyte]);
		sliced_test.push_str(&test[precomments[1][0].position.endbyte..precomments[1][1].position.startbyte]);
		sliced_test.push_str(&test[precomments[1][1].position.endbyte..precomments[2][0].position.startbyte]);
		sliced_test.push_str(&test[precomments[2][0].position.endbyte..test.len()]);

		assert_eq!(sliced_test, target);
	} else {
		assert!(false, "Not a root object was expecting one.");
	}
}


#[test]
fn utility_extract_interesting_strings() {
	let test = String::from("feedback(x) := castext(\"Something about {@x@}.\");
stack_include(\"https://somewhere.out.there/lib.mac\");
stack_include_contrib(\"validators.mac\");
other: [\"random string ⠴⺗ⶮ⎟⍞⟝\"];
");

	let mut parser = StackMaximaParser::new_no_insertions();

	let result: MPNode = parser.parse(test.clone()).expect("Should parse just fine.");

	// This function is a bit odd as it requires definition of the first guess on type.
	let strings = result.extract_stack_string_usage(StackStringUsage::Unknown);

	assert_eq!(strings.len(), 4);

	// Check the minimal context information.
	assert_eq!(strings[0].0, StackStringUsage::CASText);
	assert_eq!(strings[1].0, StackStringUsage::Include);
	assert_eq!(strings[2].0, StackStringUsage::IncludeContrib);
	assert_eq!(strings[3].0, StackStringUsage::Unknown);

	// Then the values. The nodes do also have positions...
	if let MPNodeType::String(content) = &strings[0].1.value {
		assert_eq!(content, "Something about {@x@}.");
	} else {
		assert!(false, "Expected a string type of a node.");	
	}
	if let MPNodeType::String(content) = &strings[1].1.value {
		assert_eq!(content, "https://somewhere.out.there/lib.mac");
	} else {
		assert!(false, "Expected a string type of a node.");	
	}
	if let MPNodeType::String(content) = &strings[2].1.value {
		assert_eq!(content, "validators.mac");
	} else {
		assert!(false, "Expected a string type of a node.");	
	}
	if let MPNodeType::String(content) = &strings[3].1.value {
		assert_eq!(content, "random string ⠴⺗ⶮ⎟⍞⟝");
	} else {
		assert!(false, "Expected a string type of a node.");	
	}
}