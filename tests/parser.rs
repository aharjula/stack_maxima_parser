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
**** | Op: * [stars]
***  | Op: * [spaces]
*    | Int: 1
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
*****   | Op: *
***     | Op: *
*       | Int: 1
  *     | Int: 2
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
  ****** | Op: /
  ****   | Op: *
  *      | Int: 2
    **   | PrefixOp: -
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
***************           | Op: +
***********               | Op: nounadd
*                         | Int: 1
          *               | Int: 2
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
fn precedence8() {
	let test = String::from("a or b and c");
	let mut parser = StackMaximaParser::new_no_insertions();

	let result: Option<MPNode> = parser.parse(test.clone());


	let debug: String = result.unwrap().debug_print(test);
	let debug_target = 
"a or b and c
------------
************ | Root
************ | Statement
************ | Op: or
*            | ID: a
     ******* | Op: and
     *       | ID: b
           * | ID: c
";
	assert_eq!(debug, debug_target);
}


#[test]
fn precedence9() {
	let test = String::from("a:tan^-1*(x)-1");
	let mut parser = StackMaximaParser::new_with_insert_stars();

	let result: Option<MPNode> = parser.parse(test.clone());


	let debug: String = result.unwrap().debug_print(test);
	let debug_target = 
"a:tan^-1*(x)-1
--------------
************** | Root
************** | Statement
************** | Op: :
*              | ID: a
  ************ | Op: -
  **********   | Op: *
  ******       | Op: ^
  ***          | ID: tan
      **       | PrefixOp: -
       *       | Int: 1
         ***   | Group: 1 items
          *    | ID: x
             * | Int: 1
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
	let test = String::from("feedback(x) := castext_concat(castext(\"Something about {@x@}.\"),\" concat of strings is possible\");
stack_include(\"https://somewhere.out.there/lib.mac\");
stack_include_contrib(\"validators.mac\");
other: [0,true,\"random string ⠴⺗ⶮ⎟⍞⟝\",foo(\"not directly in list\"),[1],[2],[[0,1,\"1\"]]];
compiled: [\"%root\",[\"%cs\",\"a common string\"]]
");

	let mut parser = StackMaximaParser::new_no_insertions();

	let result: MPNode = parser.parse(test.clone()).expect("Should parse just fine.");

	// This function is a bit odd as it requires definition of the first guess on type.
	let strings = result.extract_stack_string_usage(StackStringUsage::Unknown);

	assert_eq!(strings.len(), 10);

	// Check the minimal context information.
	assert_eq!(strings[0].0, StackStringUsage::CASText);
	assert_eq!(strings[1].0, StackStringUsage::CASTextConcat);
	assert_eq!(strings[2].0, StackStringUsage::Include);
	assert_eq!(strings[3].0, StackStringUsage::IncludeContrib);
	assert_eq!(strings[4].0, StackStringUsage::ListElement(2));
	assert_eq!(strings[5].0, StackStringUsage::Unknown);
	assert_eq!(strings[6].0, StackStringUsage::ListElement(2));
	assert_eq!(strings[7].0, StackStringUsage::CompiledCASText(0));
	assert_eq!(strings[8].0, StackStringUsage::CompiledCASText(0));
	assert_eq!(strings[9].0, StackStringUsage::CompiledCASText(1));

	// Then the values. The nodes do also have positions...
	if let MPNodeType::String(content) = &strings[0].1.value {
		assert_eq!(content, "Something about {@x@}.");
	} else {
		assert!(false, "Expected a string type of a node.");	
	}
	if let MPNodeType::String(content) = &strings[1].1.value {
		assert_eq!(content, " concat of strings is possible");
	} else {
		assert!(false, "Expected a string type of a node.");	
	}
	if let MPNodeType::String(content) = &strings[2].1.value {
		assert_eq!(content, "https://somewhere.out.there/lib.mac");
	} else {
		assert!(false, "Expected a string type of a node.");	
	}
	if let MPNodeType::String(content) = &strings[3].1.value {
		assert_eq!(content, "validators.mac");
	} else {
		assert!(false, "Expected a string type of a node.");	
	}
	if let MPNodeType::String(content) = &strings[4].1.value {
		assert_eq!(content, "random string ⠴⺗ⶮ⎟⍞⟝");
	} else {
		assert!(false, "Expected a string type of a node.");	
	}
	if let MPNodeType::String(content) = &strings[5].1.value {
		assert_eq!(content, "not directly in list");
	} else {
		assert!(false, "Expected a string type of a node.");	
	}
	if let MPNodeType::String(content) = &strings[6].1.value {
		assert_eq!(content, "1");
	} else {
		assert!(false, "Expected a string type of a node.");	
	}
	if let MPNodeType::String(content) = &strings[7].1.value {
		assert_eq!(content, "%root");
	} else {
		assert!(false, "Expected a string type of a node.");	
	}
	if let MPNodeType::String(content) = &strings[8].1.value {
		assert_eq!(content, "%cs");
	} else {
		assert!(false, "Expected a string type of a node.");	
	}
	if let MPNodeType::String(content) = &strings[9].1.value {
		assert_eq!(content, "a common string");
	} else {
		assert!(false, "Expected a string type of a node.");	
	}
}

#[test]
fn empty_document_empty_string() {
	let test = String::from("");
	let mut parser = StackMaximaParser::new_no_insertions();

	let result: Option<MPNode> = parser.parse(test.clone());

	if let MPNodeType::Root(statements, _, _) = result.clone().expect("Should have a result from parsing.").value {
		assert_eq!(statements.len(), 0);
	}
}

#[test]
fn empty_document_whitespace() {
	let test = String::from("   	");
	let mut parser = StackMaximaParser::new_no_insertions();

	let result: Option<MPNode> = parser.parse(test.clone());

	if let MPNodeType::Root(statements, _, _) = result.clone().expect("Should have a result from parsing.").value {
		assert_eq!(statements.len(), 0);
	}
}

#[test]
fn empty_document_comment() {
	let test = String::from("/* comment */");
	let mut parser = StackMaximaParser::new_no_insertions();

	let result: Option<MPNode> = parser.parse(test.clone());

	if let MPNodeType::Root(statements, _, _) = result.clone().expect("Should have a result from parsing.").value {
		assert_eq!(statements.len(), 0);
	}
}

#[test]
fn keywords_as_ids() {
	let test = String::from("subst(%not, \"not\", ex)");
	let mut parser = StackMaximaParser::new_no_insertions();

	let result: Option<MPNode> = parser.parse(test.clone());

	let debug: String = result.unwrap().debug_print(test);
	let debug_target = 
"subst(%not, \"not\", ex)
----------------------
********************** | Root
********************** | Statement
********************** | FunctionCall: 3 args
*****                  | ID: subst
      ****             | ID: %not
            *****      | String
                   **  | ID: ex
";
	assert_eq!(debug, debug_target);
}



#[test]
fn try_almost_everything() {
	// Just check that the lexer has the tokens and the rules exist.
	// Try to remember to add any new operators here.
	let test = String::from("
		foo(x,[1/(1.2-y),%pi],{})@bar,simp=false;
		if a then b else return(e),simp:true;
		t::= a[d](y);
		t:= if (is(tru)) then t elseif is(x <= y) then f else maybe;
		t: for x in ''y from 'z next foo thru bar while +%Z unless not -%X do something;
		t : is(z>y)! nounor ?is (z<=y*u);
		t ~ |y|; subst(%not,b,y);
");

	let mut parser = StackMaximaParser::new_no_insertions();

	let _result: MPNode = parser.parse(test.clone()).expect("Should parse just fine.");
}


#[test]
fn assigns_precedence() {
	let test = String::from("c1:f(x):=x^3");
	let mut parser = StackMaximaParser::new_no_insertions();

	let result: Option<MPNode> = parser.parse(test.clone());

	let debug: String = result.unwrap().debug_print(test);
	let debug_target = 
"c1:f(x):=x^3
------------
************ | Root
************ | Statement
************ | Op: :
**           | ID: c1
   ********* | Op: :=
   ****      | FunctionCall: 1 args
   *         | ID: f
     *       | ID: x
         *** | Op: ^
         *   | ID: x
           * | Int: 3
";
	assert_eq!(debug, debug_target);
}

#[test]
fn stack_1882() {
	let test = String::from("while not gcd(p,q) = 1 do p:p+1;");
  let mut parser = StackMaximaParser::new_no_insertions();

	let result: Option<MPNode> = parser.parse(test.clone());

	let debug: String = result.unwrap().debug_print(test);

  let debug_target = 
"while not gcd(p,q) = 1 do p:p+1;
--------------------------------
*******************************  | Root
*******************************  | Statement
*******************************  | Loop
**********************           | LoopBit: while
      ****************           | PrefixOp: not 
          ************           | Op: =
          ********               | FunctionCall: 2 args
          ***                    | ID: gcd
              *                  | ID: p
                *                | ID: q
                     *           | Int: 1
                          *****  | Op: :
                          *      | ID: p
                            ***  | Op: +
                            *    | ID: p
                              *  | Int: 1
";
  assert_eq!(debug, debug_target);
}

#[test]
fn sub_add_div_mul() {
	let test = String::from("[a-b+c,-a+b,a/b*c];");
  let mut parser = StackMaximaParser::new_no_insertions();

	let result: Option<MPNode> = parser.parse(test.clone());

	let debug: String = result.unwrap().debug_print(test);

  let debug_target = 
"[a-b+c,-a+b,a/b*c];
-------------------
******************  | Root
******************  | Statement
******************  | List: 3 items
 *****              | Op: +
 ***                | Op: -
 *                  | ID: a
   *                | ID: b
     *              | ID: c
       ****         | Op: +
       **           | PrefixOp: -
        *           | ID: a
          *         | ID: b
            *****   | Op: *
            ***     | Op: /
            *       | ID: a
              *     | ID: b
                *   | ID: c
";
  assert_eq!(debug, debug_target);
}

#[test]
fn log_and_rel() {
	let test = String::from("[1=x and x=1 and x=x,not x<y];");
  let mut parser = StackMaximaParser::new_no_insertions();

	let result: Option<MPNode> = parser.parse(test.clone());

	let debug: String = result.unwrap().debug_print(test);

  let debug_target = 
"[1=x and x=1 and x=x,not x<y];
------------------------------
*****************************  | Root
*****************************  | Statement
*****************************  | List: 2 items
 *******************           | Op: and
 ***********                   | Op: and
 ***                           | Op: =
 *                             | Int: 1
   *                           | ID: x
         ***                   | Op: =
         *                     | ID: x
           *                   | Int: 1
                 ***           | Op: =
                 *             | ID: x
                   *           | ID: x
                     *******   | PrefixOp: not 
                         ***   | Op: <
                         *     | ID: x
                           *   | ID: y
";
  assert_eq!(debug, debug_target);
}
#[test]
fn arith_before_logic() {
	let test = String::from("[a+2 and b,a/b or c,x^2 and -y,m<=1/2 and m>-2];");
	let mut parser = StackMaximaParser::new_no_insertions();

	let result: Option<MPNode> = parser.parse(test.clone());

	let debug: String = result.unwrap().debug_print(test);

	let debug_target = 
"[a+2 and b,a/b or c,x^2 and -y,m<=1/2 and m>-2];
------------------------------------------------
***********************************************  | Root
***********************************************  | Statement
***********************************************  | List: 4 items
 *********                                       | Op: and
 ***                                             | Op: +
 *                                               | ID: a
   *                                             | Int: 2
         *                                       | ID: b
           ********                              | Op: or
           ***                                   | Op: /
           *                                     | ID: a
             *                                   | ID: b
                  *                              | ID: c
                    **********                   | Op: and
                    ***                          | Op: ^
                    *                            | ID: x
                      *                          | Int: 2
                            **                   | PrefixOp: -
                             *                   | ID: y
                               ***************   | Op: and
                               ******            | Op: <=
                               *                 | ID: m
                                  ***            | Op: /
                                  *              | Int: 1
                                    *            | Int: 2
                                          ****   | Op: >
                                          *      | ID: m
                                            **   | PrefixOp: -
                                             *   | Int: 2
";
	assert_eq!(debug, debug_target);
}

#[test]
fn logic_after_prefix_and_rel() {
	let test = String::from("[-a and b,a.b or c,x^-1 and y,a and b+c,not a+1 and b];");
	let mut parser = StackMaximaParser::new_no_insertions();

	let result: Option<MPNode> = parser.parse(test.clone());

	let debug: String = result.unwrap().debug_print(test);

	let debug_target = 
"[-a and b,a.b or c,x^-1 and y,a and b+c,not a+1 and b];
-------------------------------------------------------
******************************************************  | Root
******************************************************  | Statement
******************************************************  | List: 5 items
 ********                                               | Op: and
 **                                                     | PrefixOp: -
  *                                                     | ID: a
        *                                               | ID: b
          ********                                      | Op: or
          ***                                           | Op: .
          *                                             | ID: a
            *                                           | ID: b
                 *                                      | ID: c
                   **********                           | Op: and
                   ****                                 | Op: ^
                   *                                    | ID: x
                     **                                 | PrefixOp: -
                      *                                 | Int: 1
                            *                           | ID: y
                              *********                 | Op: and
                              *                         | ID: a
                                    ***                 | Op: +
                                    *                   | ID: b
                                      *                 | ID: c
                                        *************   | Op: and
                                        *******         | PrefixOp: not 
                                            ***         | Op: +
                                            *           | ID: a
                                              *         | Int: 1
                                                    *   | ID: b
";
	assert_eq!(debug, debug_target);
}

#[test]
fn keyword_logic_ops() {
	let test = String::from("[a xor b,a nand b,a nor b,a xnor b,a+1 xor b,a=b xor c,a and b xor c];");
	let mut parser = StackMaximaParser::new_no_insertions();

	let result: Option<MPNode> = parser.parse(test.clone());

	let debug: String = result.unwrap().debug_print(test);

	let debug_target = 
"[a xor b,a nand b,a nor b,a xnor b,a+1 xor b,a=b xor c,a and b xor c];
----------------------------------------------------------------------
*********************************************************************  | Root
*********************************************************************  | Statement
*********************************************************************  | List: 7 items
 *******                                                               | Op: xor
 *                                                                     | ID: a
       *                                                               | ID: b
         ********                                                      | Op: nand
         *                                                             | ID: a
                *                                                      | ID: b
                  *******                                              | Op: nor
                  *                                                    | ID: a
                        *                                              | ID: b
                          ********                                     | Op: xnor
                          *                                            | ID: a
                                 *                                     | ID: b
                                   *********                           | Op: xor
                                   ***                                 | Op: +
                                   *                                   | ID: a
                                     *                                 | Int: 1
                                           *                           | ID: b
                                             *********                 | Op: xor
                                             ***                       | Op: =
                                             *                         | ID: a
                                               *                       | ID: b
                                                     *                 | ID: c
                                                       *************   | Op: xor
                                                       *******         | Op: and
                                                       *               | ID: a
                                                             *         | ID: b
                                                                   *   | ID: c
";
	assert_eq!(debug, debug_target);
}
