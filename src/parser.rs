//! Parser with some object-definitions and utility functions for those objects.
//!

// WARNING parts of this file comes from a code-generator, don't edit here.
use crate::lexer::{StackMaximaLexer, StackMaximaToken, StackMaximaParserTokenType};
use crate::parsertables::PARSERTABLES as parsertables;
use crate::parsertables::TableAction;

/// Details about a given multiplication-operation. 
/// Basically, was the '*'-added due to some reason?
#[derive(Clone, Debug)]
pub enum InsertionType {
	None,
	FixSpaces,
	InsertStars
}

/// Types of nodes in the parse-tree.
#[derive(Clone, Debug)]
pub enum MPNodeType {
	Integer(String, String), // Converted and original values
	Float(String, String), // Converted and original values
	Boolean(bool, String), // Converted and original values
	String(String),
	Identifier(String),
	PrefixOperation(String, Box<MPNode>),
	Operation(Box<MPNode>, String, Box<MPNode>, InsertionType), // Some times these are insertions in those cases the type matters.
	PostfixOperation(Box<MPNode>, String),
	Comment(String),
	FunctionCall(Box<MPNode>, Vec<MPNode>), // name and arguments
	Group(Vec<MPNode>),
	Set(Vec<MPNode>),
	List(Vec<MPNode>),
	Indexing(Box<MPNode>, Vec<MPNode>),
	If(Vec<MPNode>, Vec<MPNode>), // Conditions and branches
	Loop(Box<MPNode>, Vec<MPNode>), // Body and conditions
	LoopBit(String, Box<MPNode>), // Type and value
	EvaluationFlag(Box<MPNode>, Option<Box<MPNode>>), // Name and possibly a value
	Statement(Box<MPNode>, Vec<MPNode>), // Possibly many flags
	Root(Vec<MPNode>,Vec<Vec<MPNode>>,Vec<Vec<MPNode>>) // Many statements. 
	// And comments, the first list comments in front of a given statement index, N+1 if comments after statements, the second list for in statement comments.
}

/// All nodes have position data, both directly human readable like line and column details
/// and raw byte references to the source. The human readable ones are inclusive ranges,
/// the byte reference is exclusive so `source[startbyte..endbyte]` will be directly usable.
#[derive(Clone, Debug)]
pub struct MPPosition {
	pub startline: usize, 
	pub startcolumn: usize,
	pub startbyte: usize,
	pub endline: usize,
	pub endcolumn: usize,
	pub endbyte: usize
}
impl MPPosition {
	/// Constructors for positions, from lexer tokens and/or nodes.
	pub fn new_tt(start: &StackMaximaToken, end: &StackMaximaToken) -> MPPosition {
		MPPosition {
			startline: start.startline,
			startcolumn: start.startcolumn,
			startbyte: start.startbyte,
			endline: end.endline,
			endcolumn: end.endcolumn,
			endbyte: end.endbyte
		}
	}
	/// Constructors for positions, from lexer tokens and/or nodes.
	pub fn new_tn(start: &StackMaximaToken, end: &MPNode) -> MPPosition {
		MPPosition {
			startline: start.startline,
			startcolumn: start.startcolumn,
			startbyte: start.startbyte,
			endline: end.position.endline,
			endcolumn: end.position.endcolumn,
			endbyte: end.position.endbyte
		}
	}
	/// Constructors for positions, from lexer tokens and/or nodes.
	pub fn new_nt(start: &MPNode, end: &StackMaximaToken) -> MPPosition {
		MPPosition {
			startline: start.position.startline,
			startcolumn: start.position.startcolumn,
			startbyte: start.position.startbyte,
			endline: end.endline,
			endcolumn: end.endcolumn,
			endbyte: end.endbyte
		}
	}
	/// Constructors for positions, from lexer tokens and/or nodes.
	pub fn new_nn(start: &MPNode, end: &MPNode) -> MPPosition {
		MPPosition {
			startline: start.position.startline,
			startcolumn: start.position.startcolumn,
			startbyte: start.position.startbyte,
			endline: end.position.endline,
			endcolumn: end.position.endcolumn,
			endbyte: end.position.endbyte
		}
	}
	/// Constructors for positions, from lexer tokens and/or nodes.
	pub fn new_t(start: &StackMaximaToken) -> MPPosition {
		MPPosition {
			startline: start.startline,
			startcolumn: start.startcolumn,
			startbyte: start.startbyte,
			endline: start.endline,
			endcolumn: start.endcolumn,
			endbyte: start.endbyte
		}
	}
	/// Some things might not map to a real thing in the source, those will use MAX-values.
	pub fn new_max() -> MPPosition {
		MPPosition {
			startline: usize::MAX,
			startcolumn: usize::MAX,
			startbyte: usize::MAX,
			endline: usize::MAX,
			endcolumn: usize::MAX,
			endbyte: usize::MAX
		}
	}
}

/// Type of string based on its context.
#[derive(Clone, Debug, PartialEq)]
pub enum StackStringUsage {
	/// For the string argument of the `castext` function.
	CASText, 
	/// For a string argument of `castext_concat` function.
	CASTextConcat, 
	/// For any strings inside the nested list structure resulting from CASText compilation. 
	/// The index in the containing list is the only detail provided.
	CompiledCASText(usize),
	/// For `stack_include` addresses.
	Include, 
	/// For `stack_include_contrib` addresses.
	IncludeContrib, 
	/// Direct items of lists. With the index in that list.
	ListElement(usize), 
	/// Other uses of strings.
	Unknown
}


/// Nodes in the parse-tree.
#[derive(Clone, Debug)]
pub struct MPNode {
	pub position: MPPosition,
	pub value: MPNodeType
}
impl MPNode {
	pub fn new(position: MPPosition, value: MPNodeType) -> MPNode {
		MPNode {
			position,
			value
		}
	}

	/// Cloned list of the direct children of this node.
	pub fn children(&self) -> Vec<MPNode> {
		let mut childs: Vec<MPNode> = Vec::new();
		match &self.value {
			MPNodeType::Integer(_, _) | MPNodeType::Float(_, _) => {}
			MPNodeType::Boolean(_, _) => {}
			MPNodeType::String(_) | MPNodeType::Identifier(_) | MPNodeType::Comment(_) => {}
			MPNodeType::PrefixOperation(_, n) | MPNodeType::PostfixOperation(n, _) | MPNodeType::LoopBit(_, n)=> {
				childs.push(*n.clone());
			}
			MPNodeType::Operation(n0, _, n1, _) => {
				childs.push(*n0.clone());
				childs.push(*n1.clone());
			}
			MPNodeType::Group(ns) | MPNodeType::Set(ns) | MPNodeType::List(ns) | MPNodeType::Root(ns,_,_) => {
				for n in ns {
					childs.push(n.clone());
				}
			}
			MPNodeType::FunctionCall(n0, ns) | MPNodeType::Indexing(n0, ns) | MPNodeType::Statement(n0, ns) => {
				childs.push(*n0.clone());
				for n in ns {
					childs.push(n.clone());
				}
			}
			MPNodeType::Loop(body, ns) => {
				for n in ns {
					childs.push(n.clone());
				}
				childs.push(*body.clone());
			}
			MPNodeType::EvaluationFlag(n0, optn) => {
				childs.push(*n0.clone());
				if let Some(n) = optn {
					childs.push(*n.clone());
				}
			}
			MPNodeType::If(conds, branches) => {
				for i in 0..conds.len() {
					childs.push(conds[i].clone());
					childs.push(branches[i].clone());
				}
				// Then the else branch.
				if branches.len() > conds.len() {
					childs.push(branches.last().unwrap().clone());
				}
			}
		}
		childs
	}

	/// Cloned list of all the nodes in this subtree, including this node. Very much not efficient.
	pub fn all_nodes_in_tree(&self) -> Vec<MPNode> {
		let mut result: Vec<MPNode> = vec![self.clone()];
		for child in self.children() {
			for i in child.all_nodes_in_tree() {
				result.push(i);
			}
		}
		result
	}

	/// Short type information for a node, used in debug messages.
	pub fn type_short(&self) -> String {
		match &self.value {
			MPNodeType::Integer(v, _) => {format!("Int: {v}")},
			MPNodeType::Float(v, _) => {format!("Float: {v}")},
			MPNodeType::Boolean(v, _) => {format!("Bool: {v}")},
			MPNodeType::String(_) => {"String".to_string()}
			MPNodeType::Identifier(v) => {format!("ID: {v}")},
			MPNodeType::PrefixOperation(v, _) => {format!("PrefixOp: {v}")},
			MPNodeType::Operation(_, v, _, i) => {
				match i {
					InsertionType::None => {format!("Op: {v}")}
					InsertionType::FixSpaces => {format!("Op: {v} [spaces]")}
					InsertionType::InsertStars => {format!("Op: {v} [stars]")}
				}
			},
			MPNodeType::PostfixOperation(_, v) => {format!("PostfixOp: {v}")},
			MPNodeType::Comment(_) => {"Comment".to_string()}
			MPNodeType::FunctionCall(_, a) => {format!("FunctionCall: {} args", a.len())} 
			MPNodeType::Group(a) => {format!("Group: {} items", a.len())} 
			MPNodeType::Set(a) => {format!("Set: {} items", a.len())} 
			MPNodeType::List(a) => {format!("List: {} items", a.len())} 
			MPNodeType::Indexing(_, _) => {"Indexing".to_string()}
			MPNodeType::If(_, a) => {format!("If: {} branches", a.len())}
			MPNodeType::Loop(_, _) => {"Loop".to_string()}
			MPNodeType::LoopBit(v, _) => {format!("LoopBit: {v}")},
			MPNodeType::EvaluationFlag(_, _) => {"EvFlag".to_string()}
			MPNodeType::Statement(_, _) => {"Statement".to_string()}
			MPNodeType::Root(_,_,_) => {"Root".to_string()}
		}
	}

	/// Builds an ASCII-art representation of a parse-tree, below the original source.
	pub fn debug_print(&self, orig: String) -> String {
		let origlen = orig.chars().count();
		let mut output: String = String::new();
		output.push_str(orig.as_str());
		output.push('\n');
		output.push_str(&"-".repeat(origlen));
		output.push('\n');

		for n in self.all_nodes_in_tree() {
			output.push_str(&" ".repeat(n.position.startcolumn));
			output.push_str(&"*".repeat((n.position.endcolumn as i32 - n.position.startcolumn as i32 + 1).try_into().unwrap()));
			output.push_str(&" ".repeat((origlen as i32 - n.position.endcolumn as i32 - 1).try_into().unwrap()));
			output.push_str(&format!(" | {}", n.type_short()));
			output.push('\n');
		}

		output
	}

	/// Turns the parse tree back to code, uses white-space only to remove ambiguity.
	pub fn to_code(&self) -> String {
		match &self.value {
			MPNodeType::Integer(v, _) | MPNodeType::Identifier(v) | MPNodeType::Float(v, _) => {
				v.clone()
			}
			MPNodeType::Boolean(v, _) => {
				if *v {
					String::from("true")
				} else {
					String::from("false")
				}
			}
			MPNodeType::String(v) => {
				let mut out = String::from("\"");
				out.push_str(&v.clone().replace("\\","\\\\").replace("\"","\\\""));
				out.push('"');
				out
			}
			MPNodeType::Comment(_) => {
				String::from("")
			}
			MPNodeType::PrefixOperation(op, n) | MPNodeType::LoopBit(op, n)=> {
				format!("{op} {}", n.clone().to_code())
			} 
			MPNodeType::PostfixOperation(n, op) => {
				format!("{}{op}", n.clone().to_code())
			}
			MPNodeType::Operation(n0, op, n1, _) => {
				format!("{} {op} {}", n0.clone().to_code(), n1.clone().to_code())
			}
			MPNodeType::Group(ns) => {
				let mut out = String::from("(");
				for n in ns {
					out.push_str(&n.clone().to_code());
					out.push(',');
				}
				out.pop(); // Extra comma.
				out.push(')');
				out
			}
			MPNodeType::Set(ns) => {
				let mut out = String::from("{");
				for n in ns {
					out.push_str(&n.clone().to_code());
					out.push(',');
				}
				out.pop(); // Extra comma.
				out.push('}');
				out
			}
			MPNodeType::List(ns) => {
				let mut out = String::from("[");
				for n in ns {
					out.push_str(&n.clone().to_code());
					out.push(',');
				}
				out.pop(); // Extra comma.
				out.push(']');
				out
			}
			MPNodeType::Root(ns,_,_) => {
				let mut out = String::new();
				for n in ns {
					out.push_str(&n.clone().to_code());
					out.push_str(";\n");
				}
				out
			}
			MPNodeType::FunctionCall(n0, ns) => {
				let mut out = n0.clone().to_code();
				out.push('(');
				for n in ns {
					out.push_str(&n.clone().to_code());
					out.push(',');
				}
				out.pop(); // Extra comma.
				out.push(')');
				out
			}
			MPNodeType::Indexing(n0, ns) => {
				let mut out = n0.clone().to_code();
				out.push('[');
				for n in ns {
					out.push_str(&n.clone().to_code());
					out.push(',');
				}
				out.pop(); // Extra comma.
				out.push(']');
				out
			} 
			MPNodeType::Statement(n0, ns) => {
				let mut out = n0.clone().to_code();
				for n in ns {
					out.push_str(&n.clone().to_code());
				}
				out
			}
			MPNodeType::Loop(body, ns) => {
				let mut out = String::new();
				for n in ns {
					out.push_str(&n.clone().to_code());
					out.push(' ');
				}
				out.push_str(" do ");
				out.push_str(&body.clone().to_code());
				out
			}
			MPNodeType::EvaluationFlag(n0, optn) => {
				let mut out = String::from(",");
				out.push_str(&n0.clone().to_code());
				if let Some(n) = optn {
					out.push('=');
					out.push_str(&n.clone().to_code());
				}
				out
			}
			MPNodeType::If(conds, branches) => {
				let mut out = String::from("if ");
				for i in 0..conds.len() {
					out.push_str(&conds[i].clone().to_code());
					out.push_str(" then ");
					out.push_str(&branches[i].clone().to_code());
				}
				// Then the else branch.
				if branches.len() > conds.len() {
					out.push_str(" else ");
					out.push_str(&branches.last().unwrap().clone().to_code());
				}
				out
			}
		}
	}

	/// Returns all "strings" present in the subtree tagged with some 
	/// information about their context. Note this is a recursive function
	/// and needs the initial context as an argument. Just give 
	/// `StackStringUsage::Unknown` as the argument.
	pub fn extract_stack_string_usage(&self, tag: StackStringUsage) -> Vec<(StackStringUsage, MPNode)> {
		// Reset the tag to unknown for deeper structs, unless we are 
		// inside a compilation result.
		let mut tc = if let StackStringUsage::CompiledCASText(_) = tag {tag.clone()} else {StackStringUsage::Unknown};
		match &self.value {
			MPNodeType::String(_) => {
				vec![(tag.clone(), self.clone())]
			}
			MPNodeType::PostfixOperation(n, _) | MPNodeType::PrefixOperation(_, n) | MPNodeType::LoopBit(_, n) => {
				n.extract_stack_string_usage(tc)
			},
			MPNodeType::Operation(n1, _, n2, _) => {
				let mut result: Vec<(StackStringUsage, MPNode)> = vec![];
				for i in n1.extract_stack_string_usage(tc.clone()) {
					result.push(i.clone());
				}
				for i in n2.extract_stack_string_usage(tc.clone()) {
					result.push(i.clone());
				}
				result
			},
			MPNodeType::Group(ns) | MPNodeType::Set(ns) | MPNodeType::Root(ns,_,_) => {
				let mut result: Vec<(StackStringUsage, MPNode)> = vec![];
				for n in ns {
					for i in n.extract_stack_string_usage(tc.clone()) {
						result.push(i.clone());
					}
				}
				result
			},
			MPNodeType::List(ns) => {
				let mut result: Vec<(StackStringUsage, MPNode)> = vec![];
				// If we happen to identify a compiled CASText style list.
				if ns.len() > 0 {
					if let MPNodeType::String(v) = &ns[0].value {
						if *v == "%root".to_string() {
							tc = StackStringUsage::CompiledCASText(0);
						}
					}
				}
				for (ind,n) in ns.iter().enumerate() {
					if tc != StackStringUsage::Unknown {
						for i in n.extract_stack_string_usage(StackStringUsage::CompiledCASText(ind)) {
							result.push(i.clone());
						}
					} else {
						for i in n.extract_stack_string_usage(StackStringUsage::ListElement(ind)) {
							result.push(i.clone());
						}
					}
				}
				result
			},
			MPNodeType::FunctionCall(name, a) => {
				if name.to_code() == "castext" {
					let mut result: Vec<(StackStringUsage, MPNode)> = vec![];
					for n in a {
						for i in n.extract_stack_string_usage(StackStringUsage::CASText) {
							result.push(i.clone());
						}
					}
					result
				} else if name.to_code() == "castext_concat" {
					let mut result: Vec<(StackStringUsage, MPNode)> = vec![];
					for n in a {
						for i in n.extract_stack_string_usage(StackStringUsage::CASTextConcat) {
							result.push(i.clone());
						}
					}
					result
				} else if name.to_code() == "stack_include" {
					let mut result: Vec<(StackStringUsage, MPNode)> = vec![];
					for n in a {
						for i in n.extract_stack_string_usage(StackStringUsage::Include) {
							result.push(i.clone());
						}
					}
					result
				} else if name.to_code() == "stack_include_contrib" {
					let mut result: Vec<(StackStringUsage, MPNode)> = vec![];
					for n in a {
						for i in n.extract_stack_string_usage(StackStringUsage::IncludeContrib) {
							result.push(i.clone());
						}
					}
					result
				} else {
					let mut result: Vec<(StackStringUsage, MPNode)> = vec![];
					for i in name.extract_stack_string_usage(tc.clone()) {
						result.push(i.clone());
					}
					for n in a {
						for i in n.extract_stack_string_usage(tc.clone()) {
							result.push(i.clone());
						}
					}
					result
				}
			} 
			MPNodeType::Indexing(l, ns) => {
				let mut result: Vec<(StackStringUsage, MPNode)> = vec![];
				for i in l.extract_stack_string_usage(tc.clone()) {
					result.push(i.clone());
				}
				for n in ns {
					for i in n.extract_stack_string_usage(tc.clone()) {
						result.push(i.clone());
					}
				}
				result
			}
			MPNodeType::If(conds, branches) => {
				let mut result: Vec<(StackStringUsage, MPNode)> = vec![];
				for i in 0..conds.len() {
					for i in conds[i].extract_stack_string_usage(tc.clone()) {
						result.push(i.clone());
					}
					for i in branches[i].extract_stack_string_usage(tc.clone()) {
						result.push(i.clone());
					}
				}
				// Then the else branch.
				if branches.len() > conds.len() {
					for i in branches.last().unwrap().extract_stack_string_usage(tc.clone()) {
						result.push(i.clone());
					}
				}
				result
			}
			MPNodeType::Loop(body, ns) => {
				let mut result: Vec<(StackStringUsage, MPNode)> = vec![];
				for n in ns {
					for i in n.extract_stack_string_usage(tc.clone()) {
						result.push(i.clone());
					}
				}
				for i in body.extract_stack_string_usage(tc.clone()) {
					result.push(i.clone());
				}
				result
			}
			MPNodeType::Statement(n1, ns) => {
				let mut result: Vec<(StackStringUsage, MPNode)> = vec![];
				for i in n1.extract_stack_string_usage(tc.clone()) {
					result.push(i.clone());
				}
				for n in ns {
					for i in n.extract_stack_string_usage(tc.clone()) {
						result.push(i.clone());
					}
				}
				result
			}
			MPNodeType::EvaluationFlag(n1, optn) => {
				let mut result: Vec<(StackStringUsage, MPNode)> = vec![];
				for i in n1.extract_stack_string_usage(tc.clone()) {
					result.push(i.clone());
				}
				if let Some(n2) = optn {
					for i in n2.extract_stack_string_usage(tc.clone()) {
						result.push(i.clone());
					}
				}
				result
			}
			MPNodeType::Integer(_,_) | MPNodeType::Identifier(_) | MPNodeType::Float(_,_) | MPNodeType::Boolean(_,_) | MPNodeType::Comment(_) => {vec![]}
		}

	}
}


/// Internal logic, for keeping track of the parser-state.
#[derive(Debug)]
enum ParserStackItem {
	Token(StackMaximaToken),
	Node(MPNode),
	List(Vec<MPNode>),
	DualList(Vec<MPNode>,Vec<MPNode>),
	State(usize)
}

/// The actual parser, object.
pub struct StackMaximaParser {
	/// Public access to the lexer so that its settings can be tuned.
	pub lexer: StackMaximaLexer,
	/// At some point errors might be shared though here.
	pub errors: Vec<String>,
	/// Ordered list of tokens to attempt instead of the lexer given if no match is found.
	inserts_to_attempt: Vec<StackMaximaToken>,
	/// Do we care about comments? Collect them?
	pub collect_comments: bool
}
impl StackMaximaParser {
	/// Generates a version of the parser without any insertion logic.
	pub fn new_no_insertions() -> StackMaximaParser {
		StackMaximaParser {
			lexer: StackMaximaLexer::new("init".to_string()),
			errors: Vec::new(),
			inserts_to_attempt: Vec::new(),
			collect_comments: false
		}
	}

	/// Generates a version of the parser that tries to insert missing `*`-chars.
	/// Typical behaviour for student input handling.
	pub fn new_with_insert_stars() -> StackMaximaParser {
		StackMaximaParser {
			lexer: StackMaximaLexer::new("init".to_string()),
			errors: Vec::new(),
			inserts_to_attempt: vec![StackMaximaToken {
				startline: usize::MAX, // Not going to use these positions, will map to the insertion point.
				startcolumn: usize::MAX,
				endline: usize::MAX,
				endcolumn: usize::MAX,
				length: -1,
				startbyte: usize::MAX,
				endbyte: usize::MAX,
				value: StackMaximaParserTokenType::SYMBOL("*".to_string())
			}],
			collect_comments: false
		}
	}

	/// Generates a version of the parser with END_TOKEN insertion.
	/// This is the typical behaviour for STACK keyval-fields.
	pub fn new_with_insert_semicolons() -> StackMaximaParser {
		StackMaximaParser {
			lexer: StackMaximaLexer::new("init".to_string()),
			errors: Vec::new(),
			inserts_to_attempt: vec![StackMaximaToken {
				startline: usize::MAX, // Not going to use these positions, will map to the insertion point.
				startcolumn: usize::MAX,
				endline: usize::MAX,
				endcolumn: usize::MAX,
				length: -1,
				startbyte: usize::MAX,
				endbyte: usize::MAX,
				value: StackMaximaParserTokenType::ENDTOKEN(";".to_string())
			}],
			collect_comments: false
		}
	}

	/// Parses a given bit of source.
	pub fn parse(&mut self, source: String) -> Option<MPNode> {
		self.lexer.set_source(source);

		// Some holders of state.
		// We might want to attach these somewhere.
		let mut comments: Vec<MPNode> = Vec::new();
		// Some insertion logic cares on whether whitespace has been seen.
		let mut whitespaceseen: bool = false;

		// For error reports tracking the previous token makes sense.
		// TODO: add error reporting...
		// let mut previous_token: Option<StackMaximaToken> = None;

		// The stack.
		let mut stack: Vec<ParserStackItem> = Vec::new();
		// Initialise to state 0.
		stack.push(ParserStackItem::State(0));
		// Are we currently shifting?
		let mut shifted: bool = true;

		// The raw token and the matching grammar item.
		let mut token: Option<StackMaximaToken> = None;
		let mut sym: String = "err".to_string();
		loop {
			if shifted {
				// previous_token = token;
				token = self.lexer.next_token();
				// First eat whitespace and comments
				while token.is_some() {
					if let StackMaximaParserTokenType::WS(_) = token.clone().unwrap().value {
						whitespaceseen = true;
						token = self.lexer.next_token();
					} else if let StackMaximaParserTokenType::COMMENT(content) = token.clone().unwrap().value {
						if self.collect_comments {
							comments.push(
								MPNode::new(
									MPPosition::new_t(&token.unwrap()),
									MPNodeType::Comment(content)
								)
							);
						}
						token = self.lexer.next_token();
					} else {
						break;
					}
				}
				// Then convert the token to grammar symbol.
				// Is it the end?
				if token.is_none() {
					sym = "\u{0000}".to_string();
				} else {
					match token.clone().unwrap().value {
    					StackMaximaParserTokenType::KEYWORD(content,_) => {
    						sym = content;
    					},
    					StackMaximaParserTokenType::ID(_) => {
    						sym = "ID".to_string();
    					},
    					StackMaximaParserTokenType::INT(_,_) => {
    						sym = "INT".to_string();
    					},
    					StackMaximaParserTokenType::FLOAT(_,_) => {
    						sym = "FLOAT".to_string();
    					},
    					StackMaximaParserTokenType::BOOL(_,_) => {
    						sym = "BOOL".to_string();
    					},
    					StackMaximaParserTokenType::STRING(_) => {
    						sym = "STRING".to_string();
    					},
    					StackMaximaParserTokenType::SYMBOL(content) => {
    						sym = content;
    					},
    					StackMaximaParserTokenType::LISTSEP(_) => {
    						sym = "LIST_SEP".to_string();
    					},
    					StackMaximaParserTokenType::ENDTOKEN(_) => {
    						sym = "END_TOKEN".to_string();
    					},
    					StackMaximaParserTokenType::LISPID(_) => {
    						sym = "LISP_ID".to_string();
    					},
						StackMaximaParserTokenType::COMMENT(_) => {
							panic!("No comments should appear here.");
						},
						StackMaximaParserTokenType::WS(_) => {
							panic!("No whitspace should appear here.");
						},
						StackMaximaParserTokenType::ERROR(e) => {
							panic!("A lexer error: {e}");
						}
					}
				}
				shifted = false;
			}

			// Then peek at the state at the top of the stack.
			let current_state: usize = match stack[stack.len()-1] {
				ParserStackItem::State(state) => {
					state
				}
				_ => {
					panic!("The top of the stack did not hold a state.");
				}
			};

			// Do we have an action?
			let mut action: TableAction = parsertables.get_action(current_state, sym.clone());
			
			// If no action we are in a bad state. We could try if an insertion would work.
			if action == TableAction::None {
				for attempt in &self.inserts_to_attempt {
					// Basically three types of things can be attempted. Symbols like '*', the end-token and list-separator.
					if let StackMaximaParserTokenType::SYMBOL(a) = &attempt.value {
						let maybe_action: TableAction = parsertables.get_action(current_state, a.clone());
						if maybe_action != TableAction::None {
							// So some action was found, lets return the token we had back.
							self.lexer.return_token(token.clone().unwrap());
							sym = a.clone();
							let mut itoken = attempt.clone();
							let otoken = token.unwrap();
							// Copy the position to the inserted one.
							// Start and endbytes are the same to signal zero actual length.
							itoken.startline = otoken.startline;
							itoken.startcolumn = otoken.startcolumn;
							itoken.startbyte = otoken.startbyte;
							itoken.endline = otoken.startline;
							itoken.endcolumn = otoken.startcolumn;
							itoken.endbyte = otoken.startbyte;
							// Encode the insertion type using the bytes.
							// If it is insert starts then those are the same.
							// If fix spaces then start > end.
							if whitespaceseen {
								whitespaceseen = false;
								itoken.startbyte += 1;
							}
							token = Some(itoken);
							action = maybe_action;
							break;
						}
					} else if let StackMaximaParserTokenType::ENDTOKEN(_a) = &attempt.value {
						let maybe_action: TableAction = parsertables.get_action(current_state, "END_TOKEN".to_string());
						if maybe_action != TableAction::None {
							self.lexer.return_token(token.clone().unwrap());
							sym = "END_TOKEN".to_string();
							let mut itoken = attempt.clone();
							let otoken = token.unwrap();
							// Copy the position to the inserted one.
							// Start and endbytes are the same to signal zero actual length.
							itoken.startline = otoken.startline;
							itoken.startcolumn = otoken.startcolumn;
							itoken.startbyte = otoken.startbyte;
							itoken.endline = otoken.startline;
							itoken.endcolumn = otoken.startcolumn;
							itoken.endbyte = otoken.startbyte;
							token = Some(itoken);
							action = maybe_action;
							break;
						}
					} else if let StackMaximaParserTokenType::LISTSEP(_a) = &attempt.value {
						let maybe_action: TableAction = parsertables.get_action(current_state, "LIST_SEP".to_string());
						if maybe_action != TableAction::None {
							self.lexer.return_token(token.clone().unwrap());
							sym = "LIST_SEP".to_string();
							let mut itoken = attempt.clone();
							let otoken = token.unwrap();
							// Copy the position to the inserted one.
							// Start and endbytes are the same to signal zero actual length.
							itoken.startline = otoken.startline;
							itoken.startcolumn = otoken.startcolumn;
							itoken.startbyte = otoken.startbyte;
							itoken.endline = otoken.startline;
							itoken.endcolumn = otoken.startcolumn;
							itoken.endbyte = otoken.startbyte;
							token = Some(itoken);
							action = maybe_action;
							break;
						}
					}
				}
				if action == TableAction::None {
					// TODO generate error message
					return None;
				}
			}

			// So now we should have actions that make sense.
			if let TableAction::Shift(target_state) = action {
				// First push in the token
				stack.push(ParserStackItem::Token(token.clone().unwrap()));
				stack.push(ParserStackItem::State(target_state));
				shifted = true;
			} else if let TableAction::Reduce(rule, _count, nt_name, nt_id) = action {
				// So now we reduce.
				let term: ParserStackItem =	match rule {
					38 => {
						let _ = stack.pop();
						let term4dv = if let ParserStackItem::DualList(a,b) = stack.pop().unwrap() {(a,b)} else {panic!("Unexpected item in stack.");};
						let _ = stack.pop();
						let term3n = if let ParserStackItem::Node(a) = stack.pop().unwrap() {a} else {panic!("Unexpected item in stack.");};
						let _ = stack.pop();
						let _ = stack.pop();
						let _ = stack.pop();
						let term1n = if let ParserStackItem::Node(a) = stack.pop().unwrap() {a} else {panic!("Unexpected item in stack.");};
						let _ = stack.pop();
						let _ = stack.pop();
						let mut conds = term4dv.0;
						let mut contents = term4dv.1;
						conds.insert(0, term1n);
						contents.insert(0, term3n);
						ParserStackItem::DualList(conds,contents)
					},
					70..=75 | 77..=101 => {
						let _ = stack.pop();
						let term2n = if let ParserStackItem::Node(a) = stack.pop().unwrap() {a} else {panic!("Unexpected item in stack.");};
						let _ = stack.pop();
						let term1t = if let ParserStackItem::Token(a) = stack.pop().unwrap() {a} else {panic!("Unexpected item in stack.");};
						let _ = stack.pop();
						let term0n = if let ParserStackItem::Node(a) = stack.pop().unwrap() {a} else {panic!("Unexpected item in stack.");};
						if let StackMaximaParserTokenType::SYMBOL(symbol) = term1t.value.clone() {
							ParserStackItem::Node(MPNode::new(MPPosition::new_nn(&term0n, &term2n), MPNodeType::Operation(Box::new(term0n), symbol, Box::new(term2n), InsertionType::None)))
						} else {
							panic!("Unexpected token type.")
						}
					},
					76 => {
						let _ = stack.pop();
						let term2n = if let ParserStackItem::Node(a) = stack.pop().unwrap() {a} else {panic!("Unexpected item in stack.");};
						let _ = stack.pop();
						let term1t = if let ParserStackItem::Token(a) = stack.pop().unwrap() {a} else {panic!("Unexpected item in stack.");};
						let _ = stack.pop();
						let term0n = if let ParserStackItem::Node(a) = stack.pop().unwrap() {a} else {panic!("Unexpected item in stack.");};
						if term1t.endbyte == term1t.startbyte {
							ParserStackItem::Node(MPNode::new(MPPosition::new_nn(&term0n, &term2n), MPNodeType::Operation(Box::new(term0n), "*".to_string(), Box::new(term2n), InsertionType::InsertStars)))
						} else if term1t.endbyte < term1t.startbyte {
							ParserStackItem::Node(MPNode::new(MPPosition::new_nn(&term0n, &term2n), MPNodeType::Operation(Box::new(term0n), "*".to_string(), Box::new(term2n), InsertionType::FixSpaces)))
						} else {
							ParserStackItem::Node(MPNode::new(MPPosition::new_nn(&term0n, &term2n), MPNodeType::Operation(Box::new(term0n), "*".to_string(), Box::new(term2n), InsertionType::None)))
						}
					},
					8..=9 => {
						let _ = stack.pop();
						let mut term4mv = if let ParserStackItem::List(a) = stack.pop().unwrap() {a} else {panic!("Unexpected item in stack.");};
						let _ = stack.pop();
						let term3n = if let ParserStackItem::Node(a) = stack.pop().unwrap() {a} else {panic!("Unexpected item in stack.");};
						let _ = stack.pop();
						let _ = stack.pop();
						let _ = stack.pop();
						let term1t = if let ParserStackItem::Token(a) = stack.pop().unwrap() {a} else {panic!("Unexpected item in stack.");};
						let _ = stack.pop();
						let term0t = if let ParserStackItem::Token(a) = stack.pop().unwrap() {a} else {panic!("Unexpected item in stack.");};
						if let StackMaximaParserTokenType::ID(parsed) = term1t.value.clone() {
							term4mv.insert(0, MPNode::new(MPPosition::new_tn(&term0t, &term3n), MPNodeType::EvaluationFlag(Box::new(MPNode::new(MPPosition::new_t(&term1t), MPNodeType::Identifier(parsed))), Some(Box::new(term3n)))));} else{
							panic!("Unexpected token type.")}
						ParserStackItem::List(term4mv)
					},
					25 => {
						let _ = stack.pop();
						let term1t = if let ParserStackItem::Token(a) = stack.pop().unwrap() {a} else {panic!("Unexpected item in stack.");};
						let _ = stack.pop();
						let term0t = if let ParserStackItem::Token(a) = stack.pop().unwrap() {a} else {panic!("Unexpected item in stack.");};
						if let StackMaximaParserTokenType::LISPID(parsed) = term1t.value.clone() {
							ParserStackItem::Node(MPNode::new(MPPosition::new_tt(&term0t, &term1t), MPNodeType::Identifier(format!("?{parsed}"))))
						} else {
							panic!("Unexpected token type.")
						}
					},
					102 => {
						let _ = stack.pop();
						let term2t = if let ParserStackItem::Token(a) = stack.pop().unwrap() {a} else {panic!("Unexpected item in stack.");};
						let _ = stack.pop();
						let term1n = if let ParserStackItem::Node(a) = stack.pop().unwrap() {a} else {panic!("Unexpected item in stack.");};
						let _ = stack.pop();
						let term0t = if let ParserStackItem::Token(a) = stack.pop().unwrap() {a} else {panic!("Unexpected item in stack.");};
						ParserStackItem::Node(MPNode::new(MPPosition::new_tt(&term0t, &term2t), MPNodeType::FunctionCall(Box::new(MPNode::new(MPPosition::new_max(), MPNodeType::Identifier("abs".to_string()))), vec![term1n])))
					},
					7 => {
						let _ = stack.pop();
						let mut term2mv = if let ParserStackItem::List(a) = stack.pop().unwrap() {a} else {panic!("Unexpected item in stack.");};
						let _ = stack.pop();
						let term1t = if let ParserStackItem::Token(a) = stack.pop().unwrap() {a} else {panic!("Unexpected item in stack.");};
						let _ = stack.pop();
						let term0t = if let ParserStackItem::Token(a) = stack.pop().unwrap() {a} else {panic!("Unexpected item in stack.");};
						if let StackMaximaParserTokenType::ID(parsed) = term1t.value.clone() {
							term2mv.insert(0, MPNode::new(MPPosition::new_tt(&term0t, &term1t), MPNodeType::EvaluationFlag(Box::new(MPNode::new(MPPosition::new_t(&term1t), MPNodeType::Identifier(parsed))), None)));
						} else {
							panic!("Unexpected token type.")}
						ParserStackItem::List(term2mv)
					},
					37 => {
						let _ = stack.pop();
						let term1n = if let ParserStackItem::Node(a) = stack.pop().unwrap() {a} else {panic!("Unexpected item in stack.");};
						let _ = stack.pop();
						let _ = stack.pop();
						ParserStackItem::DualList(Vec::new(),vec![term1n])
					},
					4 => {
						let _ = stack.pop();
						let mut term3mv = if let ParserStackItem::List(a) = stack.pop().unwrap() {a} else {panic!("Unexpected item in stack.");};
						let _ = stack.pop();
						let term2v = if let ParserStackItem::List(a) = stack.pop().unwrap() {a} else {panic!("Unexpected item in stack.");};
						let _ = stack.pop();
						let term1n = if let ParserStackItem::Node(a) = stack.pop().unwrap() {a} else {panic!("Unexpected item in stack.");};
						let _ = stack.pop();
						let _ = stack.pop();
						let mut last = term1n.clone();
						if !term2v.is_empty() {
							last = term2v.last().unwrap().clone();
						}
						term3mv.insert(0, MPNode::new(MPPosition::new_nn(&term1n, &last), MPNodeType::Statement(Box::new(term1n), term2v)));
						ParserStackItem::List(term3mv)
					},
					5 | 10 | 15 | 17 | 31 | 42 => {
						ParserStackItem::List(Vec::new())
					},
					13 => {
						let _ = stack.pop();
						let term2t = if let ParserStackItem::Token(a) = stack.pop().unwrap() {a} else {panic!("Unexpected item in stack.");};
						let _ = stack.pop();
						let term1v = if let ParserStackItem::List(a) = stack.pop().unwrap() {a} else {panic!("Unexpected item in stack.");};
						let _ = stack.pop();
						let term0t = if let ParserStackItem::Token(a) = stack.pop().unwrap() {a} else {panic!("Unexpected item in stack.");};
						ParserStackItem::Node(MPNode::new(MPPosition::new_tt(&term0t, &term2t), MPNodeType::Group(term1v)))
					},
					29 => {
						let _ = stack.pop();
						let mut term1mv = if let ParserStackItem::List(a) = stack.pop().unwrap() {a} else {panic!("Unexpected item in stack.");};
						let _ = stack.pop();
						let term0n = if let ParserStackItem::Node(a) = stack.pop().unwrap() {a} else {panic!("Unexpected item in stack.");};
						let mut result = term0n.clone();
						while !term1mv.is_empty() {
							let node = term1mv.remove(0);
							if let MPNodeType::List(indices) = node.value.clone() {
								result = MPNode::new(MPPosition::new_nn(&term0n, &node), MPNodeType::Indexing(Box::new(result), indices));
							} else if let MPNodeType::Group(args) = node.value.clone() {
								result = MPNode::new(MPPosition::new_nn(&term0n, &node), MPNodeType::FunctionCall(Box::new(result), args));
							} else {
								panic!("Unexpected node type.");
							}
						}
						ParserStackItem::Node(result)
					},
					24 => {
						let _ = stack.pop();
						let term0t = if let ParserStackItem::Token(a) = stack.pop().unwrap() {a} else {panic!("Unexpected item in stack.");};
						if let StackMaximaParserTokenType::ID(parsed) = term0t.value.clone() {
							ParserStackItem::Node(MPNode::new(MPPosition::new_t(&term0t), MPNodeType::Identifier(parsed)))
						} else {
							panic!("Unexpected token type.")
						}
					},
					43..=50 => {
						let _ = stack.pop();
						let term1n = if let ParserStackItem::Node(a) = stack.pop().unwrap() {a} else {panic!("Unexpected item in stack.");};
						let _ = stack.pop();
						let term0t = if let ParserStackItem::Token(a) = stack.pop().unwrap() {a} else {panic!("Unexpected item in stack.");};
						if let StackMaximaParserTokenType::KEYWORD(parsed, _) = term0t.value.clone() {
							ParserStackItem::Node(MPNode::new(MPPosition::new_tn(&term0t, &term1n), MPNodeType::LoopBit(parsed, Box::new(term1n))))
						} else {
							panic!("Unexpected token type.")
						}
					},
					23 => {
						let _ = stack.pop();
						let term0t = if let ParserStackItem::Token(a) = stack.pop().unwrap() {a} else {panic!("Unexpected item in stack.");};
						if let StackMaximaParserTokenType::STRING(parsed) = term0t.value.clone() {
							ParserStackItem::Node(MPNode::new(MPPosition::new_t(&term0t), MPNodeType::String(parsed)))
						} else {
							panic!("Unexpected token type.")
						}
					},
					36 => {
						let _ = stack.pop();
						let term4dv = if let ParserStackItem::DualList(a,b) = stack.pop().unwrap() {(a,b)} else {panic!("Unexpected item in stack.");};
						let _ = stack.pop();
						let term3n = if let ParserStackItem::Node(a) = stack.pop().unwrap() {a} else {panic!("Unexpected item in stack.");};
						let _ = stack.pop();
						let _ = stack.pop();
						let _ = stack.pop();
						let term1n = if let ParserStackItem::Node(a) = stack.pop().unwrap() {a} else {panic!("Unexpected item in stack.");};
						let _ = stack.pop();
						let term0t = if let ParserStackItem::Token(a) = stack.pop().unwrap() {a} else {panic!("Unexpected item in stack.");};
						let mut conds = term4dv.0;
						let mut contents = term4dv.1;
						let mut last = term3n.clone();
						if !contents.is_empty() {
							last = contents.last().unwrap().clone();
						}
						conds.insert(0, term1n);
						contents.insert(0, term3n);
						ParserStackItem::Node(MPNode::new(MPPosition::new_tn(&term0t, &last), MPNodeType::If(conds, contents)))
					},
					39 => {
						ParserStackItem::DualList(Vec::new(),Vec::new())
					},
					0 | 18 | 22 | 26..=28 | 33..=35 | 51..=55 => {
						let _ = stack.pop();
						let term0n = if let ParserStackItem::Node(a) = stack.pop().unwrap() {a} else {panic!("Unexpected item in stack.");};
						ParserStackItem::Node(term0n)
					},
					21 => {
						let _ = stack.pop();
						let term0t = if let ParserStackItem::Token(a) = stack.pop().unwrap() {a} else {panic!("Unexpected item in stack.");};
						if let StackMaximaParserTokenType::FLOAT(parsed, raw) = term0t.value.clone() {
							ParserStackItem::Node(MPNode::new(MPPosition::new_t(&term0t), MPNodeType::Float(parsed, raw)))
						} else {
							panic!("Unexpected token type.")
						}
					},
					2 => {
						let _ = stack.pop();
						let term0v = if let ParserStackItem::List(a) = stack.pop().unwrap() {a} else {panic!("Unexpected item in stack.");};
						let first = term0v.first().unwrap().clone();
						let last = term0v.last().unwrap().clone();
						ParserStackItem::Node(MPNode::new(MPPosition::new_nn(&first, &last), MPNodeType::Root(term0v,Vec::new(),Vec::new())))
					},
					20 => {
						let _ = stack.pop();
						let term0t = if let ParserStackItem::Token(a) = stack.pop().unwrap() {a} else {panic!("Unexpected item in stack.");};
						if let StackMaximaParserTokenType::INT(parsed, raw) = term0t.value.clone() {
							ParserStackItem::Node(MPNode::new(MPPosition::new_t(&term0t), MPNodeType::Integer(parsed, raw)))
						} else {
							panic!("Unexpected token type.")
						}
					},
					11 => {
						let _ = stack.pop();
						let term2t = if let ParserStackItem::Token(a) = stack.pop().unwrap() {a} else {panic!("Unexpected item in stack.");};
						let _ = stack.pop();
						let term1v = if let ParserStackItem::List(a) = stack.pop().unwrap() {a} else {panic!("Unexpected item in stack.");};
						let _ = stack.pop();
						let term0t = if let ParserStackItem::Token(a) = stack.pop().unwrap() {a} else {panic!("Unexpected item in stack.");};
						ParserStackItem::Node(MPNode::new(MPPosition::new_tt(&term0t, &term2t), MPNodeType::List(term1v)))
					},
					14 | 30 | 32 | 41 => {
						let _ = stack.pop();
						let mut term1mv = if let ParserStackItem::List(a) = stack.pop().unwrap() {a} else {panic!("Unexpected item in stack.");};
						let _ = stack.pop();
						let term0n = if let ParserStackItem::Node(a) = stack.pop().unwrap() {a} else {panic!("Unexpected item in stack.");};
						term1mv.insert(0, term0n);
						ParserStackItem::List(term1mv)
					},
					1 => {
						ParserStackItem::Node(MPNode::new(MPPosition {startline: 1, startcolumn: 1, startbyte:0, endline: 1, endcolumn: 1, endbyte: 0}, MPNodeType::Root(Vec::new(),Vec::new(),Vec::new())))
					},
					6 => {
						let _ = stack.pop();
						let term0t = if let ParserStackItem::Token(a) = stack.pop().unwrap() {a} else {panic!("Unexpected item in stack.");};
						let _ = term0t;
						ParserStackItem::List(Vec::new())
					},
					68..=69 => {
						let _ = stack.pop();
						let term1t = if let ParserStackItem::Token(a) = stack.pop().unwrap() {a} else {panic!("Unexpected item in stack.");};
						let _ = stack.pop();
						let term0n = if let ParserStackItem::Node(a) = stack.pop().unwrap() {a} else {panic!("Unexpected item in stack.");};
						if let StackMaximaParserTokenType::SYMBOL(symbol) = term1t.value.clone() {
							ParserStackItem::Node(MPNode::new(MPPosition::new_nt(&term0n, &term1t), MPNodeType::PostfixOperation(Box::new(term0n), symbol)))
						} else {
							panic!("Unexpected token type.")
						}
					},
					56..=67 => {
						let _ = stack.pop();
						let term1n = if let ParserStackItem::Node(a) = stack.pop().unwrap() {a} else {panic!("Unexpected item in stack.");};
						let _ = stack.pop();
						let term0t = if let ParserStackItem::Token(a) = stack.pop().unwrap() {a} else {panic!("Unexpected item in stack.");};
						if let StackMaximaParserTokenType::SYMBOL(symbol) = term0t.value.clone() {
							ParserStackItem::Node(MPNode::new(MPPosition::new_tn(&term0t, &term1n), MPNodeType::PrefixOperation(symbol, Box::new(term1n))))
						} else {
							panic!("Unexpected token type.")
						}
					},
					16 => {
						let _ = stack.pop();
						let mut term2mv = if let ParserStackItem::List(a) = stack.pop().unwrap() {a} else {panic!("Unexpected item in stack.");};
						let _ = stack.pop();
						let term1n = if let ParserStackItem::Node(a) = stack.pop().unwrap() {a} else {panic!("Unexpected item in stack.");};
						let _ = stack.pop();
						let _ = stack.pop();
						term2mv.insert(0, term1n);
						ParserStackItem::List(term2mv)
					},
					3 => {
						let _ = stack.pop();
						let mut term2mv = if let ParserStackItem::List(a) = stack.pop().unwrap() {a} else {panic!("Unexpected item in stack.");};
						let _ = stack.pop();
						let term1v = if let ParserStackItem::List(a) = stack.pop().unwrap() {a} else {panic!("Unexpected item in stack.");};
						let _ = stack.pop();
						let term0n = if let ParserStackItem::Node(a) = stack.pop().unwrap() {a} else {panic!("Unexpected item in stack.");};
						let mut last = term0n.clone();
						if !term1v.is_empty() {
							last = term1v.last().unwrap().clone();
						}
						term2mv.insert(0, MPNode::new(MPPosition::new_nn(&term0n, &last), MPNodeType::Statement(Box::new(term0n), term1v)));
						ParserStackItem::List(term2mv)
					},
					40 => {
						let _ = stack.pop();
						let term2n = if let ParserStackItem::Node(a) = stack.pop().unwrap() {a} else {panic!("Unexpected item in stack.");};
						let _ = stack.pop();
						let term1t = if let ParserStackItem::Token(a) = stack.pop().unwrap() {a} else {panic!("Unexpected item in stack.");};
						let _ = stack.pop();
						let term0v = if let ParserStackItem::List(a) = stack.pop().unwrap() {a} else {panic!("Unexpected item in stack.");};
						if term0v.is_empty() {
							ParserStackItem::Node(MPNode::new(MPPosition::new_tn(&term1t, &term2n), MPNodeType::Loop(Box::new(term2n), Vec::new())))
						} else {
							let first = term0v[0].clone();
							ParserStackItem::Node(MPNode::new(MPPosition::new_nn(&first, &term2n), MPNodeType::Loop(Box::new(term2n), term0v)))
						}
					},
					19 => {
						let _ = stack.pop();
						let term0t = if let ParserStackItem::Token(a) = stack.pop().unwrap() {a} else {panic!("Unexpected item in stack.");};
						if let StackMaximaParserTokenType::BOOL(parsed, raw) = term0t.value.clone() {
							ParserStackItem::Node(MPNode::new(MPPosition::new_t(&term0t), MPNodeType::Boolean(parsed, raw)))
						} else {
							panic!("Unexpected token type.")
						}
					},
					12 => {
						let _ = stack.pop();
						let term2t = if let ParserStackItem::Token(a) = stack.pop().unwrap() {a} else {panic!("Unexpected item in stack.");};
						let _ = stack.pop();
						let term1v = if let ParserStackItem::List(a) = stack.pop().unwrap() {a} else {panic!("Unexpected item in stack.");};
						let _ = stack.pop();
						let term0t = if let ParserStackItem::Token(a) = stack.pop().unwrap() {a} else {panic!("Unexpected item in stack.");};
						ParserStackItem::Node(MPNode::new(MPPosition::new_tt(&term0t, &term2t), MPNodeType::Set(term1v)))
					},
					_ => {
						panic!("Unknown rule.");
					}
				};

				// If the rule was the Start rule then we can end here.
				if nt_name == *"Start" {
					if let ParserStackItem::Node(n) = term {
						if self.collect_comments {
							if let MPNodeType::Root(statements, mut precomments, mut internalcomments) = n.value {
								let mut endoflaststatement: usize = 0;
								for statement in &statements {
									let curpos = &statement.position;
									let mut pre: Vec<MPNode> = Vec::new();
									let mut internal: Vec<MPNode> = Vec::new();

									for com in &comments {
										if com.position.startbyte >= endoflaststatement && com.position.endbyte <= curpos.startbyte {
											pre.push(com.clone());
										} else if com.position.startbyte > curpos.startbyte && com.position.endbyte <= curpos.endbyte {
											internal.push(com.clone());
										}
									}

									precomments.push(pre);
									internalcomments.push(internal);
									endoflaststatement = curpos.endbyte;
								}
								// Always create extra list at the end for tail comments.
								let mut post: Vec<MPNode> = Vec::new();
								for com in &comments {
									if com.position.startbyte > endoflaststatement {
										post.push(com.clone());
									}
								}
								precomments.push(post);
								return Some(MPNode::new(n.position, MPNodeType::Root(statements, precomments, internalcomments)));
							} else {
								panic!("Not a root node!?");
							}
						} else {
							return Some(n);	
						}
					} else {
						println!("NONE: {:?}", term);
						// TODO: error the Start rule did not reduce to a node.
						return None;
					}
				} else {
					// Push the new term to the stack.
					stack.push(term);	
				}

				

				// Else figure out where to go now.
				if let ParserStackItem::State(state) = stack[stack.len()-2] {
					let next: usize = *parsertables.goto.get(&state).unwrap().get(&nt_id).unwrap();
					stack.push(ParserStackItem::State(next));
				} else {
					panic!("Expected a state in the stack, got something else.");
				}

			}

		}


	}
}