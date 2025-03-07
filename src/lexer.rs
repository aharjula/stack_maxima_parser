//! Lexer for the tokens of the STACK-Maxima syntax. Soem configuration
//! Options are present:
//!  - LIST_SEP and DECIMAL_SEP can be adjusted.
//!  - Support for LISP-identifiers can be enabled, to parse some bits
//!    of the STACK-Maxima library isntead of just the questions.
//!  - `+-`-operator related settings.
//! 
//! This lexer is not optimised for speed, it spends much time keeping
//! track of the original position so that we can reference back to 
//! the original bytes should someone want to modify the source.

use std::collections::VecDeque;
use std::collections::HashMap;
use unicode_categories::UnicodeCategories;

/// Types of tokens.
#[derive(PartialEq, Clone)]
pub enum StackMaximaParserTokenType {
    /// First the identified keyword and then the untranslated one.
    KEYWORD(String,String),
    /// The value of the identifier, might have had some unicode rewriting.
    ID(String),
    /// Normalised value first with grouping and other details converted, the original form as the second item.
    INT(String,String),
    /// Normalised value first with separator and other details converted, the original form as the second item.
    FLOAT(String,String), 
    /// Possible orignal untranslated form as the second item.
    BOOL(bool,String),
    /// The unescaped content.
    STRING(String),
    /// Basically, some operator as raw value. 
    SYMBOL(String),
    /// The white-space as is.
    WS(String),
    /// The contents of a `/*comment*/`.
    COMMENT(String),
    /// Should the separator be configured to be something else than `,` the original will be in the first item.
    LISTSEP(String),
    /// Should the separator be configured to be something else than `;` the original will be in the first item.
    ENDTOKEN(String),
    /// More liberal value of an identifier.
    LISPID(String),
    /// Some sort of an lexer level error message.
    ERROR(String),
}
impl std::fmt::Debug for StackMaximaParserTokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            StackMaximaParserTokenType::KEYWORD(val, _orig) => {
                write!(f, "KEYWORD {}", val)
            },
            StackMaximaParserTokenType::ID(name) => {
                write!(f, "ID {}", name)
            },
            StackMaximaParserTokenType::INT(val, _orig) => {
                write!(f, "INT {}", val)
            },
            StackMaximaParserTokenType::FLOAT(val, _orig) => {
                write!(f, "FLOAT {}", val)
            },
            StackMaximaParserTokenType::BOOL(val, _orig) => {
                if *val {
                    write!(f, "BOOL true")
                } else {
                    write!(f, "BOOL false")
                }
            },
            StackMaximaParserTokenType::STRING(val) => {
                write!(f, "STRING '{}'", val)
            },
            StackMaximaParserTokenType::SYMBOL(val) => {
                write!(f, "SYMBOL {}", val)
            },
            StackMaximaParserTokenType::WS(_orig) => {
                write!(f, "WS")
            },
            StackMaximaParserTokenType::COMMENT(_comment) => {
                write!(f, "COMMENT")
            },
            StackMaximaParserTokenType::LISTSEP(_orig) => {
                write!(f, "LISTSEP")
            },
            StackMaximaParserTokenType::ENDTOKEN(_orig) => {
                write!(f, "ENDTOKEN")
            },
            StackMaximaParserTokenType::LISPID(val) => {
                write!(f, "LISPID {}", val)
            },
            StackMaximaParserTokenType::ERROR(err) => {
                write!(f, "ERR: {}", err)
            }
        }
    }
}

/// Tokens.
#[derive(PartialEq, Clone)]
pub struct StackMaximaToken {
    /// Line at which this token starts. First line being 1.
    pub startline: usize, 
    /// Column at which this token starts. First column being 1.
    pub startcolumn: usize,
    /// Length of this token in characters, a negative value could represent virtual tokens.
    pub length: i16, 
    /// The token ends on this line. Inclusive, the last char is in this position
    pub endline: usize,
    /// Inclusive, the last char is in this position
    pub endcolumn: usize, 
    /// The byte of the input string at which this token starts.
    pub startbyte: usize,
    /// The byte of the input string, before which this token ends.
    /// This token has been built from whatever was in `source[startbyte..endbyte]`, some unicode 
    /// char replacement may have happened though.
    pub endbyte: usize,
    /// The type and content of the token.    
    pub value: StackMaximaParserTokenType
}
impl std::fmt::Debug for StackMaximaToken {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "[{:>3}:{}-{}:{} for {}, {:?}]", self.startline, self.startcolumn, self.endline, self.endcolumn, self.length, self.value)
    }
}

/// Internally, every single unicode char we pull out of the stream is given coordinates.
/// Should a char be mapped to multiple chars they will share the original chars coords.
#[derive(Clone)]
struct Char {
	c: char,
	line: usize,
	column: usize,
	byte: usize
}

/// The actual lexer object.
pub struct StackMaximaLexer {
    chars: VecDeque<Char>,
    tokens: VecDeque<StackMaximaToken>,
    /// Decimal separator. Typically, `'.'`.
    pub decimal_sep: char,
    /// List separator. Typically, `','`.
    pub list_sep: char,
    /// Supported end_tokens.  Typically, `vec![';','$']`.
    pub end_tokens: Vec<char>,
    /// Do we support the `#pm#`-operator?
    pub pm: bool,
    /// Are keywords case insensitive? Do we convert "tRuE" to "true"?
    pub case_isensitive_keywords: bool,
    /// Are there mappings for other keywords, in other languages?
    pub localised_keywords: HashMap<String, String>, // Key is the localised one, the value is the real keyword. This allows aliases.
    /// Should we identify LISP-identifiers as tokens? Set to `false` if you work with questions, `true` if you work with the STACK-backend.
    pub lisp_ids: bool
}
impl StackMaximaLexer { 
    // Initialises a lexer with default settings, and some input string.
    pub fn new(input: String) -> StackMaximaLexer {
    	let mut deque: VecDeque<Char> = VecDeque::new();
    	let mut line: usize = 1;
    	let mut column: usize = 1;
    	let mut byte: usize = 0;

    	for c in input.chars() {
    		deque.push_back(Char {
    			c,
    			line,
				column,
				byte
    		});
    		// Update location
    		match c {
    			'\n' => {
    				line += 1;
    				column = 1;
    				byte += c.len_utf8();
    			}
    			_ => {
    				column += 1;
    				byte += c.len_utf8();
    			}
    		}
    	}

        StackMaximaLexer {
            chars: deque,
            tokens: VecDeque::new(),
            decimal_sep: '.',
            list_sep: ',',
            end_tokens: vec![';', '$'],
            pm: true,
            case_isensitive_keywords: false,
            localised_keywords: HashMap::new(),
            lisp_ids: false
        }
    }

    /// Resets the token, position and char buffers of the lexer and set the input string.
    pub fn set_source(&mut self, input: String) {
    	let mut deque: VecDeque<Char> = VecDeque::new();
    	let mut line: usize = 1;
    	let mut column: usize = 0;
    	let mut byte: usize = 0;

    	for c in input.chars() {
    		deque.push_back(Char {
    			c,
    			line,
				column,
				byte
    		});
    		// Update location
    		match c {
    			'\n' => {
    				line += 1;
    				column = 0;
    				byte += c.len_utf8();
    			}
    			_ => {
    				column += 1;
    				byte += c.len_utf8();
    			}
    		}
    	}

    	self.chars = deque;
    	self.tokens = VecDeque::new();
    }

    /// Tries to get the next token. Either from the char-buffer 
    /// or from a token buffer if any have been returned or previous 
    /// actions have generated multiple tokens.
    pub fn next_token(&mut self) -> Option<StackMaximaToken> {
        if !self.tokens.is_empty() {
            return self.tokens.pop_front();
        }

        let mut c = self.popc();

        match c {
            None => {
                None
            },
            Some(c0) => {
                let mut value: String = c0.c.to_string();
                let mut token: StackMaximaToken = StackMaximaToken {
                    startline: c0.line,
                    startcolumn: c0.column,
                    endline: c0.line,
                    endcolumn: c0.column,
                    startbyte: c0.byte,
                    endbyte: c0.byte + c0.c.len_utf8(),
                    length: 1,
                    value: StackMaximaParserTokenType::ERROR("next uninitialised".to_string())
                };
                match c0.c {
                    '0'..='9' | '.' => {
                        // Note the dot may just be a dot.
                        if c0.c == '.' && self.decimal_sep != '.' {
                            token.value = StackMaximaParserTokenType::SYMBOL(value);
                            return Some(token);    
                        }
                        self.eat_num(c0)
                    },
                    '-' | '(' | ')' | '[' | ']' | '{' | '}' | '~' | '=' | '|' => {
                        // Simple symbols.
                        token.value = StackMaximaParserTokenType::SYMBOL(value);
                        Some(token)
                    },
                    '<' | '>' => {
                        // Maybe longer.
                        c = self.popc();
                        match c {
                            None => {
                                token.value = StackMaximaParserTokenType::SYMBOL(value);
                                Some(token)
                            },
                            Some(c1) => {
                                match c1.c {
                                    '=' => {
                                        value.push(c1.c);
                                        token.length = 2;
                                        token.endcolumn = c1.column;
                                        token.endbyte = c1.byte + c1.c.len_utf8();
                                        token.value = StackMaximaParserTokenType::SYMBOL(value);
                                        Some(token)
                                    },
                                    _ => {
                                        self.pushc(c1);
                                        token.value = StackMaximaParserTokenType::SYMBOL(value);
                                        Some(token)
                                    }
                                }
                            }
                        }
                    },
                    '*' | '^' | '!' | '\'' => {
                        // Potenttially doubling ones.
                        c = self.popc();
                        match c {
                            None => {
                                token.value = StackMaximaParserTokenType::SYMBOL(value);
                                Some(token)
                            },
                            Some(c1) => {
                                match c1.c {
                                    '*' | '^' | '!' | '\'' => {
                                        if c1.c == c0.c {
                                            value.push(c1.c);
                                            token.length = 2;
                                            token.endcolumn = c1.column;
                                            token.endbyte = c1.byte + c1.c.len_utf8();
                                            token.value = StackMaximaParserTokenType::SYMBOL(value);
                                            Some(token)
                                        } else {
                                            self.pushc(c1);
                                            token.value = StackMaximaParserTokenType::SYMBOL(value);
                                            Some(token)
                                        }
                                    },
                                    _ => {
                                        self.pushc(c1);
                                        token.value = StackMaximaParserTokenType::SYMBOL(value);
                                        Some(token)
                                    }
                                }
                            }
                        }
                    },
                    '"' => {
                        self.eat_string(c0)
                    },
                    '+' => {
                        // Do we have '+-' active?
                        if self.pm {
                            c = self.popc();
                            match c {
                                None => {
                                    token.value = StackMaximaParserTokenType::SYMBOL(value);
                                    Some(token)
                                },
                                Some(c1) => {
                                    match c1.c {
                                        '-' => {
                                            value.push(c1.c);
                                            token.length = 2;
                                            token.endcolumn = c1.column;
                                        	token.endbyte = c1.byte + c1.c.len_utf8();
                                            token.value = StackMaximaParserTokenType::SYMBOL(value);
                                            Some(token)
                                        },
                                        _ => {
                                            self.pushc(c1);
                                            token.value = StackMaximaParserTokenType::SYMBOL(value);
                                            Some(token)
                                        }
                                    }
                                }
                            }
                        } else {
                            token.value = StackMaximaParserTokenType::SYMBOL(value);
                            Some(token)
                        }
                    },
                    ':' => {
                        // Various assingments.
                        c = self.popc();
                        match c {
                            None => {
                                token.value = StackMaximaParserTokenType::SYMBOL(value);
                                Some(token)
                            },
                            Some(c1) => {
                                match c1.c {
                                    ':' => {
                                        value.push(c1.c);
                                        token.endcolumn = c1.column;
                                        token.endbyte = c1.byte + c1.c.len_utf8();
                                        c = self.popc();
                                        match c {
                                            None => {
                                                token.value = StackMaximaParserTokenType::SYMBOL(value);
                                                Some(token)
                                            },
                                            Some(c2) => {
                                                match c2.c {
                                                    '=' => {
                                                        value.push(c2.c);
                                                        token.length = 3;
                                                        token.endcolumn = c2.column;
                                        				token.endbyte = c2.byte + c2.c.len_utf8();
                                                        token.value = StackMaximaParserTokenType::SYMBOL(value);
                                                        Some(token)
                                                    },
                                                    _ => {
                                                        self.pushc(c2);
                                                        token.value = StackMaximaParserTokenType::SYMBOL(value);
                                                        Some(token)
                                                    }
                                                }
                                            }
                                        }
                                    },
                                    '=' => {
                                        value.push(c1.c);
                                        token.length = 2;
                                        token.endcolumn = c1.column;
                                        token.endbyte = c1.byte + c1.c.len_utf8();
                                        token.value = StackMaximaParserTokenType::SYMBOL(value);
                                        Some(token)
                                    },
                                    _ => {
                                        self.pushc(c1);
                                        token.value = StackMaximaParserTokenType::SYMBOL(value);
                                        Some(token)
                                    }
                                }
                            }
                        }
                    },
                    '#' => {
                        // '#pm#'
                        c = self.popc();
                        match c {
                            None => {
                                token.value = StackMaximaParserTokenType::SYMBOL(value);
                                Some(token)
                            },
                            Some(c1) => {
                                match c1.c {
                                    'p' => {
                                        c = self.popc();
                                        match c {
                                            None => {
                                                token.value = StackMaximaParserTokenType::SYMBOL(value);
                                                Some(token)
                                            },
                                            Some(c2) => {
                                                match c2.c {
                                                    'm' => {
                                                        c = self.popc();
                                                        match c {
                                                            None => {
                                                                token.value = StackMaximaParserTokenType::SYMBOL(value);
                                                                Some(token)
                                                            },
                                                            Some(c3) => {
                                                                match c3.c {
                                                                    '#' => {
                                                                        token.length = 4;
                                                                        value.push(c1.c);
                                                                        value.push(c2.c);
                                                                        value.push(c3.c);
                                                                        token.endcolumn = c3.column;
                                        								token.endbyte = c3.byte + c3.c.len_utf8();
                                                                        token.value = StackMaximaParserTokenType::SYMBOL(value);
                                                                        Some(token)
                                                                    },
                                                                    _ => {
                                                                        self.pushc(c3);
                                                                        self.pushc(c2);
                                                                        self.pushc(c1);
                                                                        token.value = StackMaximaParserTokenType::SYMBOL(value);
                                                                        Some(token)
                                                                    }
                                                                }
                                                            }
                                                        }
                                                    },
                                                    _ => {
                                                        self.pushc(c2);
                                                        self.pushc(c1);
                                                        token.value = StackMaximaParserTokenType::SYMBOL(value);
                                                        Some(token)
                                                    }
                                                }
                                            }
                                        }
                                    },
                                    _ => {
                                        self.pushc(c1);
                                        token.value = StackMaximaParserTokenType::SYMBOL(value);
                                        Some(token)
                                    }
                                }
                            }
                        }
                    },
                    '/' => {
                        // Comment or div?
                        c = self.popc();
                        match c {
                            None => {
                                token.value = StackMaximaParserTokenType::SYMBOL(value);
                                Some(token)
                            },
                            Some(c1) => {
                                match c1.c {
                                    '*' => {
                                        self.eat_comment(token)
                                    },
                                    _ => {
                                        self.pushc(c1);
                                        token.value = StackMaximaParserTokenType::SYMBOL(value);
                                        Some(token)
                                    }
                                }
                            }
                        }
                    },
                    ' ' | '\t' | '\n' => {
                        self.eat_whitespace(token, c0)
                    },
                    '%' | '_' => {
                        self.eat_identifier(token, c0)
                    },
                    '?' => {
                        c = self.popc();
                        match c {
                            None => {
                                token.value = StackMaximaParserTokenType::SYMBOL(value);
                                Some(token)
                            },
                            Some(ref c1) => {
                                match c1.c {
                                    ' ' => {
                                        token.length = 2;
                                        value.push(c1.c);
                                        token.endcolumn = c1.column;
                                        token.endbyte = c1.byte + c1.c.len_utf8();
                                        token.value = StackMaximaParserTokenType::SYMBOL(value);
                                        Some(token)
                                    },
                                    '?' => {
                                        token.length = 2;
                                        token.endcolumn = c1.column;
                                        token.endbyte = c1.byte + c1.c.len_utf8();
                                        value.push(c1.c);
                                        c = self.popc();
                                        match c {
                                            None => {
                                            },
                                            Some(c2) => {
                                                match c2.c {
                                                    ' ' => {
                                                        value.push(c2.c);
                                                        token.length = 3;
                                                        token.endcolumn = c2.column;
                                        				token.endbyte = c2.byte + c2.c.len_utf8();
                                                    },
                                                    _ => {
                                                        self.pushc(c2);
                                                    }
                                                }
                                            }
                                        }
                                        token.value = StackMaximaParserTokenType::SYMBOL(value);
                                        Some(token)
                                    },
                                    cn if (cn == '\\' || cn == '%' || cn == '_' || cn.is_letter_uppercase() || cn.is_letter_lowercase()) => {
                                        token.value = StackMaximaParserTokenType::SYMBOL(value);
                                        if !self.lisp_ids {
                                            self.pushc(c1.clone());
                                            Some(token)
                                        } else {
                                            // LISP-IDS are emitted as two separate tokens. First that "?" then the id.
                                            let mut id_value: String = String::new();
                                            let mut id_token: StackMaximaToken = StackMaximaToken {
                                                startline: c1.line,
                                                startcolumn: c1.column,
                                                endline: c1.line,
                                                endcolumn: c1.column,
                                                startbyte: c1.byte,
                                        		endbyte: c1.byte + c1.c.len_utf8(),
                                                length: 1,
                                                value: StackMaximaParserTokenType::ERROR("lisp_id uninitialised".to_string())
                                            };
                                            loop {
                                                match c {
                                                    None => {
                                                        id_token.value = StackMaximaParserTokenType::LISPID(id_value);
                                                        self.tokens.push_front(id_token);
                                                        return Some(token); 
                                                    },
                                                    Some(cn) => {
                                                        match cn.c {
                                                            '\\' | '0'..='9' | '%' | '_' => {
                                                                id_token.length += 1;
                                                                id_token.endcolumn = cn.column;
                                        						id_token.endbyte = cn.byte + cn.c.len_utf8();
                                                                id_value.push(cn.c);
                                                            },
                                                            c if (c.is_letter_uppercase() || c.is_letter_lowercase()) => {
                                                                id_token.length += 1;
                                                                id_token.endcolumn = cn.column;
                                        						id_token.endbyte = cn.byte + c.len_utf8();
                                                                id_value.push(c);
                                                            },
                                                            _ => {
                                                                self.pushc(cn);
                                                                id_token.value = StackMaximaParserTokenType::LISPID(id_value);
                                                                self.tokens.push_front(id_token);
                                                                return Some(token); 
                                                            }
                                                        }
                                                    }
                                                }
                                                c = self.popc();
                                            }
                                        }
                                    },
                                    _ => {
                                        self.pushc(c1.clone());
                                        token.value = StackMaximaParserTokenType::SYMBOL(value);
                                        Some(token)
                                    }
                                }
                            }
                        }

                    },
                    c if (c.is_letter_lowercase() || c.is_letter_uppercase()) => {
                        self.eat_identifier(token, c0)
                    },
                    c if self.decimal_sep == c => {
                        // Note the dot may just be a dot. The logic in the number eater will detect that.
                        self.eat_num(c0)
                    },
                    c if self.list_sep == c => {
                        token.value = StackMaximaParserTokenType::LISTSEP(value);
                        Some(token)
                    },
                    c if self.end_tokens.contains(&c) => {
                        token.value = StackMaximaParserTokenType::ENDTOKEN(value);
                        Some(token)
                    },
                    _ => {
                        if self.decimal_sep == c0.c {
                            return self.eat_num(c0);
                        }
                        // TODO: Unicode equivalents.
                        None
                    }
                }
            }
        }
    }

    /// Allows returning a token to the lexer, e.g., when the parser 
    /// does its own insertions of virtual tokens.
    pub fn return_token(&mut self, token: StackMaximaToken) {
        self.tokens.push_front(token);
    }

    fn popc(&mut self) -> Option<Char> {
        self.chars.pop_front()
    }

    fn pushc(&mut self, c: Char) {
        self.chars.push_front(c);
    }

    fn eat_string(&mut self, c: Char) -> Option<StackMaximaToken> {
        let mut token: StackMaximaToken = StackMaximaToken {
            startline: c.line,
            startcolumn: c.column,
            endline: c.line,
            endcolumn: c.column,
            startbyte: c.byte,
            endbyte: c.byte + c.c.len_utf8(),
            length: 1,
            value: StackMaximaParserTokenType::ERROR("STRING NOT TERMINATED".to_string())
        };
        let mut value: String = String::new();
        let mut length: i16 = 1;

        loop {
            let mut c = self.popc();
            match c {
                None => {
                    token.length = length;
                    return Some(token);
                },
                Some(cn) => {
                    match cn.c {
                        '"' => {
                            length += 1;
							token.endcolumn = cn.column;
                            token.endbyte = cn.byte + cn.c.len_utf8();
                            break;
                        },
                        '\\' => {
                            c = self.popc();
                            length += 1;
                            match c {
                                None => {
                                    token.length = length;
                                    return Some(token);
                                },
                                Some(cn) => {
                                    match cn.c {
                                        // In Maxima "strings" only the case of newline after a \ is special, all others are interpereted as if that \ was not there and as if the second char had no other role.
                                        '\n' => {
                                            length += 1;
                                        },
                                        _ => {
                                            length += 1;
                                            token.endcolumn = cn.column;
                            				token.endbyte = cn.byte + cn.c.len_utf8();
                                            value.push(cn.c);
                                        }
                                    }
                                }
                            }
                        },
                        _ => {
                            value.push(cn.c);
                            token.endcolumn = cn.column;
                            token.endbyte = cn.byte + cn.c.len_utf8();
                            length += 1;
                        }
                    }
                }
            }
        }

        token.length = length;
        token.value = StackMaximaParserTokenType::STRING(value);
        Some(token)
    }

    fn eat_num(&mut self, first: Char) -> Option<StackMaximaToken> {
        let mut value: String = String::new();
        value.push(first.c);

        let mut pre_decimal = first.c != self.decimal_sep;
        let mut in_exponent = false;

        let mut token: StackMaximaToken = StackMaximaToken {
            startline: first.line,
            startcolumn: first.column,
            endline: first.line,
            endcolumn: first.column,
            startbyte: first.byte,
            endbyte: first.byte + first.c.len_utf8(),
            length: 1,
            value: StackMaximaParserTokenType::ERROR("eat_num uninitialised".to_string())
        };


        let mut c = self.popc();
        // The second char is special, in the sense that things may end fast.
        match c {
            None => {
                if value == "." {    
                    token.value = StackMaximaParserTokenType::SYMBOL(value);
                    return Some(token);
                } else {
                    token.value = StackMaximaParserTokenType::INT(value.clone(), value);
                    return Some(token);
                }
            },
            Some(c1) => {
                match c1.c {
                    '0'..='9' => {
                        value.push(c1.c);
                        token.endcolumn = c1.column;
                        token.endbyte = c1.byte + c1.c.len_utf8();
                    },
                    'e' | 'E' => {
                        value.push(c1.c);
                        in_exponent = true;
                        token.endcolumn = c1.column;
                        token.endbyte = c1.byte + c1.c.len_utf8();
                    },
                    c if c == self.decimal_sep => {
                        if pre_decimal {
                            value.push(c);
                            token.endcolumn = c1.column;
                        	token.endbyte = c1.byte + c1.c.len_utf8();
                            pre_decimal = false;
                        } else {
                            // Cannot have a second decimal separator at the start.
                            self.pushc(c1);
                            // Therefore must be a symbol, but even then things get interesting.
                            // If decimal_sep == '.' then all is fine but if not what to do.
                            // Leave the correcting to upper levels.
                            token.value = StackMaximaParserTokenType::SYMBOL(value);
                            return Some(token);
                        }
                    },
                    // TODO: digit grouping
                    _ => {
                        self.pushc(c1);
                        if value == "." || first.c == self.decimal_sep {
                            token.value = StackMaximaParserTokenType::SYMBOL(value);
                            return Some(token);
                        } else {
                            token.value = StackMaximaParserTokenType::INT(value.clone(), value);
                            return Some(token);
                        }
                    }
                }
            }
        }

        loop {
            c = self.popc();
            match c {
                None => {
                    break;
                },
                Some(cn) => {
                    match cn.c {
                        '0'..='9' => {
                            value.push(cn.c);
                            token.endcolumn = cn.column;
                        	token.endbyte = cn.byte + cn.c.len_utf8();
                        },
                        'e' | 'E' => {
                            if in_exponent {
                                // Second "e", break.
                                self.pushc(cn);
                                token.length = value.chars().count() as i16;
                                token.value = StackMaximaParserTokenType::FLOAT(value.replace(self.decimal_sep, "."), value);
                                return Some(token);
                            } else {
                                value.push(cn.c);
                                token.endcolumn = cn.column;
                        		token.endbyte = cn.byte + cn.c.len_utf8();
                                in_exponent = true;
                            }
                        },
                        c if c == self.decimal_sep => {
                            if !pre_decimal {
                                // Second decimal_sep, break.
                                self.pushc(cn);
                                token.length = value.chars().count() as i16;
                                token.value = StackMaximaParserTokenType::FLOAT(value.replace(self.decimal_sep, "."), value);
                                return Some(token);
                            } else {
                                value.push(c);
                                token.endcolumn = cn.column;
                        		token.endbyte = cn.byte + cn.c.len_utf8();
                                pre_decimal = false;
                            }
                        },
                        '+' | '-' => {
                            if in_exponent && (value.ends_with('e') || value.ends_with('E')) {
                                value.push(cn.c);
                                token.endcolumn = cn.column;
                        		token.endbyte = cn.byte + cn.c.len_utf8();
                            } else {
                                // Not the right place for those.
                                self.pushc(cn);
                                if !pre_decimal || in_exponent {
                                    token.length = value.chars().count() as i16;
                                    token.value = StackMaximaParserTokenType::FLOAT(value.replace(self.decimal_sep, "."), value);
                                    return Some(token);
                                } else {
                                    token.length = value.chars().count() as i16;
                                    token.value = StackMaximaParserTokenType::INT(value.clone(), value);
                                    return Some(token);
                                }
                            }
                        },
                        // TODO groupings.
                        _ => {
                            self.pushc(cn);
                            break;
                        }
                    }
                }
            }
        }
        if !pre_decimal || in_exponent {
            token.length = value.chars().count() as i16;
            token.value = StackMaximaParserTokenType::FLOAT(value.replace(self.decimal_sep, "."), value);
            Some(token)
        } else {
            token.length = value.chars().count() as i16;
            token.value = StackMaximaParserTokenType::INT(value.clone(), value);
            Some(token)
        }
    }

    fn eat_whitespace(&mut self, mut token: StackMaximaToken, first: Char) -> Option<StackMaximaToken> {
        let mut c: Option<Char>;
        let mut value: String = String::new();
        value.push(first.c);
        loop {
            c = self.popc();
            match c {
                None => {
                    break;
                },
                Some(cn) => {
                    match cn.c {
                        ' ' | '\t' | '\n' => {
                            token.length += 1;
                            value.push(cn.c);
                            token.endcolumn = cn.column;
                        	token.endbyte = cn.byte + cn.c.len_utf8();
                        },
                        _ => {
                            self.pushc(cn);
                            break;
                        }
                    }
                }
            }
        }
        token.value = StackMaximaParserTokenType::WS(value);
        Some(token)
    }

    fn eat_comment(&mut self, mut token: StackMaximaToken) -> Option<StackMaximaToken> {
        let mut c: Option<Char>;
        let mut value: String = String::new();
        token.length = 2;
        loop {
            c = self.popc();
            match c {
                None => {
                    token.value = StackMaximaParserTokenType::ERROR("COMMENT NOT TERMINATED".to_string());
                    return Some(token);
                },
                Some(c1) => {
                    match c1.c {
                        '*' => {
                            token.length += 1;
                            c = self.popc();
                            match c {
                                None => {
                                    token.value = StackMaximaParserTokenType::ERROR("COMMENT NOT TERMINATED".to_string());
                                    return Some(token);
                                },
                                Some(c2) => {
                                    match c2.c {
                                        '/' => {
                                            token.length += 1;
                            				token.endcolumn = c2.column;
                        					token.endbyte = c2.byte + c2.c.len_utf8();
                                            break;
                                        },
                                        _ => {
                                            value.push(c1.c);
                                            value.push(c2.c);
                                            token.length += 1;
                                        }
                                    }
                                }
                            }                
                        },
                        _ => {
                            token.length += 1;
                            token.endcolumn = c1.column;
                        	token.endbyte = c1.byte + c1.c.len_utf8();
                            value.push(c1.c);
                        }
                    }
                }
            }
        }
        token.value = StackMaximaParserTokenType::COMMENT(value);
        Some(token)
    }

    fn eat_identifier(&mut self, mut token: StackMaximaToken, first: Char) -> Option<StackMaximaToken> {
        let mut c: Option<Char>;
        let mut value: String = String::new();
        value.push(first.c);
        loop {
            c = self.popc();
            match c {
                None => {
                    break;
                },
                Some(cn) => {
                    match cn.c {
                        '0'..='9' | '%' | '_' => {
                            token.length += 1;
                            token.endcolumn = cn.column;
                        	token.endbyte = cn.byte + cn.c.len_utf8();
                            value.push(cn.c);
                        },
                        c if (c.is_letter_lowercase() || c.is_letter_uppercase()) => {
                            token.endcolumn = cn.column;
                        	token.endbyte = cn.byte + cn.c.len_utf8();
                            token.length += 1;
                            value.push(c);
                        },
                        _ => {
                            self.pushc(cn);
                            break;
                        }
                    }
                }
            }
        }
        self.kw_identify(token, value)
    }


    fn kw_identify(&mut self, mut token: StackMaximaToken, mut value: String) -> Option<StackMaximaToken> {
        let mut converted: String = value.clone();

        // If we are insensitive then all is lower case.
        if self.case_isensitive_keywords {
            converted = converted.to_lowercase();
        }

        // If we work with localised ones.
        if self.localised_keywords.contains_key(&converted) {
            converted = self.localised_keywords.get(&converted).unwrap().clone();
        }

        // Then match with keywords
        match converted.as_str() {
            "true" => {
                token.value = StackMaximaParserTokenType::BOOL(true, value);
                Some(token)
            },
            "false" => {
                token.value = StackMaximaParserTokenType::BOOL(false, value);
                Some(token)
            },
            "not" => {
                // Special, there is a case with following whitespace.
                let c = self.popc();
                match c {
                    None => {
                        token.value = StackMaximaParserTokenType::SYMBOL(value);
                        Some(token)
                    },
                    Some(cn) => {
                        match cn.c {
                            ' ' => {
                                value.push(cn.c);
                                token.endcolumn = cn.column;
                                token.endbyte = cn.byte + cn.c.len_utf8();
                                token.value = StackMaximaParserTokenType::SYMBOL(value);
                                Some(token)
                            },
                            _ => {
                                self.pushc(cn);
                                token.value = StackMaximaParserTokenType::SYMBOL(value);
                                Some(token)
                            }
                        }
                    }
                }
            },
            "nounnot" | "%not" | "%and" | "%or" | "and" | "or" | "nouneq" | "nounadd" | "nounand" | "nounor" | "nounsub" | "nounpow" | "noundiv" | "nand" | "nor" | "implies" | "xor" | "xnor" | "UNARY_RECIP" | "unary_recip" | "blankmult" => {
                // Various ops.
                token.value = StackMaximaParserTokenType::SYMBOL(value);
                Some(token)
            },
            "if" | "then" | "elseif" | "else" | "do" | "for" | "from" | "step" | "next" | "in" | "thru" | "while" | "unless" => {
                // Keywords.
                token.value = StackMaximaParserTokenType::KEYWORD(converted, value);
                Some(token)
            },
            _ => {
                // It was not a keyword it is an identifier.
                token.value = StackMaximaParserTokenType::ID(value);
                Some(token)
            }
        }
    }
}
