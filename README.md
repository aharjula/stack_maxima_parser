# stack_maxima_parser

This is a library for those of us that want to parse [STACK](https://stack-assessment.org/)-Maxima syntax, i.e. the logic parts of STACK-questions.

Currently this has no real error handling, but maybe the world changes.

Basically, this is a LALR(1) parser, built on exploratory work at the STACK side.

## For who?

In particular, this will probably be of use to those building quick modification scripts working on their [gitsync](https://github.com/maths/moodle-qbank_gitsync)-clones of large question-banks. Probably combined with [this another library related to question-XML processing](https://crates.io/crates/position_preserving_moodle_question_xml_edit).

## Current state of development

Early days for this lib, expect new features as needs arise.

Changelog:
 
 - 0.1.8 parses on 32-bit targets such as WebAssembly: the parser tables are keyed by
   `u32`, because the hash of a `usize` key depends on the target's width, so tables built
   on a 64-bit host were not found on a 32-bit one and nothing parsed
 - 0.1.7 operator grouping now follows Maxima's binding powers (with STACK's operator
   declarations) everywhere, checked against Maxima itself: e.g. `a:b:0` is `a:(b:0)`,
   `a*b/c` is `(a*b)/c`, `-a*b` is `(-a)*b`, `a+2 and b` is `(a+2) and b`, and STACK's
   logic operators have their own levels. Same-level operators group to the left.
   `nounsub` is a prefix operator, `nounmul` is lexed, and the infix `xor`, `xnor`, `nor`
   and `nand` no longer panic. Bad input no longer panics or loops: unterminated strings
   and comments, and tokens the grammar does not know, make `parse` return `None` with
   the reason in `errors` (cleared at each parse), and semicolon insertion no longer
   repeats in front of a token that still does not fit
 - 0.1.6 changes to the precedences noted in STACK issue #1882
 - 0.1.5 some special assignment operator behaviour.
 - 0.1.4 plenty of extra operators and work on operator precedence, also
   the storage format of the tables has slightly changed
 - 0.1.3 fixed comment tokeniser and added logic to fallback to interpret KEYWORDs as IDs also `@` as infix operator
 - 0.1.2 some fixes to that extraction logic, also define behaviour for empty input
 - 0.1.1 the string extraction utility logic was tuned
 - 0.1.0 the first release

# Docs...

Check the tests, those should tell everything worth knowing. Or compile the rustdoc for a list of things.
