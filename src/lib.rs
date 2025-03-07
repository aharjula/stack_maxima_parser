//! Parser for STACK-Maxima
//!
//! For parsing STACK-logic.
//!
//! Fully position tracking, all parse results will provide position data
//! both in lines/columns as well as bytes in the source.
//! 
//! The lexer portion is tunable and can be adjusted to work with other
//! decimal separators.
//!
//! The parser itself has logic for inserting missing multiplication-signs
//! or for those working with STACK keyvals, missing semicolons.

pub mod lexer;
pub mod parser;
mod parsertables;