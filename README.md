# stack_maxima_parser

This is a library for those of us that want to parse [STACK](https://stack-assessment.org/)-Maxima syntax, i.e. the logic parts of STACK-questions.

Currently this has no real error handling, but maybe the world changes.

Basically, this is a LALR(1) parser, built on exploratory work at the STACK side.

## For who?

In particular, this will probably be of use to those building quick modification scripts working on their [gitsync](https://github.com/maths/moodle-qbank_gitsync)-clones of large question-banks. Probably combined with [this another library related to question-XML processing](https://crates.io/crates/position_preserving_moodle_question_xml_edit).

## Current state of development

Early days for this lib, expect new features as needs arise.

Changelog:

 - 0.1.0 the first release

# Docs...

Check the tests, those should tell everything worth knowing. Or compile the rustdoc for a list of things.
