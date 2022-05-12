// RUN: %target-typecheck-verify-swift 

// Check that we correctly process an unterminated string literal right near EOF.

// This file does not end in a trailing newline; this is deliberate, don't fix it!

/* expected-error {{unterminated string literal}} */ "