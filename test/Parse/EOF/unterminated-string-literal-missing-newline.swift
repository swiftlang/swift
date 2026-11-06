// RUN: %target-typecheck-verify-swift 

// Check that we correctly process an unterminated string literal right near EOF.

// This file does not end in a trailing newline; this is deliberate, don't fix it!

/* expected-cparser-error {{unterminated string literal}} expected-astgen-error {{expected '"' to end string literal}} expected-astgen-note {{to match this opening '"'}} expected-astgen-warning {{string literal is unused}} */ "