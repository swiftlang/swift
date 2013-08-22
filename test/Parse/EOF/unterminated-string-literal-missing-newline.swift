// RUN: %swift %s -verify

// This file does not end in a trailing newline; this is deliberate, don't fix it!

// Check that we correctly process an unterminated string literal right near EOF.

/* expected-error {{unterminated string literal}} expected-warning {{missing newline at end of file}} */ "