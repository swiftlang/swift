// RUN: %swift %s -verify -parse

// Check that we correctly process an unterminated character literal right near
// EOF.

/* expected-error {{unterminated character literal}} */ 'a
