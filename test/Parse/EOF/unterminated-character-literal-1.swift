// RUN: %swift %s -verify

// Check that we correctly process an unterminated character literal right near
// EOF.

/* expected-error {{unterminated string literal}} */ '
