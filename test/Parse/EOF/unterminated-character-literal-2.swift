// RUN: %swift %s -verify -parse -enable-character-literals

// Check that we correctly process an unterminated character literal right near
// EOF.

/* expected-error {{unterminated character literal}} */ 'a
