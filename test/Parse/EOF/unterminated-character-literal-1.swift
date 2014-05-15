// RUN: %swift %s -verify -parse -enable-character-literals

// Check that we correctly process an unterminated character literal right near
// EOF.

// expected-error@+1 {{cannot convert the expression's type '$T0' to type 'CharacterLiteralConvertible'}}
/* expected-error {{unterminated string literal}} */ '
