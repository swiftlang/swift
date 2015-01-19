// RUN: %target-parse-verify-swift -enable-character-literals

// Check that we correctly process an unterminated character literal right near
// EOF.

// expected-error@+1 {{expression does not conform to type 'CharacterLiteralConvertible'}}
/* expected-error {{unterminated string literal}} */ '
