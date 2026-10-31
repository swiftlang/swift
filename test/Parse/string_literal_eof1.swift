// RUN: %target-typecheck-verify-swift

// NOTE: DO NOT add a newline at EOF.
// expected-cparser-error@+2 {{unterminated string literal}} expected-astgen-error@+2 {{expected '"' to end string literal}}
// expected-cparser-error@+1 {{cannot find ')' to match opening '(' in string interpolation}} expected-astgen-error@+1 {{expected value and ')' in string literal}}
_ = "foo\( // expected-astgen-note {{to match this opening '"'}}