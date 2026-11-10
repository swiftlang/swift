// RUN: %target-typecheck-verify-swift

// NOTE: DO NOT add a newline at EOF.
// expected-cparser-error@+6 {{cannot find ')' to match opening '(' in string interpolation}}
// expected-cparser-error@+5 {{unterminated string literal}}
// expected-astgen-error@+4 2 {{expected '"' to end string literal}}
// expected-astgen-error@+3 {{expected ')' in string literal}}
// expected-astgen-note@+2 2 {{to match this opening '"'}}
// expected-astgen-note@+1 {{to match this opening '('}}
_ = "foo\("bar