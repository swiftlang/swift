// RUN: %target-typecheck-verify-swift

// NOTE: DO NOT add a newline at EOF.
// expected-error@+2 {{expected ')' at end of string interpolation}} expected-note @+2 {{to match this opening '('}}
// expected-error@+1 {{unterminated string literal}}
_ = "foo\("bar