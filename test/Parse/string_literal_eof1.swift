// RUN: %target-typecheck-verify-swift

// NOTE: DO NOT add a newline at EOF.
// expected-error@+1 {{expected ')' at end of string interpolation}} expected-note @+1 {{to match this opening '('}}
_ = "foo\(