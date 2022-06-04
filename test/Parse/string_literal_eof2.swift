// RUN: %target-typecheck-verify-swift

// NOTE: DO NOT add a newline at EOF.
// expected-error@+2 {{cannot find ')' to match opening '(' in string interpolation}}
// expected-error@+1 {{unterminated string literal}}
_ = "foo\("bar