// RUN: %target-typecheck-verify-swift

// expected-error@+2 {{unterminated string literal}}
// expected-error@+1 {{invalid escape sequence in literal}}
_ = "foo \

// NOTE: DO NOT add a newline at EOF.
// expected-error@+2 {{unterminated string literal}}
// expected-error@+1 {{invalid escape sequence in literal}}
_ = "foo \