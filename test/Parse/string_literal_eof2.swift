// RUN: %target-typecheck-verify-swift

// NOTE: DO NOT add a newline at EOF.
// expected-error@+1 {{expected ')' at end of string interpolation}} {{14-14=)}}
_ = "foo\("bar