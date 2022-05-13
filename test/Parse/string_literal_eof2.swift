// RUN: %target-typecheck-verify-swift

// NOTE: DO NOT add a newline at EOF.
// expected-error@+2 {{expected ')' at end of string interpolation}} 
// expected-note @+1 {{add ')' to end of interpolation statement}} {{15-15=)}}
_ = "foo\("bar