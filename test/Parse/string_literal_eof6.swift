// RUN: %target-typecheck-verify-swift

// NOTE: DO NOT add a newline at EOF.
// expected-error@+3 {{expected ')' at end of string interpolation}} expected-note @+3 {{add ')' to end of interpolation statement}} {{11-11=)}}
_ = """
    foo
    \("bar