// RUN: %target-typecheck-verify-swift

// NOTE: DO NOT add a newline at EOF.
// expected-note @+5 {{add ')' to end of interpolation statement}} {{1-1=)}}
// expected-error@+4 {{expected ')' at end of string interpolation}}
_ = """
    foo
    \(
