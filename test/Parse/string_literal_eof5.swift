// RUN: %target-typecheck-verify-swift

// NOTE: DO NOT add a newline at EOF.
// expected-note @+4 {{add ')' to end of interpolation statement}} {{7-7=)}}
// expected-error@+3 {{expected ')' at end of string interpolation}}
_ = """
    foo
    \(
