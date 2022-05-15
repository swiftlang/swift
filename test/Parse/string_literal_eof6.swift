// RUN: %target-typecheck-verify-swift

// NOTE: DO NOT add a newline at EOF.
// expected-error@+3 {{expected ')' at end of string interpolation}}
_ = """
    foo
    \("bar