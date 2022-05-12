// RUN: %target-typecheck-verify-swift

// NOTE: DO NOT add a newline at EOF.
// expected-error@+4 {{expected ')' at end of string interpolation}}
// expected-error@+1 {{unterminated string literal}}
_ = """
    foo
    \("bar