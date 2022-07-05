// RUN: %target-typecheck-verify-swift

// NOTE: DO NOT add a newline at EOF.
// expected-error@+2 {{unterminated string literal}}
// expected-error@+3 {{cannot find ')' to match opening '(' in string interpolation}}
_ = """
    foo
    \("bar