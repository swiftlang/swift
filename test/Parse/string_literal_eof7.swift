// RUN: %target-typecheck-verify-swift

// expected-error@+4 {{unterminated string literal}}
// expected-error@+1 {{unterminated string literal}}
_ = """
    foo
    \("bar
    baz

