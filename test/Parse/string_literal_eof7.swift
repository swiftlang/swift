// RUN: %target-typecheck-verify-swift

// expected-error@+4 {{string interpolation needs to be closed by a )}}
// expected-error@+1 {{unterminated string literal}}
_ = """
    foo
    \("bar
    baz

