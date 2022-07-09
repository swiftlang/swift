// RUN: %target-typecheck-verify-swift

// expected-error@+4 {{cannot find ')' to match opening '(' in string interpolation}}
// expected-error@+1 {{unterminated string literal}} expected-error@+3 {{unterminated string literal}}
_ = """
    foo
    \("bar
    baz

