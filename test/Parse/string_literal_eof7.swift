// RUN: %target-typecheck-verify-swift

// expected-error@+4 {{expected ')' at end of string interpolation}} expected-note @+4 {{to match this opening '('}}
// expected-error@+1 {{unterminated string literal}}
_ = """
    foo
    \("bar
    baz

