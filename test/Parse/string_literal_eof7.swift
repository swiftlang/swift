// RUN: %target-typecheck-verify-swift

// expected-error@+4 {{unterminated string literal}} expected-note@+4 {{add an ending quote to string literal}}
// expected-error@+1 {{unterminated string literal}} expected-note@+1 {{add an ending quote to string literal}}
_ = """
    foo
    \("bar
    baz

