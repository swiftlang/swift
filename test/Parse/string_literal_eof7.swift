// RUN: %target-typecheck-verify-swift

// expected-cparser-error@+4 {{cannot find ')' to match opening '(' in string interpolation}} expected-astgen-error@+4 {{expected '"' to end string literal}} expected-astgen-note@+4 {{to match this opening '('}} expected-astgen-note@+4 {{to match this opening '"'}} expected-astgen-note@+2 {{to match this opening '"""'}}
// expected-cparser-error@+1 {{unterminated string literal}} expected-cparser-error@+3 {{unterminated string literal}} expected-astgen-error@+4 {{unexpected code 'baz' in string literal}} expected-astgen-error@+4 {{expected ')' in string literal}} expected-astgen-error@+6 {{expected '"""' to end string literal}}
_ = """
    foo
    \("bar
    baz

