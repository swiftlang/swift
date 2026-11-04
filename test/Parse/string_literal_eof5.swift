// RUN: %target-typecheck-verify-swift

// NOTE: DO NOT add a newline at EOF.
// expected-cparser-error@+2 {{unterminated string literal}} expected-astgen-note@+2 {{to match this opening '"""'}}
// expected-cparser-error@+3 {{cannot find ')' to match opening '(' in string interpolation}} expected-astgen-error@+3 {{expected value and ')' in string literal}} expected-astgen-error@+4 {{expected '"""' to end string literal}}
_ = """
    foo
    \(
