// RUN: %target-typecheck-verify-swift

// NOTE: DO NOT add a newline at EOF.
// expected-cparser-error@+1 {{unterminated string literal}} expected-astgen-error@+2 {{expected '"""' to end string literal}} expected-astgen-note@+1 {{to match this opening '"""'}}
_ = """
    foo