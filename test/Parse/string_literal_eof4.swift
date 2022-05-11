// RUN: %target-typecheck-verify-swift

// NOTE: DO NOT add a newline at EOF.
// expected-error@+1 {{unterminated string literal}} expected-note@+1 {{add an ending quote to string literal}}
_ = """
    foo