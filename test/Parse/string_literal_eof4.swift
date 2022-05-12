// RUN: %target-typecheck-verify-swift

// NOTE: DO NOT add a newline at EOF.
// expected-error@+1 {{unterminated string literal}} expected-note@+3 {{add an ending quote to string literal}} {{1-3=""""}}
_ = """
    foo