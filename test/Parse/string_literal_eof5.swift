// RUN: %target-typecheck-verify-swift

// NOTE: DO NOT add a newline at EOF.
// expected-error@+1 {{unterminated string literal}}
_ = """
    foo
    \(
