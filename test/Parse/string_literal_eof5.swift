// RUN: %target-typecheck-verify-swift

// NOTE: DO NOT add a newline at EOF.
// expected-error@+1 {{string interpolation needs to be closed by a )}}
_ = """
    foo
    \(
