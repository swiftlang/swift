// RUN: %target-typecheck-verify-swift

// https://github.com/apple/swift/issues/48001
// Used to crash with "apply expression is not marked as throwing or
// non-throwing".

struct S : Error {}
func f(op: (() throws -> Void)?) rethrows { try op?() }
try? f(op: { throw S() })
