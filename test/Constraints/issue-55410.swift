// RUN: %target-typecheck-verify-swift

// https://github.com/apple/swift/issues/55410

protocol P {}
typealias T = (P) -> Void
let x: T! = [1, 2, 3].reversed().reduce()
// expected-error@-1 {{missing arguments for parameters #1, #2 in call}}
