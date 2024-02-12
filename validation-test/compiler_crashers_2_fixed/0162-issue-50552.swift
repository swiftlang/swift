// RUN: %target-typecheck-verify-swift

// https://github.com/apple/swift/issues/50552

protocol P {}
struct A<C> {}
extension A: P where A: P {}
