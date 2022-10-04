// RUN: %target-typecheck-verify-swift -warn-redundant-requirements

// https://github.com/apple/swift/issues/50552

protocol P {}
struct A<C> {}
extension A: P where A: P {} // expected-warning {{redundant conformance constraint 'A<C>' : 'P'}}
