// RUN: %target-swift-frontend -typecheck -primary-file %s %S/Inputs/sr9644-helper.swift -import-objc-header %S/Inputs/sr9644.h -verify

// REQUIRES: objc_interop
// expected-no-warning

protocol HasAssoc {
  associatedtype Assoc: TestProto
}

struct Impl: HasAssoc {
  // This used to trigger validation of the Test: TestProto conformance, but
  // /without/ propagating the correct selector onto Test.swiftFoo's getter.
  typealias Assoc = Test
}
