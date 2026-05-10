// RUN: %target-swift-frontend -typecheck -primary-file %s %S/Inputs/issue-52088-helper.swift -import-objc-header %S/Inputs/issue-52088.h -verify

// REQUIRES: objc_interop
// expected-no-warning

// https://github.com/apple/swift/issues/52088

protocol HasAssoc {
  associatedtype Assoc: TestProto
}

struct Impl: HasAssoc {
  // This used to trigger validation of the Test: TestProto conformance, but
  // /without/ propagating the correct selector onto Test.swiftFoo's getter.
  typealias Assoc = Test
}
