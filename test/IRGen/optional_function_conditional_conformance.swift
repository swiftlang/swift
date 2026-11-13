// RUN: %target-swift-frontend -module-name main -emit-ir %s | %FileCheck %s

// https://github.com/swiftlang/swift/issues/90786
// For a conformance of Optional whose Wrapped type is a function type, the
// witness thunk's Self is a non-formal type, Optional of a SIL function type.
// IRGen used to crash searching it for fulfillments.

protocol P {
  func f()
}

extension Optional: P where Wrapped == () -> () {
  func f() {}
}

// The witness thunk for P.f() in the conformance of (() -> ())? is emitted.
// CHECK: define {{.*}} @"$syycSg4main1PA2bCP1fyyFTW"(
