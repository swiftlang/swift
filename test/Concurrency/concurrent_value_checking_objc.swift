// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple -strict-concurrency=complete %s -emit-sil -o /dev/null -verify -verify-additional-prefix ni-
// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple -strict-concurrency=complete %s -emit-sil -o /dev/null -verify -verify-additional-prefix ni-ns- -enable-upcoming-feature NonisolatedNonsendingByDefault

// REQUIRES: concurrency
// REQUIRES: objc_interop
// REQUIRES: asserts
// REQUIRES: swift_feature_NonisolatedNonsendingByDefault

import Foundation

final class A: NSObject, Sendable {
  let x: Int = 5
}

final class B: NSObject, Sendable {
  var x: Int = 5 // expected-warning{{stored property 'x' of 'Sendable'-conforming class 'B' is mutable}}
}

class C { } // expected-note{{class 'C' does not conform to the 'Sendable' protocol}}

final class D: NSObject, Sendable {
  let c: C = C() // expected-warning{{stored property 'c' of 'Sendable'-conforming class 'D' has non-Sendable type 'C'}}
}

@MainActor
final class IsolatedNSObjectSubclass: NSObject, Sendable {}

class NSObjectSubclass: NSObject {}

@MainActor
class IsolatedObjCSubclass: NSObjectSubclass, Sendable {}
// expected-warning@-1:7 {{class 'IsolatedObjCSubclass' cannot conform to the 'Sendable' protocol; this will be an error in a future Swift language mode}}
// expected-note@-2:7 {{a 'Sendable' class cannot inherit from a non-Sendable class}}
// expected-note@-3:29 {{inherits from non-Sendable class 'NSObjectSubclass'}}


