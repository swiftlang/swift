// RUN: %target-swift-frontend  -disable-availability-checking -warn-concurrency %s -emit-sil -o /dev/null -verify
// RUN: %target-swift-frontend  -disable-availability-checking -warn-concurrency %s -emit-sil -o /dev/null -verify -enable-experimental-feature SendNonSendable

// REQUIRES: concurrency
// REQUIRES: objc_interop
// REQUIRES: asserts

import Foundation

final class A: NSObject, Sendable {
  let x: Int = 5
}

final class B: NSObject, Sendable {
  var x: Int = 5 // expected-warning{{stored property 'x' of 'Sendable'-conforming class 'B' is mutable}}
}

class C { } // expected-note{{class 'C' does not conform to the 'Sendable' protocol}}

final class D: NSObject, Sendable {
  let c: C = C() // expected-warning{{stored property 'c' of 'Sendable'-conforming class 'D' has non-sendable type 'C'}}
}


