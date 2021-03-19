// RUN: %target-typecheck-verify-swift -enable-experimental-concurrency

// REQUIRES: concurrency
// REQUIRES: objc_interop

import Foundation

final class A: NSObject, Sendable {
  let x: Int = 5
}

final class B: NSObject, Sendable {
  var x: Int = 5 // expected-error{{stored property 'x' of 'Sendable'-conforming class 'B' is mutable}}
}

class C { }

final class D: NSObject, Sendable {
  let c: C = C() // expected-error{{stored property 'c' of 'Sendable'-conforming class 'D' has non-sendable type 'C'}}
}


