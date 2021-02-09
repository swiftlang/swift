// RUN: %target-typecheck-verify-swift -enable-experimental-concurrency
// REQUIRES: concurrency
// REQUIRES: objc_interop

import Foundation

func g(_ selector: Selector) -> Int { }

actor A {
  func selectors() {
    _ = #selector(type(of: self).f) // expected-error{{argument of '#selector' refers to instance method 'f()' that is not exposed to Objective-C}}
    _ = #selector(type(of: self).g) // expected-error{{argument of '#selector' refers to instance method 'g()' that is not exposed to Objective-C}}
    _ = #selector(type(of: self).h)
  }

  func keypaths() {
    _ = #keyPath(A.x) // expected-error{{argument of '#keyPath' refers to non-'@objc' property 'x'}}
    _ = #keyPath(A.y) // expected-error{{argument of '#keyPath' refers to non-'@objc' property 'y'}}
    _ = #keyPath(A.z)
  }

  var x: Int = 0 // expected-note{{add '@objc' to expose this property to Objective-C}}
  @objc var y: Int = 0 // expected-note{{add '@objc' to expose this property to Objective-C}}
  // expected-error@-1{{actor-isolated property 'y' cannot be @objc}}
  @objc @actorIndependent(unsafe) var z: Int = 0

  func f() { } // expected-note{{add '@objc' to expose this instance method to Objective-C}}
  func g() { } // expected-note{{add '@objc' to expose this instance method to Objective-C}}
  @objc func h() async { }
}



