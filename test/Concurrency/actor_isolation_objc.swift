// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple %s -emit-sil -o /dev/null -verify
// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple %s -emit-sil -o /dev/null -verify -strict-concurrency=targeted
// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple %s -emit-sil -o /dev/null -verify -strict-concurrency=complete

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
    _ = #keyPath(A.w) // expected-error {{'#keyPath' refers to non-'@objc' property 'w'}}

    // expected-error@+1 {{cannot form key path to actor-isolated property 'x'}}
    _ = #keyPath(A.x) // expected-error{{argument of '#keyPath' refers to non-'@objc' property 'x'}}

    // expected-error@+1 {{cannot form key path to actor-isolated property 'y'}}
    _ = #keyPath(A.y) // expected-error{{argument of '#keyPath' refers to non-'@objc' property 'y'}}

    // expected-error@+1 {{cannot form key path to actor-isolated property 'computed'}}
    _ = #keyPath(A.computed) // expected-error{{argument of '#keyPath' refers to non-'@objc' property 'computed'}}
  }

  let w: Int = 0 // expected-note{{add '@objc' to expose this property to Objective-C}}

  var x: Int = 0 // expected-note{{add '@objc' to expose this property to Objective-C}}

  @objc var y: Int = 0 // expected-note{{add '@objc' to expose this property to Objective-C}}
  // expected-error@-1{{actor-isolated property 'y' cannot be '@objc'}}

  // expected-note@+1 {{add '@objc' to expose this property to Objective-C}}
  @objc var computed : Int { // expected-error{{actor-isolated property 'computed' cannot be '@objc'}}
    get { 120 }
  }

  func f() { } // expected-note{{add '@objc' to expose this instance method to Objective-C}}
  func g() { } // expected-note{{add '@objc' to expose this instance method to Objective-C}}
  @objc func h() async { }
}

actor Dril: NSObject {
  // expected-note@+2 {{add 'async' to function 'postSynchronouslyTo(twitter:)' to make it asynchronous}}
  // expected-error@+1 {{actor-isolated instance method 'postSynchronouslyTo(twitter:)' cannot be '@objc'}}
  @objc func postSynchronouslyTo(twitter msg: String) -> Bool {
    return true
  }

  @MainActor
  @objc func postFromMainActorTo(twitter msg: String) -> Bool {
    return true
  }
}


// makes sure the synthesized init's delegation kind is determined correctly.
actor Pumpkin: NSObject {}

actor Bad {
  @objc nonisolated lazy var invalid = 0
  // expected-warning@-1 {{'nonisolated' is not supported on lazy properties; this is an error in the Swift 6 language mode}}
}
