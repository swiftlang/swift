// RUN: %swift -parse %s -verify

@__nocapture var fn : () -> Int = { 4 }  // expected-error {{'__nocapture' may only be used on 'parameter' declarations}}

func takesClosure(@__nocapture fn : () -> Int) {
  takesClosure { 4 }  // ok
}

class SomeClass {
  final var x = 42

  func test() {
    // Since 'takesClosure' doesn't capture its closure, it doesn't require
    // "self." qualification of member references.
    takesClosure { x }
  }


}
