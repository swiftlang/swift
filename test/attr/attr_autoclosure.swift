// RUN: %swift -parse %s -verify

// Simple case.
@autoclosure var fn : () -> Int = 4

@autoclosure func func1() {}  // expected-error {{'autoclosure' attribute cannot be applied to this declaration}}
@autoclosure var v1 : Int = 4 // expected-error {{'autoclosure' attribute may only be applied to values of function type}}


struct SomeStruct {
  @autoclosure let property : () -> Int  // autoclosures work as an property as well.

  init() {
    property = 4
    let a : Int = property()
  }
}

class BaseClass {
  // FIXME: rdar://19311652 - class properties don't work due to synthesized code issues
  @autoclosure var property : () -> Int = 4 // autoclosures work as an property as well.

}

class DerivedClass {
  var property : () -> Int { get {} set {} }
}


