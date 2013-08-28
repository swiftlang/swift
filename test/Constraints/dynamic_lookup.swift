// RUN: %swift -parse %s -verify

class X : DynamicLookup {
  func foo(i : Int) { }
  func bar() { }
}

class Y : DynamicLookup, P {
  func foo(s : String) { }
  func wibble() { }
}

protocol P {
  func wibble()
}

struct Z {
  func wobble() { }
}

// Find methods via dynamic method lookup.
typealias Id = DynamicLookup
var obj : Id = X()
(obj.bar!)()
(obj.foo!)(5)
(obj.foo!)("hello")
(obj.wibble!)()
(obj.wobble!)() // expected-error{{'Id' does not have a member named 'wobble'}}

// Find methods via dynamic method lookup in a protocol composition.
var obj2 : protocol<DynamicLookup, P> = Y()
(obj2.bar!)()
(obj2.foo!)(5)
(obj2.foo!)("hello")
(obj2.wibble!)()
(obj2.wobble!)() // expected-error{{'protocol<P, DynamicLookup>' does not have a member named 'wobble'}}

