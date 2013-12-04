// RUN: %swift -parse %s -verify

// Test the use of 'as' for type coercion (which requires no checking).
@class_protocol, @objc protocol P1 {
  func foo()
}

class A : P1 { 
  func foo() { }
}
@objc class B : A { 
  func bar() { }
}

func test_coercion(a: A, b: B) {
  // Coercion to a protocol type
  var x = a as P1
  x.foo()
  // Coercion to a superclass type
  var y = b as A
  y.foo()
}

// Ensure that we prefer coercions over casts.
@objc class C {
  @conversion func __conversion() -> D {
    return D()
  }
}

class D : C {
}


func prefer_coercion(c: C) {
  var d = c as D
  c = d
}
