// RUN: %swift -parse %s -verify

// Test the use of 'as' for type coercion (which requires no checking).
@class_protocol @objc protocol P1 {
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


func prefer_coercion(inout c: C) {
  var d = c as D
  c = d
}

// Coerce literals
var i32 = 1 as Int32
var i8 = -1 as Int8

// Coerce to a superclass with generic parameter inference
class C1<T> { 
  func f(x: T) { }
}
class C2<T> : C1<Int> { }

var c2 = C2<()>()
var c1 = c2 as C1
c1.f(5)
