// RUN: %target-parse-verify-swift

// Test the use of 'as' for type coercion (which requires no checking).
@objc protocol P1 {
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

class C : B { }

class D : C { }


func prefer_coercion(inout c: C) {
  var d = c as! D
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

@objc protocol P {}
class CC : P {}
let cc: Any = CC()
if cc is P {
   println("P")
}
if let p = cc as? P {
   println("P")
}

// Test that 'as?' coercion fails.
let strImplicitOpt: String! = nil
strImplicitOpt as? String // expected-error{{conditional downcast from 'String!' to 'String' always succeeds}}

class C3 {}
class C4 : C3 {}
class C5 {}

var c: AnyObject = C3()

if let castX = c as! C4? {} // expected-error {{cannot downcast from 'AnyObject' to a more optional type 'C4?'}}

// Only suggest replacing 'as' with 'as!' if it would fix the error.
C3() as C4 // expected-error {{'C3' is not convertible to 'C4'; did you mean to use 'as!' to force downcast?}}
C3() as C5 // expected-error {{'C3' is not convertible to 'C5'}}
