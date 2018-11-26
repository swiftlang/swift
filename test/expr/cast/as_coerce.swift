// RUN: %target-typecheck-verify-swift -enable-objc-interop

// Test the use of 'as' for type coercion (which requires no checking).
@objc protocol P1 {
  func foo()
}

class A : P1 { 
  @objc func foo() { }
}
@objc class B : A { 
  func bar() { }
}

func doFoo() {}

func test_coercion(_ a: A, b: B) {
  // Coercion to a protocol type
  let x = a as P1
  x.foo()
  // Coercion to a superclass type
  let y = b as A
  y.foo()
}

class C : B { }

class D : C { }


func prefer_coercion(_ c: inout C) {
  let d = c as! D
  c = d
}

// Coerce literals
var i32 = 1 as Int32
var i8 = -1 as Int8

// Coerce to a superclass with generic parameter inference
class C1<T> { 
  func f(_ x: T) { }
}
class C2<T> : C1<Int> { }

var c2 = C2<()>()
var c1 = c2 as C1
c1.f(5)

@objc protocol P {}
class CC : P {}
let cc: Any = CC()
if cc is P {
  doFoo()
}
if let p = cc as? P {
  doFoo()
  _ = p
}

// Test that 'as?' coercion fails.
let strImplicitOpt: String! = nil
_ = strImplicitOpt as? String // expected-warning{{conditional downcast from 'String?' to 'String' does nothing}}{{19-30=}}

class C3 {}
class C4 : C3 {}
class C5 {}

var c: AnyObject = C3()

if let castX = c as! C4? {} // expected-error {{cannot downcast from 'AnyObject' to a more optional type 'C4?'}}

// Only suggest replacing 'as' with 'as!' if it would fix the error.
C3() as C4 // expected-error {{'C3' is not convertible to 'C4'; did you mean to use 'as!' to force downcast?}} {{6-8=as!}}
C3() as C5 // expected-error {{cannot convert value of type 'C3' to type 'C5' in coercion}}

// Diagnostic shouldn't include @lvalue in type of c3.
var c3 = C3()
c3 as C4 // expected-error {{'C3' is not convertible to 'C4'; did you mean to use 'as!' to force downcast?}} {{4-6=as!}}

// <rdar://problem/19495142> Various incorrect diagnostics for explicit type conversions
1 as Double as Float // expected-error{{cannot convert value of type 'Double' to type 'Float' in coercion}}
1 as Int as String // expected-error{{cannot convert value of type 'Int' to type 'String' in coercion}}
Double(1) as Double as String // expected-error{{cannot convert value of type 'Double' to type 'String' in coercion}}
["awd"] as [Int] // expected-error{{cannot convert value of type 'String' to expected element type 'Int'}}
([1, 2, 1.0], 1) as ([String], Int) // expected-error{{cannot convert value of type 'Int' to expected element type 'String'}}
[[1]] as [[String]] // expected-error{{cannot convert value of type 'Int' to expected element type 'String'}}
(1, 1.0) as (Int, Int) // expected-error{{cannot convert value of type 'Double' to type 'Int' in coercion}}
(1.0, 1, "asd") as (String, Int, Float) // expected-error{{cannot convert value of type 'Double' to type 'String' in coercion}}
(1, 1.0, "a", [1, 23]) as (Int, Double, String, [String]) // expected-error{{cannot convert value of type 'Int' to expected element type 'String'}}

_ = [1] as! [String] // expected-warning{{cast from '[Int]' to unrelated type '[String]' always fails}}
_ = [(1, (1, 1))] as! [(Int, (String, Int))] // expected-warning{{cast from '[(Int, (Int, Int))]' to unrelated type '[(Int, (String, Int))]' always fails}}

// <rdar://problem/19495253> Incorrect diagnostic for explicitly casting to the same type
_ = "hello" as! String // expected-warning{{forced cast of 'String' to same type has no effect}} {{13-24=}}

// <rdar://problem/19499340> QoI: Nimble as -> as! changes not covered by Fix-Its
func f(_ x : String) {}
f("what" as Any as String) // expected-error {{'Any' is not convertible to 'String'; did you mean to use 'as!' to force downcast?}} {{17-19=as!}}
f(1 as String) // expected-error{{cannot convert value of type 'Int' to type 'String' in coercion}}

// <rdar://problem/19650402> Swift compiler segfaults while running the annotation tests
let s : AnyObject = C3()
s as C3 // expected-error{{'AnyObject' is not convertible to 'C3'; did you mean to use 'as!' to force downcast?}} {{3-5=as!}}
