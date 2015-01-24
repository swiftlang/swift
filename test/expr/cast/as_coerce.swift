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
strImplicitOpt as? String // expected-warning{{conditional cast from 'String!' to 'String' always succeeds}}

class C3 {}
class C4 : C3 {}
class C5 {}

var c: AnyObject = C3()

if let castX = c as! C4? {} // expected-error {{cannot downcast from 'AnyObject' to a more optional type 'C4?'}}

// Only suggest replacing 'as' with 'as!' if it would fix the error.
C3() as C4 // expected-error {{'C3' is not convertible to 'C4'; did you mean to use 'as!' to force downcast?}}
C3() as C5 // expected-error {{'C3' is not convertible to 'C5'}}

// <rdar://problem/19495142> Various incorrect diagnostics for explicit type conversions
1 as Double as Float // expected-error{{'Double' is not convertible to 'Float'}}
1 as Int as String // expected-error{{'Int' is not convertible to 'String'}}
Double(1) as Double as String // expected-error{{'Double' is not convertible to 'String'}}
["awd"] as [Int] // expected-error{{'[String]' is not convertible to '[Int]'}}
([1, 2, 1.0], 1) as ([String], Int) // expected-error{{'([Double], Int)' is not convertible to '([String], Int)'}}
// FIXME: below diagnostic should say [[Int]], not [Array<Int>]
[[1]] as [[String]] // expected-error{{'[Array<Int>]' is not convertible to '[[String]]'}}
(1, 1.0) as (Int, Int) // expected-error{{'(Int, Double)' is not convertible to '(Int, Int)'}}
(1.0, 1, "asd") as (String, Int, Float) // expected-error{{'(Double, Int, String)' is not convertible to '(String, Int, Float)'}}
(1, 1.0, "a", [1, 23]) as (Int, Double, String, [String]) // expected-error{{'(Int, Double, String, [Int])' is not convertible to '(Int, Double, String, [String])'}}

[1] as! [String] // expected-error{{'[Int]' is not convertible to '[String]'}}
[(1, (1, 1))] as! [(Int, (String, Int))] // expected-error{{'[(Int, (Int, Int))]' is not convertible to '[(Int, (String, Int))]'}}

// <rdar://problem/19495253> Incorrect diagnostic for explicitly casting to the same type
"hello" as! String // expected-warning{{forced cast from 'String' to 'String' always succeeds; did you mean to use 'as'?}}
