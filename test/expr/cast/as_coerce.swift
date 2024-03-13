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

// XXX TODO: Constant-folding should generate an error about 'C3' not being convertible to 'C4'
//if let castX = c as! C4? {}

// XXX TODO: Only suggest replacing 'as' with 'as!' if it would fix the error.
C3() as C4 // expected-error {{'C3' is not convertible to 'C4'}} 
// expected-note@-1 {{did you mean to use 'as!' to force downcast?}} {{6-8=as!}}
C3() as C5 // expected-error {{cannot convert value of type 'C3' to type 'C5' in coercion}}

// Diagnostic shouldn't include @lvalue in type of c3.
var c3 = C3()
// XXX TODO: This should not suggest `as!`
c3 as C4 // expected-error {{'C3' is not convertible to 'C4'}} 
// expected-note@-1{{did you mean to use 'as!' to force downcast?}} {{4-6=as!}}

// <rdar://problem/19495142> Various incorrect diagnostics for explicit type conversions
1 as Double as Float // expected-error{{cannot convert value of type 'Double' to type 'Float' in coercion}}
1 as Int as String // expected-error{{cannot convert value of type 'Int' to type 'String' in coercion}}
Double(1) as Double as String // expected-error{{cannot convert value of type 'Double' to type 'String' in coercion}}
["awd"] as [Int] // expected-error{{cannot convert value of type 'String' to expected element type 'Int'}}
([1, 2, 1.0], 1) as ([String], Int)
// expected-error@-1 2 {{cannot convert value of type 'Int' to expected element type 'String'}}
// expected-error@-2   {{cannot convert value of type 'Double' to expected element type 'String'}}
[[1]] as [[String]] // expected-error{{cannot convert value of type 'Int' to expected element type 'String'}}
(1, 1.0) as (Int, Int) // expected-error{{cannot convert value of type '(Int, Double)' to type '(Int, Int)' in coercion}}
(1.0, 1, "asd") as (String, Int, Float) // expected-error{{cannot convert value of type '(Double, Int, String)' to type '(String, Int, Float)' in coercion}}
(1, 1.0, "a", [1, 23]) as (Int, Double, String, [String])
// expected-error@-1 2 {{cannot convert value of type 'Int' to expected element type 'String'}}

_ = [1] as! [String] // OK
_ = [(1, (1, 1))] as! [(Int, (String, Int))] // OK

// <rdar://problem/19495253> Incorrect diagnostic for explicitly casting to the same type
_ = "hello" as! String // expected-warning{{forced cast of 'String' to same type has no effect}} {{13-24=}}

// <rdar://problem/19499340> QoI: Nimble as -> as! changes not covered by Fix-Its
func f(_ x : String) {}
f("what" as Any as String) // expected-error {{'Any' is not convertible to 'String'}} 
// expected-note@-1{{did you mean to use 'as!' to force downcast?}} {{17-19=as!}}
f(1 as String) // expected-error{{cannot convert value of type 'Int' to type 'String' in coercion}}

// <rdar://problem/19650402> Swift compiler segfaults while running the annotation tests
let s : AnyObject = C3()
s as C3 // expected-error{{'AnyObject' is not convertible to 'C3'}} 
// expected-note@-1{{did you mean to use 'as!' to force downcast?}} {{3-5=as!}}

// https://github.com/apple/swift/issues/48579

protocol P_48579 {}
do {
  func f1() -> Any {}
  func f2() {}
  @Sendable func f3() {}

  _ = f1 is P_48579 // expected-warning {{cast from '() -> Any' to unrelated type 'any P_48579' always fails}} // expected-note {{did you mean to call 'f1' with '()'?}}{{9-9=()}}
  _ = f1 as! P_48579 // expected-warning {{cast from '() -> Any' to unrelated type 'any P_48579' always fails}} // expected-note {{did you mean to call 'f1' with '()'?}}{{9-9=()}}
  _ = f1 as? P_48579 // expected-warning {{cast from '() -> Any' to unrelated type 'any P_48579' always fails}} // expected-note {{did you mean to call 'f1' with '()'}}{{9-9=()}}
  _ = f2 is P_48579 // expected-warning {{cast from '() -> ()' to unrelated type 'any P_48579' always fails}}
  _ = f2 as! P_48579 // expected-warning {{cast from '() -> ()' to unrelated type 'any P_48579' always fails}}
  _ = f2 as? P_48579 // expected-warning {{cast from '() -> ()' to unrelated type 'any P_48579' always fails}}

  _ = f1 as! AnyObject // expected-warning {{forced cast from '() -> Any' to 'AnyObject' always succeeds; did you mean to use 'as'?}}
  _ = f1 as? AnyObject // expected-warning {{conditional cast from '() -> Any' to 'AnyObject' always succeeds}}
  _ = f2 as! Any // expected-warning {{forced cast from '() -> ()' to 'Any' always succeeds; did you mean to use 'as'?}}
  _ = f2 as? Any // expected-warning {{conditional cast from '() -> ()' to 'Any' always succeeds}}


  func test1<T: P_48579, V: P_48579 & Sendable>(_: T.Type, _: V.Type) {
    _ = f1 is T // expected-warning {{cast from '() -> Any' to unrelated type 'T' always fails}} // expected-note {{did you mean to call 'f1' with '()'?}}{{11-11=()}}
    _ = f1 as! V // expected-warning {{cast from '() -> Any' to unrelated type 'V' always fails}} // expected-note {{did you mean to call 'f1' with '()'?}}{{11-11=()}}
    _ = f1 as? T // expected-warning {{cast from '() -> Any' to unrelated type 'T' always fails}} // expected-note {{did you mean to call 'f1' with '()'?}}{{11-11=()}}
    _ = f2 is T // expected-warning {{cast from '() -> ()' to unrelated type 'T' always fails}}
    _ = f2 as! V // expected-warning {{cast from '() -> ()' to unrelated type 'V' always fails}}
    _ = f2 as? T // expected-warning {{cast from '() -> ()' to unrelated type 'T' always fails}}
    _ = f3 is T // expected-warning {{cast from '@Sendable () -> ()' to unrelated type 'T' always fails}}
    _ = f3 as! V // expected-warning {{cast from '@Sendable () -> ()' to unrelated type 'V' always fails}}
    _ = f3 as? T // expected-warning {{cast from '@Sendable () -> ()' to unrelated type 'T' always fails}}
  }

  func test2<U, S: Sendable>(_: U.Type, _: S.Type) {
    _ = f1 is U  // Okay
    _ = f1 as! U // Okay
    _ = f1 as? U // Okay
    _ = f1 is U  // Okay
    _ = f2 as! U // Okay
    _ = f2 as? U // Okay

    _ = f2 is S   // expected-warning {{cast from '() -> ()' to unrelated type 'S' always fails}}
    _ = f2 as! S  // expected-warning {{cast from '() -> ()' to unrelated type 'S' always fails}}
    _ = f2 as? S  // expected-warning {{cast from '() -> ()' to unrelated type 'S' always fails}}
    _ = f3 is S   // Okay
    _ = f3 as! S  // Okay
    _ = f3 as? S  // Okay
  }
}

// https://github.com/apple/swift/issues/56297

let any: Any = 1
if let int = any as Int { // expected-error {{'Any' is not convertible to 'Int'}}
// expected-note@-1 {{did you mean to use 'as?' to conditionally downcast?}} {{18-20=as?}}
}

let _ = any as Int // expected-error {{'Any' is not convertible to 'Int'}}
// expected-note@-1 {{did you mean to use 'as!' to force downcast?}} {{13-15=as!}}
let _: Int = any as Int // expected-error {{'Any' is not convertible to 'Int'}}
// expected-note@-1 {{did you mean to use 'as!' to force downcast?}} {{18-20=as!}}
let _: Int? = any as Int // expected-error {{'Any' is not convertible to 'Int'}}
// expected-note@-1 {{did you mean to use 'as?' to conditionally downcast?}} {{19-21=as?}}

// https://github.com/apple/swift/issues/63926

do {
  func fn(_: Int) {} // expected-note {{found candidate with type '(Int) -> ()'}}
  // expected-note@-1 {{candidate '(Int) -> ()' has 1 parameter, but context '() -> Void' has 0}}
  func fn(_: Bool) {} // expected-note {{found candidate with type '(Bool) -> ()'}}
  // expected-note@-1 {{candidate '(Bool) -> ()' has 1 parameter, but context '() -> Void' has 0}}

  func fn_1(_: Bool) {} 

  let i = 0
  // Missing parameters.
  (fn as (Int, Int) -> Void)(i, i) // expected-error {{cannot convert value of type '(Int) -> ()' to type '(Int, Int) -> Void' in coercion}}
  (fn as (Bool, Bool) -> Void)(i, i) // expected-error {{cannot convert value of type '(Bool) -> ()' to type '(Bool, Bool) -> Void' in coercion}}
  // expected-error@-1 2{{type 'Int' cannot be used as a boolean; test for '!= 0' instead}}
  (fn as (Int, Bool) -> Void)(i, i) // expected-error {{cannot convert value of type '(Int) -> ()' to type '(Int, Bool) -> Void' in coercion}}
  // expected-error@-1 {{type 'Int' cannot be used as a boolean; test for '!= 0' instead}}
  (fn as (String) -> Void)(i) // expected-error {{no exact matches in reference to local function 'fn'}}
  // expected-error@-1 {{cannot convert value of type 'Int' to expected argument type 'String'}}

  // Extraneous parameters.
  (fn as () -> Void)() // expected-error {{no exact matches in reference to local function 'fn'}}
  (fn_1 as () -> Void)() // expected-error {{cannot convert value of type '(Bool) -> ()' to type '() -> Void' in coercion}}
}

// Test generic parameter inference through casts
do {
  class A<T> {
  }

  class B<U> : A<U> {
  }

  func test(v: any B<Int> & Sendable) {
    _ = v as A // infers `Int` for `A.T`
  }
}
