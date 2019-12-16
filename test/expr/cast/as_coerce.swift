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
Double(1) as Double as String // expected-error{{cannot convert value of type 'Double' to type 'String' in coercion}} expected-warning {{redundant cast to 'Double' has no effect}} {{11-21=}}
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
f("what" as Any as String) // expected-error {{'Any' is not convertible to 'String'; did you mean to use 'as!' to force downcast?}} {{17-19=as!}}
f(1 as String) // expected-error{{cannot convert value of type 'Int' to type 'String' in coercion}}

// <rdar://problem/19650402> Swift compiler segfaults while running the annotation tests
let s : AnyObject = C3()
s as C3 // expected-error{{'AnyObject' is not convertible to 'C3'; did you mean to use 'as!' to force downcast?}} {{3-5=as!}}

// SR-6022
func sr6022() -> Any { return 0 }
func sr6022_1() { return; }
protocol SR6022_P {}

_ = sr6022 as! SR6022_P // expected-warning {{cast from '() -> Any' to unrelated type 'SR6022_P' always fails}} // expected-note {{did you mean to call 'sr6022' with '()'?}}{{11-11=()}}
_ = sr6022 as? SR6022_P // expected-warning {{cast from '() -> Any' to unrelated type 'SR6022_P' always fails}} // expected-note {{did you mean to call 'sr6022' with '()'}}{{11-11=()}}
_ = sr6022_1 as! SR6022_P // expected-warning {{cast from '() -> ()' to unrelated type 'SR6022_P' always fails}}
_ = sr6022_1 as? SR6022_P // expected-warning {{cast from '() -> ()' to unrelated type 'SR6022_P' always fails}}

func testSR6022_P<T: SR6022_P>(_: T.Type) {
  _ = sr6022 as! T // expected-warning {{cast from '() -> Any' to unrelated type 'T' always fails}} // expected-note {{did you mean to call 'sr6022' with '()'?}}{{13-13=()}}
  _ = sr6022 as? T // expected-warning {{cast from '() -> Any' to unrelated type 'T' always fails}} // expected-note {{did you mean to call 'sr6022' with '()'?}}{{13-13=()}}
  _ = sr6022_1 as! T // expected-warning {{cast from '() -> ()' to unrelated type 'T' always fails}}
  _ = sr6022_1 as? T // expected-warning {{cast from '() -> ()' to unrelated type 'T' always fails}}
}

func testSR6022_P_1<U>(_: U.Type) {
  _ = sr6022 as! U // Okay
  _ = sr6022 as? U // Okay
  _ = sr6022_1 as! U // Okay
  _ = sr6022_1 as? U // Okay
}

_ = sr6022 as! AnyObject // expected-warning {{forced cast from '() -> Any' to 'AnyObject' always succeeds; did you mean to use 'as'?}}
_ = sr6022 as? AnyObject // expected-warning {{conditional cast from '() -> Any' to 'AnyObject' always succeeds}}
_ = sr6022_1 as! Any // expected-warning {{forced cast from '() -> ()' to 'Any' always succeeds; did you mean to use 'as'?}}
_ = sr6022_1 as? Any // expected-warning {{conditional cast from '() -> ()' to 'Any' always succeeds}}

// SR-11295
let sr11295a = "Hello"
_ = sr11295a as String // expected-warning {{redundant cast to 'String' has no effect}} {{14-24=}}

let sr11295b = 1
_ = sr11295b as Int // expected-warning {{redundant cast to 'Int' has no effect}} {{14-21=}}

typealias Type = String

let sr11295c: Type = "Hello Typealias"
_ = sr11295c as String // expected-warning {{redundant cast to 'String' has no effect}} {{14-24=}}

let sr11295d = "Hello Typealias"
_ = sr11295d as Type // expected-warning {{redundant cast to 'Type' (aka 'String') has no effect}} {{14-22=}}

_ = "Hello" as String // Ok
_ = 1 as Int64 // Ok
_ = [] as Set<Int> // Ok

class SR11295A {}
class SR11295B: SR11295A {}

var sr11295ap = SR11295A()
var sr11295bc = SR11295B()

_ = sr11295bc as SR11295A // Ok 

_ = 1 as Double as Double // expected-warning {{redundant cast to 'Double' has no effect}} {{17-27=}}
_ = Double(1) as Double // expected-warning {{redundant cast to 'Double' has no effect}} {{15-25=}}
_ = Int(1) as Int  // expected-warning {{redundant cast to 'Int' has no effect}} {{12-19=}}

typealias Double1 = Double
typealias Double2 = Double

let sr11295ta1: Double1 = 1.0
_ = sr11295ta1 as Double2 // expected-warning {{redundant cast from 'Double1' (aka 'Double') to 'Double2' (aka 'Double') has no effect}} {{16-27=}}
_ = sr11295ta1 as Double1 // expected-warning {{redundant cast to 'Double1' (aka 'Double') has no effect}} {{16-27=}}

func ff11295_0<T>(_ t: T) -> T { return t }
func ff11295_1(_ i: Int) -> Int { return i }

// Function call expressions
_ = ff11295_0(1 as Int) as Int // expected-warning {{redundant cast to 'Int' has no effect}} {{25-32=}}
_ = ff11295_1(1) as Int // expected-warning {{redundant cast to 'Int' has no effect}} {{18-25=}}

func ff11295_3(i: Int) {
  let a: [Int] = []
  _ = a.count - ((i + 1) as Int) // Ok
}

struct SR11295C {
  var i: Int 
}
struct SR11295D<T> {
  var t: T 

  func f() -> T { return t }
}
enum SR11295_E: Int {
  case a
}

// Coerce members
_ = SR11295C(i: 1).i as Int // expected-warning {{redundant cast to 'Int' has no effect}} {{22-29=}}
_ = SR11295D<Int>(t: 1).t as Int // expected-warning {{redundant cast to 'Int' has no effect}} {{27-34=}}
_ = SR11295D(t: 1 as Int).t as Int // expected-warning {{redundant cast to 'Int' has no effect}} {{29-36=}}
_ = SR11295D(t: 1).t as Int // Ok
_ = SR11295D(t: 1).t as UInt // Ok
_ = SR11295_E.a.rawValue as Int // expected-warning {{redundant cast to 'Int' has no effect}} {{26-33=}}
_ = SR11295D(t: 1 as Int).f() as Int // expected-warning {{redundant cast to 'Int' has no effect}} {{31-38=}}
_ = SR11295D(t: 1).f() as Int // Ok

// Overload decl expr
func f11295_Overload(a: Int, b: Int) -> Int { }
func f11295_Overload(c: Double, d: Double) -> Double { }

_ = (f11295_Overload as (Int, Int) -> Int)(0, 0)
_ = (f11295_Overload as (Double, Double) -> Double)(0, 0)

_ = (1 - 2 / 3 * 6) as UInt
_ = 1/4 as Float > 3/2 as Float // Ok
_ = 1/4 as Int > 3/2 as Int // Ok

// Special cases where the coerced expression type is inferred by context.

var f11295: (Float) -> Float = { $0 as Float } // expected-warning {{redundant cast to 'Float' has no effect}} {{37-46=}}
var f11295_1 = { $0 as Float } // Ok

func ff11295() -> (Int) -> Int {
  return { $0 as Int } // expected-warning {{redundant cast to 'Int' has no effect}} {{15-22=}}
}

func f11295t_2<A: Hashable, R>(f: @escaping ((A) -> R, A) -> R) {}

f11295t_2 { (f, n) in
  n < 2 ? n : 0 // Ok
}

f11295t_2 { (f, n) in
  n < 2 ? n as Int: 0 // Ok
}

f11295t_2 { (_, n) in
  _ = n as Int // Ok
}

f11295t_2 { (f, n: Int) in
  n < 2 ? n as Int : 0 // expected-warning {{redundant cast to 'Int' has no effect}} {{13-20=}}
}

f11295t_2 { (_, n) in
  _ = SR11295D(t: n as Int).t as Int // expected-warning {{redundant cast to 'Int' has no effect}} {{31-38=}}
}

f11295t_2 { (_, n) in
  _ = SR11295D(t: n).t as Int // Ok
}

func ff11295_g<T>(_ v: T) {
  let _ = { v as T } // expected-warning {{redundant cast to 'T' has no effect}} {{15-20=}}
} 

func ff11295_g1(_ v: Int) { 
  let _ = { v as Int } // expected-warning {{redundant cast to 'Int' has no effect}} {{15-22=}}
}

func ff11295_g2<T>(_ v: T) {
  let _ = { $0 as T } // Ok
} 

func ff11295_g2<T>(_ v: T) -> (T) -> T {
  let _ : (T) -> T = { $0 as T } // expected-warning {{redundant cast to 'T' has no effect}} {{27-32=}}
  return { $0 as T } // expected-warning {{redundant cast to 'T' has no effect}} {{15-20=}}
} 

func ff11295_g3(_ v: Int) { 
  let _ = { $0 as Int } // Ok
}

func ff11295_g4<T>(_ v: T) {
  let c = v as T // expected-warning {{redundant cast to 'T' has no effect}} {{13-18=}}
  let _ = { c as T } // expected-warning {{redundant cast to 'T' has no effect}} {{15-20=}}
}

func ff11295_g5<T>(_ v: T) {
  // Nested closures
  let _ = { { { $0 as T } } } // Ok
  let _ = { { { v as T } } } // expected-warning {{redundant cast to 'T' has no effect}} {{19-24=}}
}

func ff11295_6(_ i: Int) -> Int { i }
func ff11295_g6<T>(_ i: T) -> T { i }

let _ = { ff11295_g6($0 as Int) } // Ok

let _ = { ff11295_6($0) } // Ok

let _ = { ff11295_6($0 as Int) } // Ok

// A similar test case comming from validation-test/Sema/type_checker_perf/fast/rdar17077404.swift
func f11295_3<A: Hashable, R>(
  f: @escaping ((A) -> R, A) -> R
) -> ((A) -> R) {
  return { (a: A) -> R in
    let r: R! 
    return r
  }
}

_ = f11295_3 { 
  (f, n) in
  f(n as Int) as Int // OK
}

_ = f11295_3 {
  (f, n) in
  n < 2 ? n as Int : f(n - 1) + f(n - 2) // OK 
}
