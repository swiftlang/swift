// RUN: %target-typecheck-verify-swift

// Test constraint simplification of chains of binary operators.
// <https://bugs.swift.org/browse/SR-1122>
do {
  let a: String? = "a"
  let b: String? = "b"
  let c: String? = "c"
  let _: String? = a! + b! + c!

  let x: Double = 1
  _ = x + x + x

  let sr3483: Double? = 1
  _ = sr3483! + sr3483! + sr3483!

  let sr2636: [String: Double] = ["pizza": 10.99, "ice cream": 4.99, "salad": 7.99]
  _ = sr2636["pizza"]!
  _ = sr2636["pizza"]! + sr2636["salad"]!
  _ = sr2636["pizza"]! + sr2636["salad"]! + sr2636["ice cream"]!
}

// Use operators defined within a type.
struct S0 {
  static func +(lhs: S0, rhs: S0) -> S0 { return lhs }
}

func useS0(lhs: S0, rhs: S0) { 
  _ = lhs + rhs
}

// Use operators defined within a generic type.
struct S0b<T> {
  static func + <U>(lhs: S0b<T>, rhs: U) -> S0b<U> { return S0b<U>() }
}

func useS0b(s1i: S0b<Int>, s: String) {
  var s1s = s1i + s
  s1s = S0b<String>()
  _ = s1s
}

// Use operators defined within a protocol extension.
infix operator %%%
infix operator %%%%

protocol P1 {
  static func %%%(lhs: Self, rhs: Self) -> Bool
}

extension P1 {
  static func %%%%(lhs: Self, rhs: Self) -> Bool {
    return !(lhs %%% rhs)
  }
}

func useP1Directly<T : P1>(lhs: T, rhs: T) {
  _ = lhs %%% rhs
  _ = lhs %%%% rhs
}

struct S1 : P1 {
  static func %%%(lhs: S1, rhs: S1) -> Bool { return false }
}

func useP1Model(lhs: S1, rhs: S1) {
  _ = lhs %%% rhs
  _ = lhs %%%% rhs
}

struct S1b<T> : P1 {
  static func %%%(lhs: S1b<T>, rhs: S1b<T>) -> Bool { return false }
}

func useP1ModelB(lhs: S1b<Int>, rhs: S1b<Int>) {
  _ = lhs %%% rhs
  _ = lhs %%%% rhs
}

func useP1ModelBGeneric<T>(lhs: S1b<T>, rhs: S1b<T>) {
  _ = lhs %%% rhs
  _ = lhs %%%% rhs
}

// Use operators defined within a protocol extension to satisfy a requirement.
protocol P2 {
  static func %%%(lhs: Self, rhs: Self) -> Bool
  static func %%%%(lhs: Self, rhs: Self) -> Bool
}

extension P2 {
  static func %%%%(lhs: Self, rhs: Self) -> Bool {
    return !(lhs %%% rhs)
  }
}

func useP2Directly<T : P2>(lhs: T, rhs: T) {
  _ = lhs %%% rhs
  _ = lhs %%%% rhs
}

struct S2 : P2 {
  static func %%%(lhs: S2, rhs: S2) -> Bool { return false }
}

func useP2Model(lhs: S2, rhs: S2) {
  _ = lhs %%% rhs
  _ = lhs %%%% rhs
}

struct S2b<T> : P2 {
  static func %%%(lhs: S2b<T>, rhs: S2b<T>) -> Bool { return false }
}

func useP2Model2(lhs: S2b<Int>, rhs: S2b<Int>) {
  _ = lhs %%% rhs
  _ = lhs %%%% rhs
}

func useP2Model2Generic<T>(lhs: S2b<T>, rhs: S2b<T>) {
  _ = lhs %%% rhs
  _ = lhs %%%% rhs
}

// Using an extension of one protocol to satisfy another conformance.
protocol P3 { }

extension P3 {
  static func ==(lhs: Self, rhs: Self) -> Bool {
    return true
  }  
}

struct S3 : P3, Equatable { }

// rdar://problem/30220565
func shrinkTooFar(_ : Double, closure : ()->()) {}
func testShrinkTooFar() {
  shrinkTooFar(0*0*0) {}
}

// rdar://problem/33759839

enum E_33759839 {
    case foo
    case bar(String)
}

let foo_33759839 = ["a", "b", "c"]
let bar_33759839 = ["A", "B", "C"]

let _: [E_33759839] = foo_33759839.map { .bar($0) } +
                      bar_33759839.map { .bar($0) } +
                      [E_33759839.foo] // Ok

// rdar://problem/28688585

class B_28688585 {
  var value: Int

  init(value: Int) {
    self.value = value
  }

  func add(_ other: B_28688585) -> B_28688585 {
    return B_28688585(value: value + other.value)
  }
}

class D_28688585 : B_28688585 {
}

func + (lhs: B_28688585, rhs: B_28688585) -> B_28688585 {
  return lhs.add(rhs)
}

let var_28688585 = D_28688585(value: 1)
_ = var_28688585 + var_28688585 + var_28688585 // Ok

// rdar://problem/35740653 - Fix `LinkedExprAnalyzer` greedy operator linking

struct S_35740653 {
  var v: Double = 42

  static func value(_ value: Double) -> S_35740653 {
    return S_35740653(v: value)
  }

  static func / (lhs: S_35740653, rhs: S_35740653) -> Double {
     return lhs.v / rhs.v
  }
}

func rdar35740653(val: S_35740653) {
  let _ = 0...Int(val / .value(1.0 / 42.0)) // Ok
}

protocol P_37290898 {}
struct S_37290898: P_37290898 {}

func rdar37290898(_ arr: inout [P_37290898], _ element: S_37290898?) {
  arr += [element].compactMap { $0 } // Ok
}

// SR-8221
infix operator ??=
func ??= <T>(lhs: inout T?, rhs: T?) {}
var c: Int = 0 // expected-note {{change variable type to 'Int?' if it doesn't need to be declared as 'Int'}}
c ??= 5 // expected-error{{inout argument could be set to a value with a type other than 'Int'; use a value declared as type 'Int?' instead}}

func rdar46459603() {
  enum E {
  case foo(value: String)
  }

  let e = E.foo(value: "String")
  var arr = ["key": e]

  // FIXME(rdar://problem/64844584) - on iOS simulator this diagnostic is flaky,
  // either `referencing operator function '==' on 'Equatable'` or `operator function '==' requires`
  _ = arr.values == [e]
  // expected-error@-1 {{requires that 'Dictionary<String, E>.Values' conform to 'Equatable'}}
  // expected-error@-2 {{cannot convert value of type '[E]' to expected argument type 'Dictionary<String, E>.Values'}}
  _ = [arr.values] == [[e]]
  // expected-error@-1 {{requires that 'Dictionary<String, E>.Values' conform to 'Equatable'}}
  // expected-error@-2 {{cannot convert value of type '[E]' to expected element type 'Dictionary<String, E>.Values'}}
}

// SR-10843
infix operator ^^^
func ^^^ (lhs: String, rhs: String) {}

struct SR10843 {
  static func ^^^ (lhs: SR10843, rhs: SR10843) {}
}

func sr10843() {
  let s = SR10843()
  (^^^)(s, s)
  _ = (==)(0, 0)
}

// SR-10970
precedencegroup PowerPrecedence {
  lowerThan: BitwiseShiftPrecedence
  higherThan: AdditionPrecedence
  associativity: right
}
infix operator ^^ : PowerPrecedence

extension Int {
  static func ^^ (lhs: Int, rhs: Int) -> Int {
    var result = 1
    for _ in 1...rhs { result *= lhs }
    return result
  }
}

_ = 1 ^^ 2 ^^ 3 * 4 // expected-error {{adjacent operators are in unordered precedence groups 'PowerPrecedence' and 'MultiplicationPrecedence'}}

// rdar://problem/60185506 - Ambiguity with Float comparison
func rdar_60185506() {
  struct X {
    var foo: Float
  }

  func test(x: X?) {
    let _ = (x?.foo ?? 0) <= 0.5 // Ok
  }
}

// rdar://problem/60727310
func rdar60727310() {
  func myAssertion<T>(_ a: T, _ op: ((T,T)->Bool), _ b: T) {}
  var e: Error? = nil
  myAssertion(e, ==, nil) // expected-error {{binary operator '==' cannot be applied to two 'Error?' operands}}
}

// FIXME(SR-12438): Bad diagnostic.
func sr12438(_ e: Error) {
  func foo<T>(_ a: T, _ op: ((T, T) -> Bool)) {}
  foo(e, ==) // expected-error {{type of expression is ambiguous without more context}}
}

// rdar://problem/62054241 - Swift compiler crashes when passing < as the sort function in sorted(by:) and the type of the array is not comparable
func rdar_62054241() {
  struct Foo {
    let a: Int
  }

  func test(_ arr: [Foo]) -> [Foo] {
    return arr.sorted(by: <) // expected-error {{no exact matches in reference to operator function '<'}}
  }
}

// SR-11399 - Operator returning IUO doesn't implicitly unwrap
postfix operator ^^^
postfix func ^^^ (lhs: Int) -> Int! { 0 }

let x: Int = 1^^^
