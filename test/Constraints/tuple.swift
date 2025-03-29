// RUN: %target-typecheck-verify-swift

// Test various tuple constraints.

func f0(x: Int, y: Float) {}

var i : Int
var j : Int
var f : Float

func f1(y: Float, rest: Int...) {}

func f2(_: (_ x: Int, _ y: Int) -> Int) {}
func f2xy(x: Int, y: Int) -> Int {}
func f2ab(a: Int, b: Int) -> Int {}
func f2yx(y: Int, x: Int) -> Int {}

func f3(_ x: (_ x: Int, _ y: Int) -> ()) {}
func f3a(_ x: Int, y: Int) {}
func f3b(_: Int) {}

func f4(_ rest: Int...) {}
func f5(_ x: (Int, Int)) {}

func f6(_: (i: Int, j: Int), k: Int = 15) {}

//===----------------------------------------------------------------------===//
// Conversions and shuffles
//===----------------------------------------------------------------------===//

func foo(a : [(some: Int, (key: Int, value: String))]) -> String {
  for (i , (j, k)) in a {
    if i == j { return k }
  }
}

func rdar28207648() -> [(Int, CustomStringConvertible)] {
  let v : [(Int, Int)] = []
  return v as [(Int, CustomStringConvertible)]
}

class rdar28207648Base {}
class rdar28207648Derived : rdar28207648Base {}

func rdar28207648(x: (Int, rdar28207648Derived)) -> (Int, rdar28207648Base) {
  return x as (Int, rdar28207648Base)
}

public typealias Success<T, V> = (response: T, data: V?)

public enum Result {
    case success(Success<Any, Any>)
    case error(Error)
}


let a = Success<Int, Int>(response: 3, data: 3)
let success: Result = .success(a)

// Variadic functions.
f4()
f4(1)
f4(1, 2, 3)

f2(f2xy)
f2(f2ab)
f2(f2yx)

f3(f3a)
f3(f3b) // expected-error{{cannot convert value of type '(Int) -> ()' to expected argument type '(Int, Int) -> ()'}} 

func getIntFloat() -> (int: Int, float: Float) {}
var values = getIntFloat()
func wantFloat(_: Float) {}
wantFloat(values.float)

var e : (x: Int..., y: Int) // expected-error{{variadic parameter cannot appear outside of a function parameter list}}

typealias Interval = (a:Int, b:Int)
func takeInterval(_ x: Interval) {}
takeInterval(Interval(1, 2))

f5((1,1))

// Tuples with existentials
var any : Any = ()
any = (1, 2)
any = (label: 4) // expected-error {{cannot create a single-element tuple with an element label}}

// Scalars don't have .0/.1/etc
i = j.0 // expected-error{{value of type 'Int' has no member '0'}}
any.1 // expected-error{{value of type 'Any' has no member '1'}}
// expected-note@-1{{cast 'Any' to 'AnyObject' or use 'as!' to force downcast to a more specific type to access members}}
any = (5.0, 6.0) as (Float, Float)
_ = (any as! (Float, Float)).1

// Fun with tuples
protocol PosixErrorReturn {
  static func errorReturnValue() -> Self
}

extension Int : PosixErrorReturn {
  static func errorReturnValue() -> Int { return -1 }
}

func posixCantFail<A, T : Comparable & PosixErrorReturn>
  (_ f: @escaping (A) -> T) -> (_ args:A) -> T
{
  return { args in
    let result = f(args)
    assert(result != T.errorReturnValue())
    return result
  }
}

func open(_ name: String, oflag: Int) -> Int { }

var foo: Int = 0

var fd = posixCantFail(open)(("foo", 0))

// Tuples and lvalues
class C {
  init() {}
  func f(_: C) {}
}

func testLValue(_ c: C) {
  var c = c
  c.f(c)

  let x = c
  c = x
}


// <rdar://problem/21444509> Crash in TypeChecker::coercePatternToType
func invalidPatternCrash(_ k : Int) {
  switch k {
  case (k, cph_: k) as UInt8:  // expected-error {{tuple pattern cannot match values of the non-tuple type 'UInt8'}} expected-warning {{cast from 'Int' to unrelated type 'UInt8' always fails}}
    break
  }
}

// <rdar://problem/21875219> Tuple to tuple conversion with IdentityExpr / AnyTryExpr hang
class Paws {
  init() throws {}
}

func scruff() -> (AnyObject?, Error?) {
  do {
    return try (Paws(), nil)
  } catch {
    return (nil, error)
  }
}

// Test variadics with trailing closures.
func variadicWithTrailingClosure(_ x: Int..., y: Int = 2, fn: (Int, Int) -> Int) {
}

variadicWithTrailingClosure(1, 2, 3) { $0 + $1 }
variadicWithTrailingClosure(1) { $0 + $1 }
variadicWithTrailingClosure() { $0 + $1 }
variadicWithTrailingClosure { $0 + $1 }

variadicWithTrailingClosure(1, 2, 3, y: 0) { $0 + $1 }
variadicWithTrailingClosure(1, y: 0) { $0 + $1 }
variadicWithTrailingClosure(y: 0) { $0 + $1 }

variadicWithTrailingClosure(1, 2, 3, y: 0, fn: +)
variadicWithTrailingClosure(1, y: 0, fn: +)
variadicWithTrailingClosure(y: 0, fn: +)

variadicWithTrailingClosure(1, 2, 3, fn: +)
variadicWithTrailingClosure(1, fn: +)
variadicWithTrailingClosure(fn: +)


// <rdar://problem/23700031> QoI: Terrible diagnostic in tuple assignment
func gcd_23700031<T>(_ a: T, b: T) {
  var a = a
  var b = b
  (a, b) = (b, a % b)  // expected-error {{binary operator '%' cannot be applied to two 'T' operands}}
}

// <rdar://problem/24210190>
//   Don't ignore tuple labels in same-type constraints or stronger.
protocol Kingdom {
  associatedtype King
}
struct Victory<General> {
  init<K: Kingdom>(_ king: K) where K.King == General {} // expected-note {{where 'General' = '(x: Int, y: Int)', 'K.King' = 'MagicKingdom<(Int, Int)>.King' (aka '(Int, Int)')}}
}
struct MagicKingdom<K> : Kingdom {
  typealias King = K
}
func magnify<T>(_ t: T) -> MagicKingdom<T> { return MagicKingdom() }
func foo(_ pair: (Int, Int)) -> Victory<(x: Int, y: Int)> {
  return Victory(magnify(pair)) // expected-error {{initializer 'init(_:)' requires the types '(x: Int, y: Int)' and 'MagicKingdom<(Int, Int)>.King' (aka '(Int, Int)') be equivalent}}
}


// https://github.com/apple/swift/issues/43213
// Compiler crashes when accessing a non-existent property of a closure
// parameter
func call(_ f: (C) -> Void) {}
func makeRequest() {
  call { obj in
    print(obj.invalidProperty)  // expected-error {{value of type 'C' has no member 'invalidProperty'}}
  }
}

// <rdar://problem/25271859> QoI: Misleading error message when expression result can't be inferred from closure
struct r25271859<T> {
}

extension r25271859 {
  func map<U>(f: (T) -> U) -> r25271859<U> {
  }

  func andThen<U>(f: (T) -> r25271859<U>) {
  }
}

func f(a : r25271859<(Float, Int)>) {
  a.map { $0.0 }
    .andThen { _ in
      print("hello")
      return r25271859<String>()
  }
}

// LValue to rvalue conversions.

func takesRValue(_: (Int, (Int, Int))) {}
func takesAny(_: Any) {}

var x = 0
var y = 0

let _ = (x, (y, 0))
takesRValue((x, (y, 0)))
takesAny((x, (y, 0)))

// https://github.com/apple/swift/issues/45205
// Closure cannot infer tuple parameter names

typealias Closure<A, B> = ((a: A, b: B)) -> String

func invoke<A, B>(a: A, b: B, _ closure: Closure<A,B>) {
  print(closure((a, b)))
}

invoke(a: 1, b: "B") { $0.b }

invoke(a: 1, b: "B") { $0.1 }

invoke(a: 1, b: "B") { (c: (a: Int, b: String)) in
  return c.b
}

invoke(a: 1, b: "B") { c in
  return c.b
}

// Crash with one-element tuple with labeled element
class Dinner {}

func microwave() -> Dinner? {
  let d: Dinner? = nil
  return (n: d) // expected-error{{cannot convert return expression of type '(n: Dinner?)' to return type 'Dinner'}}
}

func microwave() -> Dinner {
  let d: Dinner? = nil
  return (n: d) // expected-error{{cannot convert return expression of type '(n: Dinner?)' to return type 'Dinner'}}
}

// Tuple conversion with an optional
func f(b: Bool) -> (a: Int, b: String)? {
  let x = 3
  let y = ""
  return b ? (x, y) : nil
}

// Single element tuple expressions
func singleElementTuple() {
  let _ = (label: 123) // expected-error {{cannot create a single-element tuple with an element label}} {{12-19=}}
  let _ = (label: 123).label // expected-error {{cannot create a single-element tuple with an element label}} {{12-19=}}
  let _ = ((label: 123)) // expected-error {{cannot create a single-element tuple with an element label}} {{13-20=}}
  let _ = ((label: 123)).label // expected-error {{cannot create a single-element tuple with an element label}} {{13-20=}}
}

// Tuples with duplicate labels

let dupLabel1: (foo: Int, foo: Int) = (foo: 1, foo: 2) // expected-error 2{{cannot create a tuple with a duplicate element label}}

func dupLabel2(x a: Int, x b: Int) -> (y: Int, y: Int) { // expected-error {{cannot create a tuple with a duplicate element label}}
  return (a, b)
}

let _ = (bar: 0, bar: "") // expected-error {{cannot create a tuple with a duplicate element label}}

let zeroTuple = (0,0)

if case (foo: let x, foo: let y) = zeroTuple { print(x+y) } // expected-error {{cannot create a tuple with a duplicate element label}} 
// expected-warning@-1 {{'if' condition is always true}}

enum BishBash { case bar(foo: Int, foo: String) }
// expected-error@-1 {{invalid redeclaration of 'foo'}}
// expected-note@-2 {{'foo' previously declared here}}
let enumLabelDup: BishBash = .bar(foo: 0, foo: "") // expected-error {{cannot create a tuple with a duplicate element label}}

func dupLabelClosure(_ fn: () -> Void) {}
dupLabelClosure { print((bar: "", bar: 5).bar) } // expected-error {{cannot create a tuple with a duplicate element label}}

struct DupLabelSubscript {
  subscript(foo x: Int, foo y: Int) -> Int {
    return 0
  }
}

let dupLabelSubscriptStruct = DupLabelSubscript()
let _ = dupLabelSubscriptStruct[foo: 5, foo: 5] // ok

// https://github.com/apple/swift/issues/55316

var dict: [String: (Int, Int)] = [:]
let bignum: Int64 = 1337
dict["test"] = (bignum, 1) // expected-error {{cannot assign value of type '(Int64, Int)' to subscript of type '(Int, Int)'}}

var tuple: (Int, Int)
tuple = (bignum, 1) // expected-error {{cannot assign value of type '(Int64, Int)' to type '(Int, Int)'}}

var optionalTuple: (Int, Int)?
var optionalTuple2: (Int64, Int)? = (bignum, 1) 
var optionalTuple3: (UInt64, Int)? = (bignum, 1) // expected-error {{cannot convert value of type '(Int64, Int)' to specified type '(UInt64, Int)'}}

optionalTuple = (bignum, 1) // expected-error {{cannot assign value of type '(Int64, Int)' to type '(Int, Int)'}}
// Optional to Optional
optionalTuple = optionalTuple2 // expected-error {{cannot assign value of type '(Int64, Int)?' to type '(Int, Int)?'}}

func testTupleLabelMismatchFuncConversion(fn1: @escaping ((x: Int, y: Int)) -> Void,
                                          fn2: @escaping () -> (x: Int, Int)) {
  // Warn on mismatches
  let _: ((a: Int, b: Int)) -> Void = fn1 // expected-warning {{tuple conversion from '(a: Int, b: Int)' to '(x: Int, y: Int)' mismatches labels}}
  let _: ((x: Int, b: Int)) -> Void = fn1 // expected-warning {{tuple conversion from '(x: Int, b: Int)' to '(x: Int, y: Int)' mismatches labels}}

  let _: () -> (y: Int, Int) = fn2 // expected-warning {{tuple conversion from '(x: Int, Int)' to '(y: Int, Int)' mismatches labels}}
  let _: () -> (y: Int, k: Int) = fn2 // expected-warning {{tuple conversion from '(x: Int, Int)' to '(y: Int, k: Int)' mismatches labels}}

  // Attempting to shuffle has always been illegal here
  let _: () -> (y: Int, x: Int) = fn2 // expected-error {{cannot convert value of type '() -> (x: Int, Int)' to specified type '() -> (y: Int, x: Int)'}}

  // Losing labels is okay though.
  let _: () -> (Int, Int) = fn2

  // Gaining labels also okay.
  let _: ((x: Int, Int)) -> Void = fn1
  let _: () -> (x: Int, y: Int) = fn2
  let _: () -> (Int, y: Int) = fn2
}

func testTupleLabelMismatchKeyPath() {
  // FIXME: The warning should be upgraded to an error for key paths.
  let _: KeyPath<(x: Int, y: Int), Int> = \(a: Int, b: Int).x
  // expected-warning@-1 {{tuple conversion from '(a: Int, b: Int)' to '(x: Int, y: Int)' mismatches labels}}
}
