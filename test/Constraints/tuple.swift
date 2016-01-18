// RUN: %target-parse-verify-swift

// Test various tuple constraints.

func f0(x x: Int, y: Float) {}

var i : Int
var j : Int
var f : Float

func f1(y y: Float, rest: Int...) {}

func f2(_: (x: Int, y: Int) -> Int) {}
func f2xy(x x: Int, y: Int) -> Int {}
func f2ab(a a: Int, b: Int) -> Int {}
func f2yx(y y: Int, x: Int) -> Int {}

func f3(x: (x: Int, y: Int) -> ()) {}
func f3a(x: Int, y: Int) {}
func f3b(_: Int) {}

func f4(rest: Int...) {}
func f5(x: (Int, Int)) {}

func f6(_: (i: Int, j: Int), k: Int = 15) {}

//===----------------------------------------------------------------------===//
// Conversions and shuffles
//===----------------------------------------------------------------------===//

// Variadic functions.
f4()
f4(1)
f4(1, 2, 3)

f2(f2xy)
f2(f2ab)
f2(f2yx) // expected-error{{cannot convert value of type '(y: Int, x: Int) -> Int' to expected argument type '(x: Int, y: Int) -> Int'}} 

f3(f3a)
f3(f3b) // expected-error{{cannot convert value of type '(Int) -> ()' to expected argument type '(x: Int, y: Int) -> ()'}} 

func getIntFloat() -> (int: Int, float: Float) {}
var values = getIntFloat()
func wantFloat(_: Float) {}
wantFloat(values.float)

var e : (x: Int..., y: Int) // expected-error{{cannot create a variadic tuple}}

typealias Interval = (a:Int, b:Int)
func takeInterval(x: Interval) {}
takeInterval(Interval(1, 2))

f5((1,1))

// Tuples with existentials
var any : Any = ()
any = (1, 2)
any = (label: 4)

// Scalars don't have .0/.1/etc
i = j.0 // expected-error{{value of type 'Int' has no member '0'}}
any.1 // expected-error{{value of type 'Any' (aka 'protocol<>') has no member '1'}}

// Fun with tuples
protocol PosixErrorReturn {
  static func errorReturnValue() -> Self
}

extension Int : PosixErrorReturn {
  static func errorReturnValue() -> Int { return -1 }
}

func posixCantFail<A, T : protocol<Comparable, PosixErrorReturn>>
  (f:(A) -> T)(args:A) -> T // expected-warning{{curried function declaration syntax will be removed in a future version of Swift}}
{
  let result = f(args)
  assert(result != T.errorReturnValue())
  return result
}

func open(name: String, oflag: Int) -> Int { }

var foo: Int = 0

var fd = posixCantFail(open)(args: ("foo", 0))

// Tuples and lvalues
class C {
  init() {}
  func f(_: C) {}
}

func testLValue(c: C) {
  var c = c
  c.f(c)
  
  let x = c
  c = x
}


// <rdar://problem/21444509> Crash in TypeChecker::coercePatternToType
func invalidPatternCrash(k : Int) {
  switch k {
  case (k, cph_: k) as UInt8:  // expected-error {{tuple pattern cannot match values of the non-tuple type 'UInt8'}} expected-warning {{cast from 'Int' to unrelated type 'UInt8' always fails}}
    break
  }
}

// <rdar://problem/21875219> Tuple to tuple conversion with IdentityExpr / AnyTryExpr hang
class Paws {
  init() throws {}
}

func scruff() -> (AnyObject?, ErrorType?) {
  do {
    return try (Paws(), nil)
  } catch {
    return (nil, error)
  }
}

// Test variadics with trailing closures.
func variadicWithTrailingClosure(x: Int..., y: Int = 2, fn: (Int, Int) -> Int) {
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
func gcd_23700031<T>(a: T, b: T) {
  var a = a
  var b = b
  (a, b) = (b, a % b)  // expected-error {{binary operator '%' cannot be applied to two 'T' operands}}
  // expected-note @-1 {{overloads for '%' exist with these partially matching parameter lists: (UInt8, UInt8), (Int8, Int8), (UInt16, UInt16), (Int16, Int16), (UInt32, UInt32), (Int32, Int32), (UInt64, UInt64), (Int64, Int64), (UInt, UInt), (Int, Int), (Float, Float)}}
}


