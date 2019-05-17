// RUN: %target-typecheck-verify-swift

struct SimpleCallable {
  func callFunction(_ x: Float) -> Float {
    return x
  }
}

// Simple tests.

let foo = SimpleCallable()
_ = foo(1)
_ = foo(foo(1))

// TODO: Improve this error to match the error using a direct `call` member reference.
// expected-error @+2 {{cannot call value of non-function type 'SimpleCallable'}}
// expected-error @+1 {{cannot invoke 'foo' with an argument list of type '(Int, Int)'}}
_ = foo(1, 1)
// expected-error @+1 {{cannot convert value of type 'SimpleCallable' to specified type '(Float) -> Float'}}
let _: (Float) -> Float = foo

// Test direct `call` member references.

_ = foo.callFunction(1)
_ = [1, 2, 3].map(foo.callFunction)
_ = foo.callFunction(foo(1))
_ = foo(foo.callFunction(1))
let _: (Float) -> Float = foo.callFunction

func callable() -> SimpleCallable {
  return SimpleCallable()
}
extension SimpleCallable {
  var foo: SimpleCallable {
    return self
  }
  func bar() -> SimpleCallable {
    return self
  }
}

_ = foo.foo(1)
_ = foo.bar()(1)
_ = callable()(1)
_ = [1, 2, 3].map(foo.foo.callFunction)
_ = [1, 2, 3].map(foo.bar().callFunction)
_ = [1, 2, 3].map(callable().callFunction)

struct MultipleArgsCallable {
  func callFunction(x: Int, y: Float) -> [Int] {
    return [x]
  }
}

let bar = MultipleArgsCallable()
_ = bar(x: 1, y: 1)
_ = bar.callFunction(x: 1, y: 1)
_ = bar(x: bar.callFunction(x: 1, y: 1)[0], y: 1)
_ = bar.callFunction(x: bar(x: 1, y: 1)[0], y: 1)
_ = bar(1, 1) // expected-error {{missing argument labels 'x:y:' in call}}

struct Extended {}
extension Extended {
  @discardableResult
  func callFunction() -> Extended {
    return self
  }
}
var extended = Extended()
extended()().callFunction()()

struct TakesTrailingClosure {
  func callFunction(_ fn: () -> Void) {
    fn()
  }
  func callFunction(_ x: Int, label y: Float, _ fn: (Int, Float) -> Void) {
    fn(x, y)
  }
}
var takesTrailingClosure = TakesTrailingClosure()
takesTrailingClosure { print("Hi") }
takesTrailingClosure() { print("Hi") }
takesTrailingClosure(1, label: 2) { _ = Float($0) + $1 }

struct OptionalCallable {
  func callFunction() -> OptionalCallable? {
    return self
  }
}
var optional = OptionalCallable()
_ = optional()?.callFunction()?()

struct Variadic {
  func callFunction(_ args: Int...) -> [Int] {
    return args
  }
}
var variadic = Variadic()
_ = variadic()
_ = variadic(1, 2, 3)

struct Mutating {
  var x: Int
  mutating func callFunction() {
    x += 1
  }
}
func testMutating(_ x: Mutating, _ y: inout Mutating) {
  _ = x() // expected-error {{cannot use mutating member on immutable value: 'x' is a 'let' constant}}
  _ = x.callFunction() // expected-error {{cannot use mutating member on immutable value: 'x' is a 'let' constant}}
  _ = y()
  _ = y.callFunction()
}

struct Inout {
  func callFunction(_ x: inout Int) {
    x += 5
  }
}
func testInout(_ x: Inout, _ arg: inout Int) {
  x(&arg)
  x.callFunction(&arg)
  // TODO: Improve this error to match the error using a direct `call` member reference.
  // expected-error @+2 {{cannot invoke 'x' with an argument list of type '(Int)'}}
  // expected-error @+1 {{cannot call value of non-function type 'Inout'}}
  x(arg)
  // expected-error @+1 {{passing value of type 'Int' to an inout parameter requires explicit '&'}}
  x.callFunction(arg)
}

struct Autoclosure {
  func callFunction(_ condition: @autoclosure () -> Bool,
            _ message: @autoclosure () -> String) {
    if condition() {
      print(message())
    }
  }
}
func testAutoclosure(_ x: Autoclosure) {
  x(true, "Failure")
  x({ true }(), { "Failure" }())
}

struct Throwing {
  func callFunction() throws -> Throwing {
    return self
  }
  func callFunction(_ f: () throws -> ()) rethrows {
    try f()
  }
}
struct DummyError : Error {}
var throwing = Throwing()
_ = try throwing()
_ = try throwing { throw DummyError() }

enum BinaryOperation {
  case add, subtract, multiply, divide
}
extension BinaryOperation {
  func callFunction(_ lhs: Float, _ rhs: Float) -> Float {
    switch self {
    case .add: return lhs + rhs
    case .subtract: return lhs - rhs
    case .multiply: return lhs * rhs
    case .divide: return lhs / rhs
    }
  }
}
_ = BinaryOperation.add(1, 2)

class BaseClass {
  func callFunction() -> Self {
    return self
  }
}
class SubClass : BaseClass {
  override func callFunction() -> Self {
    return self
  }
}

func testIUO(a: SimpleCallable!, b: MultipleArgsCallable!, c: Extended!,
             d: OptionalCallable!, e: Variadic!, f: inout Mutating!,
             g: Inout!, inoutInt: inout Int, h: Throwing!) {
  _ = a(1)
  _ = b(x: 1, y: 1)
  _ = c()
  _ = d()?.callFunction()?()
  _ = e()
  _ = e(1, 2, 3)
  // FIXME(TF-444): `mutating func callFunction` and IUO doesn't work.
  // _ = f()
  _ = g(&inoutInt)
  _ = try? h()
  _ = try? h { throw DummyError() }
}
