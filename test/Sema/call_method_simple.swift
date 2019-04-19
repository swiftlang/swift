// RUN: %target-typecheck-verify-swift

struct SimpleCallable {
  func call(_ x: Float) -> Float {
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

_ = foo.call(1)
_ = [1, 2, 3].map(foo.call)
_ = foo.call(foo(1))
_ = foo(foo.call(1))
let _: (Float) -> Float = foo.call

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
_ = [1, 2, 3].map(foo.foo.call)
_ = [1, 2, 3].map(foo.bar().call)
_ = [1, 2, 3].map(callable().call)

struct MultipleArgsCallable {
  func call(x: Int, y: Float) -> [Int] {
    return [x]
  }
}

let bar = MultipleArgsCallable()
_ = bar(x: 1, y: 1)
_ = bar.call(x: 1, y: 1)
_ = bar(x: bar.call(x: 1, y: 1)[0], y: 1)
_ = bar.call(x: bar(x: 1, y: 1)[0], y: 1)
_ = bar(1, 1) // expected-error {{missing argument labels 'x:y:' in call}}

struct Extended {}
extension Extended {
  @discardableResult
  func call() -> Extended {
    return self
  }
}
var extended = Extended()
extended()().call()()

struct OptionalCallable {
  func call() -> OptionalCallable? {
    return self
  }
}
var optional = OptionalCallable()
_ = optional()?.call()?()

struct Variadic {
  func call(_ args: Int...) -> [Int] {
    return args
  }
}
var variadic = Variadic()
_ = variadic()
_ = variadic(1, 2, 3)

struct Mutating {
  var x: Int
  mutating func call() {
    x += 1
  }
}
func testMutating(_ x: Mutating, _ y: inout Mutating) {
  _ = x() // expected-error {{cannot use mutating member on immutable value: 'x' is a 'let' constant}}
  _ = x.call() // expected-error {{cannot use mutating member on immutable value: 'x' is a 'let' constant}}
  _ = y()
  _ = y.call()
}

struct Inout {
  func call(_ x: inout Int) {
    x += 5
  }
}
func testInout(_ x: Inout, _ arg: inout Int) {
  x(&arg)
  x.call(&arg)
  // TODO: Improve this error to match the error using a direct `call` member reference.
  // expected-error @+2 {{cannot invoke 'x' with an argument list of type '(Int)'}}
  // expected-error @+1 {{cannot call value of non-function type 'Inout'}}
  x(arg)
  // expected-error @+1 {{passing value of type 'Int' to an inout parameter requires explicit '&'}}
  x.call(arg)
}

struct Autoclosure {
  func call(_ condition: @autoclosure () -> Bool,
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
  func call() throws -> Throwing {
    return self
  }
  func call(_ f: () throws -> ()) rethrows {
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
  func call(_ lhs: Float, _ rhs: Float) -> Float {
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
  func call() -> Self {
    return self
  }
}
class SubClass : BaseClass {
  override func call() -> Self {
    return self
  }
}

func testIUO(a: SimpleCallable!, b: MultipleArgsCallable!, c: Extended!,
             d: OptionalCallable!, e: Variadic!, f: inout Mutating!,
             g: Inout!, inoutInt: inout Int, h: Throwing!) {
  _ = a(1)
  _ = b(x: 1, y: 1)
  _ = c()
  _ = d()?.call()?()
  _ = e()
  _ = e(1, 2, 3)
  // FIXME(TF-444): `mutating func call` and IUO doesn't work.
  // _ = f()
  _ = g(&inoutInt)
  _ = try? h()
  _ = try? h { throw DummyError() }
}
