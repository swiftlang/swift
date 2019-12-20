// RUN: %target-typecheck-verify-swift

struct SimpleCallable {
  func callAsFunction(_ x: Float) -> Float {
    return x
  }
}

// Simple tests.

let foo = SimpleCallable()
_ = foo(1)
_ = foo(foo(1))
_ = foo(1, 1) // expected-error@:12 {{extra argument in call}}
// expected-error @+1 {{cannot convert value of type 'SimpleCallable' to specified type '(Float) -> Float'}}
let _: (Float) -> Float = foo

// Test direct `callAsFunction` member references.

_ = foo.callAsFunction(1)
_ = [1, 2, 3].map(foo.callAsFunction)
_ = foo.callAsFunction(foo(1))
_ = foo(foo.callAsFunction(1))
let _: (Float) -> Float = foo.callAsFunction

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
_ = [1, 2, 3].map(foo.foo.callAsFunction)
_ = [1, 2, 3].map(foo.bar().callAsFunction)
_ = [1, 2, 3].map(callable().callAsFunction)

struct MultipleArgsCallable {
  func callAsFunction(x: Int, y: Float) -> [Int] {
    return [x]
  }
}

let bar = MultipleArgsCallable()
_ = bar(x: 1, y: 1)
_ = bar.callAsFunction(x: 1, y: 1)
_ = bar(x: bar.callAsFunction(x: 1, y: 1)[0], y: 1)
_ = bar.callAsFunction(x: bar(x: 1, y: 1)[0], y: 1)
_ = bar(1, 1) // expected-error {{missing argument labels 'x:y:' in call}}

struct Extended {}
extension Extended {
  @discardableResult
  func callAsFunction() -> Extended {
    return self
  }
}
var extended = Extended()
extended()().callAsFunction()()

struct TakesTrailingClosure {
  func callAsFunction(_ fn: () -> Void) {
    fn()
  }
  func callAsFunction(_ x: Int, label y: Float, _ fn: (Int, Float) -> Void) {
    fn(x, y)
  }
}
var takesTrailingClosure = TakesTrailingClosure()
takesTrailingClosure { print("Hi") }
takesTrailingClosure() { print("Hi") }
takesTrailingClosure(1, label: 2) { _ = Float($0) + $1 }

struct OptionalCallable {
  func callAsFunction() -> OptionalCallable? {
    return self
  }
}
var optional = OptionalCallable()
_ = optional()?.callAsFunction()?()

struct Variadic {
  func callAsFunction(_ args: Int...) -> [Int] {
    return args
  }
}
var variadic = Variadic()
_ = variadic()
_ = variadic(1, 2, 3)

struct Mutating {
  var x: Int
  mutating func callAsFunction() {
    x += 1
  }
}
func testMutating(_ x: Mutating, _ y: inout Mutating) {
  // expected-error @+1 {{cannot use mutating member on immutable value: 'x' is a 'let' constant}}
  _ = x()
  // expected-error @+1 {{cannot use mutating member on immutable value: 'x' is a 'let' constant}}
  _ = x.callAsFunction()
  _ = y()
  _ = y.callAsFunction()
}

struct Inout {
  func callAsFunction(_ x: inout Int) {
    x += 5
  }
}
func testInout(_ x: Inout, _ arg: inout Int) {
  x(&arg)
  x.callAsFunction(&arg)
  // expected-error @+1 {{passing value of type 'Int' to an inout parameter requires explicit '&'}}
  x(arg)
  // expected-error @+1 {{passing value of type 'Int' to an inout parameter requires explicit '&'}}
  x.callAsFunction(arg)
}

struct Autoclosure {
  func callAsFunction(_ condition: @autoclosure () -> Bool,
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
  func callAsFunction() throws -> Throwing {
    return self
  }
  func callAsFunction(_ f: () throws -> ()) rethrows {
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
  func callAsFunction(_ lhs: Float, _ rhs: Float) -> Float {
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
  func callAsFunction() -> Self {
    return self
  }
}
class SubClass : BaseClass {
  override func callAsFunction() -> Self {
    return self
  }
}

func testIUO(a: SimpleCallable!, b: MultipleArgsCallable!, c: Extended!,
             d: OptionalCallable!, e: Variadic!, f: inout Mutating!,
             g: Inout!, inoutInt: inout Int, h: Throwing!) {
  _ = a(1)
  _ = b(x: 1, y: 1)
  _ = c()
  _ = d()?.callAsFunction()?()
  _ = e()
  _ = e(1, 2, 3)
  _ = f()
  _ = g(&inoutInt)
  _ = try? h()
  _ = try? h { throw DummyError() }
}

// SR-11778
struct DoubleANumber {
  func callAsFunction(_ x: Int, completion: (Int) -> Void = { _ in }) {
    completion(x + x)
  }
}

func testDefaults(_ x: DoubleANumber) {
  x(5)
  x(5, completion: { _ in })
}

// SR-11881
struct IUOCallable {
  static var callable: IUOCallable { IUOCallable() }
  func callAsFunction(_ x: Int) -> IUOCallable! { nil }
}

func testIUOCallAsFunction(_ x: IUOCallable) {
  let _: IUOCallable = x(5)
  let _: IUOCallable? = x(5)
  let _ = x(5)

  let _: IUOCallable = .callable(5)
  let _: IUOCallable? = .callable(5)
}

// Test access control.
struct PrivateCallable {
  private func callAsFunction(_ x: Int) {} // expected-note {{'callAsFunction' declared here}}
}

func testAccessControl(_ x: PrivateCallable) {
  x(5) // expected-error {{'callAsFunction' is inaccessible due to 'private' protection level}}
}

struct SR_11909 {
  static let s = SR_11909()
  func callAsFunction(_ x: Int = 0) -> SR_11909 { SR_11909() }
}

func testDefaultsWithUMEs(_ x: SR_11909) {
  let _: SR_11909 = .s()
  let _: SR_11909 = .s(5)
}
