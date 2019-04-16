// RUN: %target-typecheck-verify-swift

struct SimpleCallable {
  call func(_ x: Float) -> Float {
    return x
  }
}

// Simple test.

let foo = SimpleCallable()
_ = foo(1)
_ = foo(foo(1))

// Test direct `call` member references.

_ = { foo(1) }()
_ = [1, 2, 3].map { foo($0) }
let _: (Float) -> Float = { foo($0) }

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

struct MultipleArgsCallable {
  call func(x: Int, y: Float) -> [Int] {
    return [x]
  }
}

let bar = MultipleArgsCallable()
_ = bar(x: 1, y: 1)
_ = bar(x: bar(x: 1, y: 1)[0], y: 1)

struct Extended {}
extension Extended {
  @discardableResult
  call func() -> Extended {
    return self
  }
}
var extended = Extended()
extended()()()

struct OptionalCallable {
  call func() -> OptionalCallable? {
    return self
  }
}
var optional = OptionalCallable()
_ = optional()?()?()

struct VariadicCallable {
  call func(_ args: Int...) -> [Int] {
    return args
  }
}
var variadic = VariadicCallable()
_ = variadic()
_ = variadic(1, 2, 3)

struct Mutating {
  var x: Int
  mutating call func() {
    x += 1
  }
}
func testMutating(_ x: Mutating, _ y: inout Mutating) {
  _ = x() // expected-error {{cannot use mutating member on immutable value: 'x' is a 'let' constant}}
  _ = y()
}

struct Throwing {
  call func() throws -> Throwing {
    return self
  }
}
var throwing = Throwing()
_ = try throwing()

enum BinaryOperation {
  case add, subtract, multiply, divide
}
extension BinaryOperation {
  call func(_ lhs: Float, _ rhs: Float) -> Float {
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
  call func() -> Self {
    return self
  }
}
class SubClass : BaseClass {
  override call func() -> Self {
    return self
  }
}

func testIUO(a: SimpleCallable!, b: MultipleArgsCallable!, c: Extended!,
             d: OptionalCallable!, e: Throwing!) {
  _ = a(1)
  _ = b(x: 1, y: 1)
  _ = c()
  _ = d()?()?()
  _ = try? e()
}

// Errors.

// TODO: Fix this error. Ideally, it should be "extra argument in call".
// expected-error @+2 {{cannot call value of non-function type 'SimpleCallable'}}
// expected-error @+1 {{cannot invoke 'foo' with an argument list of type '(Int, Int)'}}
_ = foo(1, 1)

_ = bar(1, 1) // expected-error {{missing argument labels 'x:y:' in call}}
let _: (Float) -> Float = foo // expected-error {{cannot convert value of type 'SimpleCallable' to specified type '(Float) -> Float'}}
