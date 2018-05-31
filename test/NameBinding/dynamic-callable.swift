// RUN: %target-swift-frontend -typecheck -verify %s
var global = 42

@dynamicCallable
struct Callable {
  func dynamicallyCall(withArguments arguments: [Int]) -> Int {
    return arguments.count
  }
}

@dynamicCallable
struct DiscardableResult {
  @discardableResult
  func dynamicallyCall(withArguments arguments: [Double]) -> Int {
    return arguments.count
  }
}

@dynamicCallable
struct Throwing {
  func dynamicallyCall(withArguments arguments: [String]) throws -> Int {
    return arguments.count
  }
}

@dynamicCallable
struct KeywordArgumentCallable {
  @discardableResult
  func dynamicallyCall(
    withKeywordArguments arguments: DictionaryLiteral<String, Float>
  ) -> Int {
    return arguments.count
  }
}

func testCallable(
  a: Callable, b: DiscardableResult, c: Throwing, d: KeywordArgumentCallable
) {
  _ = a()
  let a1 = a(1, 2, 3, 4) // expected-warning {{initialization of immutable value 'a1' was never used}}

  b()
  b(1, 2, 3, 4.0)

  _ = try? c()
  let c1 = try! c("hello", "world") // expected-warning {{initialization of immutable value 'c1' was never used}}

  d()
  d(1, 2.0, 3)
  d(x1: 1, 2.0, x2: 3)
}

func testIUO(
  a: Callable!, b: DiscardableResult!, c: Throwing!, d: KeywordArgumentCallable!
) {
  print(a(1, 2, 3))
  print(b(1, 2, 3.0))
  print(try! c("hello", "world"))
  print(d(foo: 1, 2.0, bar: 3))
}

//===----------------------------------------------------------------------===//
// Returning a function
//===----------------------------------------------------------------------===//

@dynamicCallable
struct CallableReturningFunction {
  func dynamicallyCall(withArguments arguments: [Int]) -> (_ a: Int) -> Void {
    return { a in () }
  }
}

func testFunction(a: CallableReturningFunction) {
  a(1, 2, 3)(1)
}

//===----------------------------------------------------------------------===//
// Error cases
//===----------------------------------------------------------------------===//

// Arguments' type may not be variadic.
// expected-error @+1 {{@dynamicCallable attribute requires 'Invalid1' to have either a valid, non-generic 'dynamicallyCall(withArguments:)' method or 'dynamicallyCall(withKeywordArguments:)' method}}
@dynamicCallable
struct Invalid1 {
  func dynamicallyCall(withArguments arguments: [Int]...) -> Int {
    return 1
  }
}

// Keyword arguments' key type must be ExpressibleByStringLiteral.
// expected-error @+1 {{@dynamicCallable attribute requires 'Invalid2' to have either a valid, non-generic 'dynamicallyCall(withArguments:)' method or 'dynamicallyCall(withKeywordArguments:)' method}}
@dynamicCallable
struct Invalid2 {
  func dynamicallyCall(
    withKeywordArguments arguments: DictionaryLiteral<Int, Int>
  ) -> Int {
    return 1
  }
}

// Dynamic calls with keyword arguments require `dynamicallyCall(withKeywordArguments)` to be defined.
@dynamicCallable
// expected-error @+1 {{@dynamicCallable type 'Invalid3' cannot be applied with keyword arguments; missing `dynamicCall(withKeywordArguments:)` method}}
class Invalid3 {
  func dynamicallyCall(withArguments arguments: [Int]) -> Int { return 1 }
}
@dynamicCallable
// expected-error @+1 {{@dynamicCallable type 'InvalidProtocol & AnyObject' cannot be applied with keyword arguments; missing `dynamicCall(withKeywordArguments:)` method}}
protocol InvalidProtocol {}
extension InvalidProtocol {
  func dynamicallyCall(withArguments arguments: [Int]) -> Int { return 1 }
}
func testInvalidKeywordCall(x: Invalid3, y: InvalidProtocol & AnyObject) {
  x(a: 1, b: 2) // expected-error {{cannot invoke 'x' with an argument list of type '(a: Int, b: Int)'}}
  y(a: 1, b: 2) // expected-error {{cannot invoke 'y' with an argument list of type '(a: Int, b: Int)'}}
}

// References to overloads are currently not supported.
// Eventually, they may be contextually disambiguated.
// expected-error @+1 {{@dynamicCallable attribute requires 'Ambiguity' to have either a valid, non-generic 'dynamicallyCall(withArguments:)' method or 'dynamicallyCall(withKeywordArguments:)' method}}
@dynamicCallable
struct Ambiguity {
  // expected-error @+1 {{@dynamicCallable attribute does not support ambiguous method 'dynamicallyCall(withArguments:)'}}
  func dynamicallyCall(withArguments arguments: [Int]) -> Int {
    return 1
  }
  func dynamicallyCall(withArguments arguments: [Int]) -> Float {
    return 1.0
  }
}

// expected-error @+1 {{'@dynamicCallable' attribute cannot be applied to this declaration}}
@dynamicCallable
extension Int {
  func dynamicallyCall(withArguments arguments: [Int]) -> Int {
    return 1
  }
}

// expected-error @+1 {{'@dynamicCallable' attribute cannot be applied to this declaration}}
@dynamicCallable
func NotAllowedOnFunc() {}

// @dynamicCallable cannot be declared on a base class and fulfilled with a
// derived class.

// expected-error @+1 {{@dynamicCallable attribute requires 'InvalidBase' to have either a valid, non-generic 'dynamicallyCall(withArguments:)' method or 'dynamicallyCall(withKeywordArguments:)' method}}
@dynamicCallable
class InvalidBase {}

class InvalidDerived : InvalidBase {
  func dynamicallyCall(withArguments arguments: [Int]) -> Int {
    return 1
  }
}

//===----------------------------------------------------------------------===//
// Existential tests
//===----------------------------------------------------------------------===//

@dynamicCallable
protocol CallableProtocol {}
extension CallableProtocol {
  func dynamicallyCall(withArguments arguments: [Int]) -> Int {
    return arguments.count
  }
}

@dynamicCallable
protocol KeywordCallableProtocol {}
extension KeywordCallableProtocol {
  func dynamicallyCall(
    withKeywordArguments arguments: DictionaryLiteral<String, Double>
  ) -> Int {
    return arguments.count
  }
}

extension String : CallableProtocol, KeywordCallableProtocol {}

func testProtoExtension() -> Int {
  let str = "test"
  return str(1, 2, 3) + str(label1: 1, 2, label2: 3)
}

struct CallableStruct : CallableProtocol {}

func testExistential(
  a: CallableProtocol, b: KeywordCallableProtocol, c: CallableStruct
) -> Int {
  return a(1, 2, 3) + b(label1: 1, 2, label2: 3) + c(1, 2, 3)
}

// Verify protocol compositions and refinements work.
protocol SubProtocol : CallableProtocol {}

typealias ProtocolComp = AnyObject & CallableProtocol

func testExistential2(a: AnyObject & CallableProtocol,
                      b: SubProtocol,
                      c: ProtocolComp & AnyObject,
                      d: CallableProtocol & KeywordCallableProtocol) {
  print(a(1, 2, 3))
  print(b(1, 2, 3))
  print(c(1, 2, 3))
  print(d() + d(label1: 1, 2, label2: 3))
}

//===----------------------------------------------------------------------===//
// Class inheritance test
//===----------------------------------------------------------------------===//

@dynamicCallable
class BaseClass {
  func dynamicallyCall(withArguments arguments: [Int]) -> Int {
    return arguments.count
  }
}
class DerivedClass1 : BaseClass {}

class DerivedClass2 : BaseClass {
  override func dynamicallyCall(withArguments arguments: [Int]) -> Int {
    return arguments.count
  }
}

class DerivedClass3 : BaseClass {
  func dynamicallyCall(
    withKeywordArguments arguments: DictionaryLiteral<String, Int>
  ) -> Int {
    return arguments.count
  }
}

func testDerivedClass(
  a: BaseClass, b: DerivedClass1, c: DerivedClass2, d: DerivedClass3
) -> Int {
  return a() - b(1, 2) + c(3, 4) - d(x1: 5, 6, x2: 7)
}

//===----------------------------------------------------------------------===//
// Enum test
//===----------------------------------------------------------------------===//

@dynamicCallable
enum BinaryOperation<T : Numeric> {
  case add
  case subtract
  case multiply

  func dynamicallyCall(withArguments arguments: [T]) -> T {
    precondition(arguments.count == 2, "Must have 2 arguments")
    let x = arguments[0]
    let y = arguments[1]
    switch self {
    case .add:
      return x + y
    case .subtract:
      return x - y
    case .multiply:
      return x * y
    }
  }
}

func testEnum() {
  let ops: [BinaryOperation<Int>] = [.add, .subtract, .multiply]
  for op in ops {
    print(op(3, 4))
  }
}

//===----------------------------------------------------------------------===//
// Generics
//===----------------------------------------------------------------------===//

@dynamicCallable
struct CallableGenericArray<A : ExpressibleByArrayLiteral> {
  func dynamicallyCall(withArguments arguments: A) -> Int {
    return 1
  }
}
func testGenericArray<A : ExpressibleByArrayLiteral>(
  a: CallableGenericArray<A>, x: A.ArrayLiteralElement
) -> Int {
  return a() + a(x, x)
}

@dynamicCallable
struct CallableGenericDictionary<D : ExpressibleByDictionaryLiteral>
  where D.Key : ExpressibleByStringLiteral {
  func dynamicallyCall(withKeywordArguments arguments: D) -> Int {
    return 1
  }
}
func testGenericDictionary<D : ExpressibleByDictionaryLiteral>(
  a: CallableGenericDictionary<D>, x: D.Value
) -> Int where D.Key : ExpressibleByStringLiteral {
  return a() + a(label1: x, x, label2: x)
}

@dynamicCallable
struct CallableGeneric1<T> {
  func dynamicallyCall(withArguments arguments: [T]) -> Int {
    return arguments.count
  }
}
func testGenericType1<T>(a: CallableGeneric1<T>, x: T) -> Int {
  return a() + a(x, x)
}
func testConcreteGenericType2(a: CallableGeneric1<Int>) -> Int {
  return a() + a(1, 2)
}

@dynamicCallable
struct CallableGeneric2<T> {
  func dynamicallyCall(withArguments arguments: [Any]) -> Int {
    return arguments.count
  }
}
func testGenericType2<T>(a: CallableGeneric2<T>) -> Int {
  return a(1, 2) + a("asdf", 123)
}
func testConcreteGenericType2(a: CallableGeneric2<Int>) -> Int {
  return a(1, 2) + a("asdf", 123)
}

@dynamicCallable
struct CallableGeneric3<T> {
  func dynamicallyCall(
    withKeywordArguments arguments: DictionaryLiteral<String, T>
  ) -> Int {
    return arguments.count
  }
}
func testGenericType3<T>(a: CallableGeneric3<T>, x: T) -> Int {
  return a() + a(x1: x, x, x, x2: x)
}
func testConcreteGenericType3(a: CallableGeneric3<Int>) -> Int {
  return a() + a(x1: 123, 1, 2, x2: 123)
}

@dynamicCallable
struct CallableGeneric4<T> {
  func dynamicallyCall<U>(withArguments arguments: [U]) -> Int {
    return arguments.count
  }

  func dynamicallyCall<U>(
    withKeywordArguments arguments: DictionaryLiteral<StaticString, U>
  ) -> Int {
    return arguments.count
  }
}
func testGenericType4<T>(a: CallableGeneric4<T>) -> Int {
  return a() + a(1, 2, 3) + a(x1: 1, 2, x3: 3)
}

@dynamicCallable
struct CallableGeneric5<T> {
  func dynamicallyCall<U>(withArguments arguments: [U]) -> U {
    return arguments[0]
  }

  func dynamicallyCall<U>(
    withKeywordArguments arguments: DictionaryLiteral<StaticString, U>
  ) -> U {
    return arguments[0].1
  }
}
func testGenericType5<T>(a: CallableGeneric5<T>) -> Double {
  return a(1, 2, 3) + a(x1: 1, 2, x3: 3)
}
