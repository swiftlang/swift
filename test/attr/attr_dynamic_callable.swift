// RUN: %target-swift-frontend -typecheck -verify %s

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
    withKeywordArguments arguments: KeyValuePairs<String, Float>
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
// expected-error @+1 {{@dynamicCallable attribute requires 'Invalid1' to have either a valid 'dynamicallyCall(withArguments:)' method or 'dynamicallyCall(withKeywordArguments:)' method}}
@dynamicCallable
struct Invalid1 {
  func dynamicallyCall(withArguments arguments: [Int]...) -> Int {
    return 1
  }
}

// Keyword arguments' key type must be ExpressibleByStringLiteral.
// expected-error @+1 {{@dynamicCallable attribute requires 'Invalid2' to have either a valid 'dynamicallyCall(withArguments:)' method or 'dynamicallyCall(withKeywordArguments:)' method}}
@dynamicCallable
struct Invalid2 {
  func dynamicallyCall(
    withKeywordArguments arguments: KeyValuePairs<Int, Int>
  ) -> Int {
    return 1
  }
}

// Dynamic calls with keyword arguments require `dynamicallyCall(withKeywordArguments:)` to be defined.
@dynamicCallable
class NoKeyword {
  func dynamicallyCall(withArguments arguments: [Int]) -> Int { return 1 }
}
@dynamicCallable
protocol NoKeywordProtocol {
  func dynamicallyCall(withArguments arguments: [Int]) -> Int
}

func testInvalidKeywordCall(x: NoKeyword, y: NoKeywordProtocol & AnyObject) {
  x(a: 1, b: 2) // expected-error {{@dynamicCallable type 'NoKeyword' cannot be applied with keyword arguments; missing 'dynamicCall(withKeywordArguments:)' method}}
  y(a: 1, b: 2) // expected-error {{@dynamicCallable type 'NoKeywordProtocol & AnyObject' cannot be applied with keyword arguments; missing 'dynamicCall(withKeywordArguments:)' method}}
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

// expected-error @+1 {{@dynamicCallable attribute requires 'InvalidBase' to have either a valid 'dynamicallyCall(withArguments:)' method or 'dynamicallyCall(withKeywordArguments:)' method}}
@dynamicCallable
class InvalidBase {}

class InvalidDerived : InvalidBase {
  func dynamicallyCall(withArguments arguments: [Int]) -> Int {
    return 1
  }
}

//===----------------------------------------------------------------------===//
// Multiple `dynamicallyCall` method tests
//===----------------------------------------------------------------------===//

@dynamicCallable
struct OverloadedCallable {
  // expected-note @+1 {{found this candidate}}
  func dynamicallyCall(withArguments arguments: [Int]) -> Int {
    return 1
  }
  // expected-note @+1 {{found this candidate}}
  func dynamicallyCall(withArguments arguments: [Int]) -> Float {
    return 1.0
  }
}

func testOverloaded(x: OverloadedCallable) {
  let _: Int = x(1, 2, 3)
  let _: Float = x(1, 2, 3)
  let _ = x(1, 2, 3) // expected-error {{ambiguous use of 'dynamicallyCall(withArguments:)'}}
}

//===----------------------------------------------------------------------===//
// Existential tests
//===----------------------------------------------------------------------===//

@dynamicCallable
protocol CallableProtocol {
  func dynamicallyCall(withArguments arguments: [Int]) -> Int
}

@dynamicCallable
protocol KeywordCallableProtocol {}
extension KeywordCallableProtocol {
  func dynamicallyCall(
    withKeywordArguments arguments: KeyValuePairs<String, Double>
  ) -> Int {
    return arguments.count
  }
}

extension String : CallableProtocol, KeywordCallableProtocol {
  func dynamicallyCall(withArguments arguments: [Int]) -> Int {
    return arguments.count
  }
}

func testProtoExtension() -> Int {
  let str = "test"
  return str(1, 2, 3) + str(label1: 1, 2, label2: 3)
}

struct CallableStruct : CallableProtocol {
  func dynamicallyCall(withArguments arguments: [Int]) -> Int {
    return arguments.count
  }
}
class CallableClass : KeywordCallableProtocol {}

func testExistential(
  a: CallableProtocol, b: KeywordCallableProtocol,
  c: CallableStruct, d: CallableClass
) -> Int {
  // Types that define only the `withKeywordsArguments` method can be called
  // with no arguments.
  _ = b() + d()
  return a(1, 2, 3) + b(label1: 1, 2, label2: 3) + c(1, 2, 3) + d(label1: 1, 2, 3)
}

// Verify protocol compositions and refinements work.
protocol SubProtocol : CallableProtocol {}

typealias ProtocolComp = AnyObject & CallableProtocol
typealias ProtocolComp2 = KeywordCallableProtocol & CallableClass

func testExistential2(a: AnyObject & CallableProtocol,
                      b: SubProtocol,
                      c: ProtocolComp & AnyObject,
                      d: CallableClass,
                      e: CallableProtocol & KeywordCallableProtocol,
                      f: CallableProtocol & ProtocolComp2) {
  print(a(1, 2, 3))
  print(b(1, 2, 3))
  print(c(1, 2, 3))
  print(d(1, 2, 3))
  print(e() + e(label1: 1, 2, label2: 3))
  print(f() + f(label1: 1, 2, label2: 3))
}
func testConstrainedClassType<C : AnyObject>(
  a: C
) -> Int where C : CallableProtocol {
  return a(1, 2, 3)
}
func testRefinedProtocolType<P : FloatingPoint>(
  a: P
) -> Int where P : CallableProtocol {
  return a(1, 2, 3)
}

//===----------------------------------------------------------------------===//
// Extension tests
//===----------------------------------------------------------------------===//

extension Optional : KeywordCallableProtocol {}
extension Array : KeywordCallableProtocol {}

func testOptionalExtension() {
  let x: Int? = 3
  // Test `Optional` extension.
  print(x())
  print(x(label: 1, 2))
  // Test `Array` extension.
  print([1]())
  print([1](label: 1, 2))
}

//===----------------------------------------------------------------------===//
// Class inheritance tests
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
    withKeywordArguments arguments: KeyValuePairs<String, Int>
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
// Enum tests
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
// Generics tests
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
    withKeywordArguments arguments: KeyValuePairs<String, T>
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
    withKeywordArguments arguments: KeyValuePairs<StaticString, U>
  ) -> Int {
    return arguments.count
  }
}
func testGenericType4<T>(a: CallableGeneric4<T>) -> Int {
  return a() + a(1, 2, 3) + a(x1: 1, 2, x3: 3)
}

@dynamicCallable
class CallableGeneric5<T> {
  func dynamicallyCall<U>(withArguments arguments: [U]) -> U {
    return arguments[0]
  }

  func dynamicallyCall<U>(
    withKeywordArguments arguments: KeyValuePairs<StaticString, U>
  ) -> U {
    return arguments[0].1
  }
}
func testGenericType5<T>(a: CallableGeneric5<T>) -> Double {
  return a(1, 2, 3) + a(x1: 1, 2, x3: 3)
}
func testArchetypeType5<T, C : CallableGeneric5<T>>(a: C) -> Double {
  return a(1, 2, 3) + a(x1: 1, 2, x3: 3)
}
