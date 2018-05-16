// RUN: %target-swift-frontend -typecheck -verify %s
var global = 42

@dynamicCallable
struct Callable {
  func dynamicallyCall(withArguments arguments: [Int]) -> Int {
    return 1
  }
}

//===----------------------------------------------------------------------===//
// Returning a function
//===----------------------------------------------------------------------===//

@dynamicCallable
struct FnTest {
  func dynamicallyCall(withArguments arguments: [Int]) -> () -> Int {
    return { a in 1 }
  }
}

//===----------------------------------------------------------------------===//
// Existential Cases
//===----------------------------------------------------------------------===//


@dynamicCallable
protocol ProtoExt {}
extension ProtoExt {
  func dynamicallyCall(withArguments arguments: [Int]) -> Int {
    return 1
  }
}

extension String: ProtoExt { }

//===----------------------------------------------------------------------===//
// Error cases
//===----------------------------------------------------------------------===//

// Arguments' type may not be variadic.
// expected-error @+1 {{@dynamicCallable attribute requires 'Invalid1' to have either a valid 'dynamicallyCall(withArguments:)' method or a valid 'dynamicallyCall(withKeywordArguments:)' method}}
@dynamicCallable
struct Invalid1 {
  func dynamicallyCall(withArguments arguments: [Int]...) -> Int {
    return 1
  }
}

// Keyword arguments' key type must be ExpressibleByStringLiteral.
// expected-error @+1 {{@dynamicCallable attribute requires 'Invalid2' to have either a valid 'dynamicallyCall(withArguments:)' method or a valid 'dynamicallyCall(withKeywordArguments:)' method}}
@dynamicCallable
struct Invalid2 {
  func dynamicallyCall(
    withKeywordArguments arguments: DictionaryLiteral<Int, Int>
  ) -> Int {
    return 1
  }
}

// References to overloads are currently not supported.
// Eventually, they may be contextually disambiguated.
// expected-error @+1 {{@dynamicCallable attribute requires 'Ambiguity' to have either a valid 'dynamicallyCall(withArguments:)' method or a valid 'dynamicallyCall(withKeywordArguments:)' method}}
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
func NotAllowedOnFunc() {
}


// @dynamicCallable cannot be declared on a base class and fulfilled with a
// derived class.

// expected-error @+1 {{@dynamicCallable attribute requires 'InvalidBase' to have either a valid 'dynamicallyCall(withArguments:)' method or a valid 'dynamicallyCall(withKeywordArguments:)' method}}
@dynamicCallable
class InvalidBase {}

class InvalidDerived : InvalidBase {
  func dynamicallyCall(withArguments arguments: [Int]) -> Int {
    return 1
  }
}

//===----------------------------------------------------------------------===//
// Derived Class Example
//===----------------------------------------------------------------------===//

@dynamicCallable
class BaseClass {
  func dynamicallyCall(withArguments arguments: [Int]) -> Int {
    return arguments.count
  }
}
class DerivedClass : BaseClass {}

//===----------------------------------------------------------------------===//
// Generics
//===----------------------------------------------------------------------===//

@dynamicCallable
struct CallableGeneric1<T> {
  func dynamicallyCall(withArguments arguments: [T]) -> Int {
    return arguments.count
  }
}

@dynamicCallable
struct CallableGeneric2<T> {
  func dynamicallyCall(
    withKeywordArguments arguments: DictionaryLiteral<String, T>
  ) -> T {
    return arguments[0].value
  }
}
