// RUN: %target-typecheck-verify-swift
// RUN: %target-typecheck-verify-swift -swift-version 5 -strict-concurrency=complete -verify-additional-prefix swift6-
// RUN: %target-typecheck-verify-swift -swift-version 6 -verify-additional-prefix swift6-

class User {
  @preconcurrency var dict: [String : any Sendable] = [:]
  @preconcurrency var arr: [any Sendable] = []
  // Note: No Set because `any Sendable` is not Hashable
}

extension Dictionary where Key == String, Value == Any {
  func onlyWhenValueAny() {}
}

extension Array where Element == Any {
  func onlyWhenValueAny() {}
}

func test_conditional_on_collections(u: User) {
  u.dict.onlyWhenValueAny() // Ok
  u.arr.onlyWhenValueAny() // Ok
}

// Check that `any Sendable` extension is preferred.

extension Dictionary where Key == String, Value == Any {
  func noAmbiguity() {}
}

extension Array where Element == Any {
  func noAmbiguity() {}
}

extension Dictionary where Key == String, Value == any Sendable {
  func noAmbiguity() {}
}

extension Array where Element == any Sendable {
  func noAmbiguity() {}
}

func test_no_ambiguity_with_Sendable_extension(u: User) {
  u.dict.noAmbiguity() // Ok in general
  u.arr.noAmbiguity() // Ok in general
}

struct S<T> {
  // expected-note@-1 3 {{arguments to generic parameter 'T' ('any Sendable' and 'Any') are expected to be equal}}
  // expected-note@-2 2 {{arguments to generic parameter 'T' ('(any Sendable) -> Void' and '(Any) -> Void') are expected to be equal}}
  // expected-note@-3 {{arguments to generic parameter 'T' ('(Any) -> Any' and '(any Sendable) -> Any') are expected to be equal}}
  // expected-note@-4 {{arguments to generic parameter 'T' ('(Any) -> Any' and '(any Sendable) -> any Sendable') are expected to be equal}}
  // expected-note@-5 {{arguments to generic parameter 'T' ('(Any) -> Any' and '(Any) -> any Sendable') are expected to be equal}}
}

extension S where T == Any {
  func anyOnly() {} // expected-note {{'anyOnly()' declared here}}
}

struct TestGeneral {
  @preconcurrency var v: S<any Sendable>
  @preconcurrency var optV: S<[(any Sendable)?]>
  @preconcurrency var nonOptV: S<[any Sendable]>
  @preconcurrency var funcV: S<((any Sendable)) -> Void>
  @preconcurrency var funcWithResult: S<() -> (any Sendable)?>
  @preconcurrency var anyFunc: S<(Any) -> Any>
  @preconcurrency var funcInFunc: S<((any Sendable) -> Void) -> Void>

  var regularV: S<any Sendable>
  var regularOptV: S<[(any Sendable)?]>
  var regularFuncV: S<((any Sendable)) -> Void>

  func accepts_any(_: S<Any>) {}
  func accepts_opt_any(_: S<[Any?]>) {}
  func accepts_func_any(_: S<(Any) -> Void>) {}
  func accepts_func_with_any_result(_: S<() -> Any?>) {}

  func test_contextual() -> S<Any> {
    v // Ok
  }

  func test_contextual_error() -> S<Any> {
    regularV // expected-error {{cannot convert return expression of type 'S<any Sendable>' to return type 'S<Any>'}}
  }

  func test_member_ref() {
    v.anyOnly() // Ok
    regularV.anyOnly()
    // expected-error@-1 {{referencing instance method 'anyOnly()' on 'S<any Sendable>' requires the types 'any Sendable' and 'Any' be equivalent}}
  }

  func test_passing_as_argument(t: TestGeneral) {
    accepts_any(v) // Ok
    accepts_any(t.v) // Ok

    accepts_any(regularV) // expected-error {{cannot convert value of type 'S<any Sendable>' to expected argument type 'S<Any>'}}
    accepts_any(t.regularV) // expected-error {{cannot convert value of type 'S<any Sendable>' to expected argument type 'S<Any>'}}
  }

  func test_complex_contextual() -> S<[Any?]> {
    optV // Ok
  }

  func test_complex_contextual_error() {
    let _: S<[Any?]> = optV // Ok
    let _: S<[Any?]> = nonOptV // expected-error {{cannot assign value of type 'S<[any Sendable]>' to type 'S<[Any?]>'}}
    // expected-note@-1 {{arguments to generic parameter 'Element' ('any Sendable' and 'Any?') are expected to be equal}}
    let _: S<[Any?]> = regularOptV // expected-error {{cannot assign value of type 'S<[(any Sendable)?]>' to type 'S<[Any?]>'}}
    // expected-note@-1 {{arguments to generic parameter 'Wrapped' ('any Sendable' and 'Any') are expected to be equal}}
  }

  func test_complex_with_argument(t: TestGeneral) {
    accepts_opt_any(optV) // Ok
    accepts_opt_any(t.optV) // Ok

    accepts_opt_any(nonOptV) // expected-error {{cannot convert value of type 'S<[any Sendable]>' to expected argument type 'S<[Any?]>'}}
    // expected-note@-1 {{arguments to generic parameter 'Element' ('any Sendable' and 'Any?') are expected to be equal}}
    accepts_opt_any(t.nonOptV) // expected-error {{cannot convert value of type 'S<[any Sendable]>' to expected argument type 'S<[Any?]>'}}
    // expected-note@-1 {{arguments to generic parameter 'Element' ('any Sendable' and 'Any?') are expected to be equal}}
    accepts_opt_any(regularOptV) // expected-error {{cannot convert value of type 'S<[(any Sendable)?]>' to expected argument type 'S<[Any?]>'}}
    // expected-note@-1 {{arguments to generic parameter 'Wrapped' ('any Sendable' and 'Any') are expected to be equal}}
  }

  func test_no_function_conversions() {
    let _: S<(Any) -> Void> = funcV // Ok
    let _: S<(Any) -> Void> = regularFuncV // expected-error {{cannot assign value of type 'S<(any Sendable) -> Void>' to type 'S<(Any) -> Void>'}}

    accepts_func_any(funcV) // Ok
    accepts_func_any(regularFuncV)
    // expected-error@-1 {{cannot convert value of type 'S<(any Sendable) -> Void>' to expected argument type 'S<(Any) -> Void>'}}

    accepts_func_with_any_result(funcWithResult) // Ok

    func sameType<T>(_: S<T>, _: T.Type) {}

    // FIXME: This unfortunately cannot be supported at the momment because it would require delaying bindings to any generic parameter
    // that has `any Sendable` in some position which has performance implications.
    sameType(funcV, S<(Any) -> Void>.self)
    // expected-error@-1 {{cannot convert value of type 'S<(Any) -> Void>.Type' to expected argument type '((any Sendable) -> Void).Type'}}
    sameType(funcWithResult, S<() -> (any Sendable)?>.self)
    // expected-error@-1 {{cannot convert value of type 'S<() -> (any Sendable)?>.Type' to expected argument type '(() -> (any Sendable)?).Type'}}

    // Make sure that we don't allow Any -> any Sendable
    let _: S<(any Sendable) -> Any> = anyFunc
    // expected-error@-1 {{cannot assign value of type 'S<(Any) -> Any>' to type 'S<(any Sendable) -> Any>'}}
    let _: S<(Any) -> any Sendable> = anyFunc
    // expected-error@-1 {{cannot assign value of type 'S<(Any) -> Any>' to type 'S<(Any) -> any Sendable>'}}
    let _: S<(any Sendable) -> any Sendable> = anyFunc
    // expected-error@-1 {{cannot assign value of type 'S<(Any) -> Any>' to type 'S<(any Sendable) -> any Sendable>'}}

    let _: S<((Any) -> Void) -> Void> = funcInFunc // Ok
  }
}

// Make sure that properties and subscripts and mutating methods work.
extension Dictionary where Key == String, Value == Any {
  subscript<T>(entry object: T) -> T? {
    get { nil }
    set { }
  }

  var test: Int? {
    get { nil }
    set { }
  }

  mutating func testMutating() {}
}

func test_subscript_computed_property_and_mutating_access(u: User) {
  _ = u.dict[entry: ""] // Ok
  u.dict[entry: 42] = 42 // Ok

  _ = u.dict.test // Ok
  u.dict.test = 42 // Ok

  u.dict.testMutating() // Ok
}

extension Dictionary where Key == String, Value == Any {
  init(age: Int) { // expected-note {{'init(age:)' declared here}}
    self.init()
  }
}

extension User {
  convenience init(age: Int) {
    self.init()
    self.dict = .init(age: age)
    // expected-error@-1 {{referencing initializer 'init(age:)' on '[String : any Sendable].Type' requires the types 'any Sendable' and 'Any' be equivalent}}
  }
}

// https://github.com/swiftlang/swift/issues/79361
do {
  @preconcurrency var d = Dictionary<String, any Sendable>()
  
  func test(_ dict: inout Dictionary<String, Any>) {}
  test(&d) // Ok

  @preconcurrency var a = Array<any Sendable>()
  let values: [Any] = []
  a += values // Ok
}
