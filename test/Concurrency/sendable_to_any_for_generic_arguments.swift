// RUN: %target-typecheck-verify-swift
// RUN: %target-typecheck-verify-swift -swift-version 5 -strict-concurrency=complete -verify-additional-prefix swift6-
// RUN: %target-typecheck-verify-swift -swift-version 6 -verify-additional-prefix swift6-

class User {
  @preconcurrency var dict: [String : any Sendable] = [:]
  @preconcurrency var arr: [any Sendable] = []
  // Note: No Set because `any Sendable` is not Hashable
}

extension Dictionary where Key == String, Value == Any {
  func onlyWhenValueAny() {} // expected-swift6-note {{'onlyWhenValueAny()' declared here}}
}

extension Array where Element == Any {
  func onlyWhenValueAny() {} // expected-swift6-note {{'onlyWhenValueAny()' declared here}}
}

func test_conditional_on_collections(u: User) {
  u.dict.onlyWhenValueAny() // Ok with non-strict concurrency
  // expected-swift6-error@-1 {{referencing instance method 'onlyWhenValueAny()' on '[String : any Sendable]' requires the types 'any Sendable' and 'Any' be equivalent}}
  u.arr.onlyWhenValueAny() // Ok with non-strict concurrency
  // expected-swift6-error@-1 {{referencing instance method 'onlyWhenValueAny()' on '[any Sendable]' requires the types 'any Sendable' and 'Any' be equivalent}}
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
  // expected-swift6-note@-3 3 {{arguments to generic parameter 'T' ('any Sendable' and 'Any') are expected to be equal}}
  // expected-swift6-note@-4 2 {{arguments to generic parameter 'T' ('(any Sendable) -> Void' and '(Any) -> Void') are expected to be equal}}
  // expected-note@-5 {{arguments to generic parameter 'T' ('(Any) -> Any' and '(any Sendable) -> Any') are expected to be equal}}
  // expected-note@-6 {{arguments to generic parameter 'T' ('(Any) -> Any' and '(any Sendable) -> any Sendable') are expected to be equal}}
  // expected-note@-7 {{arguments to generic parameter 'T' ('(Any) -> Any' and '(Any) -> any Sendable') are expected to be equal}}
  // expected-swift6-note@-8 {{arguments to generic parameter 'T' ('((any Sendable) -> Void) -> Void' and '((Any) -> Void) -> Void') are expected to be equal}}
}

extension S where T == Any {
  func anyOnly() {} // expected-note {{'anyOnly()' declared here}} expected-swift6-note {{'anyOnly()' declared here}}
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
    v // Ok with non-strict concurrency
    // expected-swift6-error@-1 {{cannot convert return expression of type 'S<any Sendable>' to return type 'S<Any>'}}
  }

  func test_contextual_error() -> S<Any> {
    regularV // expected-error {{cannot convert return expression of type 'S<any Sendable>' to return type 'S<Any>'}}
  }

  func test_member_ref() {
    v.anyOnly() // Ok with non-strict concurrency
    // expected-swift6-error@-1 {{referencing instance method 'anyOnly()' on 'S<any Sendable>' requires the types 'any Sendable' and 'Any' be equivalent}}
    regularV.anyOnly()
    // expected-error@-1 {{referencing instance method 'anyOnly()' on 'S<any Sendable>' requires the types 'any Sendable' and 'Any' be equivalent}}
  }

  func test_passing_as_argument(t: TestGeneral) {
    accepts_any(v) // Ok with non-strict concurrency
    // expected-swift6-error@-1 {{cannot convert value of type 'S<any Sendable>' to expected argument type 'S<Any>'}}
    accepts_any(t.v) // Ok with non-strict concurrency
    // expected-swift6-error@-1 {{cannot convert value of type 'S<any Sendable>' to expected argument type 'S<Any>'}}

    accepts_any(regularV) // expected-error {{cannot convert value of type 'S<any Sendable>' to expected argument type 'S<Any>'}}
    accepts_any(t.regularV) // expected-error {{cannot convert value of type 'S<any Sendable>' to expected argument type 'S<Any>'}}
  }

  func test_complex_contextual() -> S<[Any?]> {
    optV // Ok with non-strict concurrency
    // expected-swift6-error@-1 {{cannot convert return expression of type 'S<[(any Sendable)?]>' to return type 'S<[Any?]>'}}
    // expected-swift6-note@-2 {{arguments to generic parameter 'Wrapped' ('any Sendable' and 'Any') are expected to be equal}}
  }

  func test_complex_contextual_error() {
    let _: S<[Any?]> = optV // Ok with non-strict concurrency
    // expected-swift6-error@-1 {{cannot assign value of type 'S<[(any Sendable)?]>' to type 'S<[Any?]>'}}
    // expected-swift6-note@-2 {{arguments to generic parameter 'Wrapped' ('any Sendable' and 'Any') are expected to be equal}}
    let _: S<[Any?]> = nonOptV // expected-error {{cannot assign value of type 'S<[any Sendable]>' to type 'S<[Any?]>'}}
    // expected-note@-1 {{arguments to generic parameter 'Element' ('any Sendable' and 'Any?') are expected to be equal}}
    let _: S<[Any?]> = regularOptV // expected-error {{cannot assign value of type 'S<[(any Sendable)?]>' to type 'S<[Any?]>'}}
    // expected-note@-1 {{arguments to generic parameter 'Wrapped' ('any Sendable' and 'Any') are expected to be equal}}
  }

  func test_complex_with_argument(t: TestGeneral) {
    accepts_opt_any(optV) // Ok with non-strict concurrency
    // expected-swift6-error@-1 {{cannot convert value of type 'S<[(any Sendable)?]>' to expected argument type 'S<[Any?]>'}}
    // expected-swift6-note@-2 {{arguments to generic parameter 'Wrapped' ('any Sendable' and 'Any') are expected to be equal}}
    accepts_opt_any(t.optV) // Ok with non-strict concurrency
    // expected-swift6-error@-1 {{cannot convert value of type 'S<[(any Sendable)?]>' to expected argument type 'S<[Any?]>'}}
    // expected-swift6-note@-2 {{arguments to generic parameter 'Wrapped' ('any Sendable' and 'Any') are expected to be equal}}

    accepts_opt_any(nonOptV) // expected-error {{cannot convert value of type 'S<[any Sendable]>' to expected argument type 'S<[Any?]>'}}
    // expected-note@-1 {{arguments to generic parameter 'Element' ('any Sendable' and 'Any?') are expected to be equal}}
    accepts_opt_any(t.nonOptV) // expected-error {{cannot convert value of type 'S<[any Sendable]>' to expected argument type 'S<[Any?]>'}}
    // expected-note@-1 {{arguments to generic parameter 'Element' ('any Sendable' and 'Any?') are expected to be equal}}
    accepts_opt_any(regularOptV) // expected-error {{cannot convert value of type 'S<[(any Sendable)?]>' to expected argument type 'S<[Any?]>'}}
    // expected-note@-1 {{arguments to generic parameter 'Wrapped' ('any Sendable' and 'Any') are expected to be equal}}
  }

  func test_no_function_conversions() {
    let _: S<(Any) -> Void> = funcV // Ok with non-strict concurrency
    // expected-swift6-error@-1 {{cannot assign value of type 'S<(any Sendable) -> Void>' to type 'S<(Any) -> Void>'}}
    let _: S<(Any) -> Void> = regularFuncV // expected-error {{cannot assign value of type 'S<(any Sendable) -> Void>' to type 'S<(Any) -> Void>'}}

    accepts_func_any(funcV) // Ok with non-strict concurrency
    // expected-swift6-error@-1 {{cannot convert value of type 'S<(any Sendable) -> Void>' to expected argument type 'S<(Any) -> Void>'}}
    accepts_func_any(regularFuncV)
    // expected-error@-1 {{cannot convert value of type 'S<(any Sendable) -> Void>' to expected argument type 'S<(Any) -> Void>'}}

    accepts_func_with_any_result(funcWithResult) // Ok with non-strict concurrency
    // expected-swift6-error@-1 {{cannot convert value of type 'S<() -> (any Sendable)?>' to expected argument type 'S<() -> Any?>'}}
    // expected-swift6-note@-2 {{arguments to generic parameter 'Wrapped' ('any Sendable' and 'Any') are expected to be equal}}

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

    let _: S<((Any) -> Void) -> Void> = funcInFunc // Ok with non-strict concurrency
    // expected-swift6-error@-1 {{cannot assign value of type 'S<((any Sendable) -> Void) -> Void>' to type 'S<((Any) -> Void) -> Void>'}}
  }
}
