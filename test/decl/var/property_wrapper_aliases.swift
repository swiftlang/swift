// RUN: %target-typecheck-verify-swift

// expect-no-diagnostics

@propertyDelegate
struct Delegate<T> {
  var value: T

  var delegateValue: Wrapper<T> { // expected-warning{{property wrapper's `delegateValue` property should be renamed to 'wrapperValue'; use of 'delegateValue' is deprecated}}{{7-20=wrapperValue}}
    return Wrapper(value: value)
  }
}

struct TestDelegateValue {
  @Delegate var foo: String

  func test() -> Wrapper<String> {
    return $foo
  }
}

@propertyWrapper
struct Wrapper<T> {
  var value: T
}
