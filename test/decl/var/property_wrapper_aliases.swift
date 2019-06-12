// RUN: %target-typecheck-verify-swift

// expect-no-diagnostics

@propertyDelegate // expected-warning{{'@propertyDelegate' has been renamed to '@propertyWrapper'}}{{2-18=propertyWrapper}}
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

@_propertyWrapper // expected-warning{{'@_propertyWrapper' has been renamed to '@propertyWrapper'}}{{2-18=propertyWrapper}}
struct Wrapper<T> {
  var value: T
}
