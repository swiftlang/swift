// RUN: %target-typecheck-verify-swift

// expect-no-diagnostics

@propertyDelegate // expected-warning{{'@propertyDelegate' has been renamed to '@propertyWrapper'}}{{2-18=propertyWrapper}}
struct Delegate<T> {
  var wrappedValue: T

  var delegateValue: Wrapper<T> { // expected-warning{{property wrapper's `delegateValue` property should be renamed to 'projectedValue'; use of 'delegateValue' is deprecated}}{{7-20=projectedValue}}
    return Wrapper(wrappedValue: wrappedValue)
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
  var wrappedValue: T
}

@propertyWrapper
struct OldValue<T> {
  var value: T // expected-warning{{property wrapper's `value` property should be renamed to 'wrappedValue'; use of 'value' is deprecated}}{{7-12=wrappedValue}}
}

struct TestOldValue {
  @OldValue var x: String

  func f() -> String {
    return x
  }
}

@propertyWrapper
struct OldWrapper<T> {
  var wrappedValue: T

  var wrapperValue: Wrapper<T> { // expected-warning{{property wrapper's `wrapperValue` property should be renamed to 'projectedValue'; use of 'wrapperValue' is deprecated}}{{7-19=projectedValue}}
    return Wrapper(wrappedValue: wrappedValue)
  }
}

@propertyWrapper
struct OldWrapperWithInit<T> {
  var wrappedValue: T

  init(initialValue: T) { // expected-warning{{property wrapper's `init(initialValue:)` should be renamed to 'init(wrappedValue:)'; use of 'init(initialValue:)' is deprecated}}{{8-8=wrappedValue }}
    self.wrappedValue = initialValue
  }
}

@propertyWrapper
struct OldWrapperWithInit2<T> {
  var wrappedValue: T

  init(initialValue value: T) { // expected-warning{{property wrapper's `init(initialValue:)` should be renamed to 'init(wrappedValue:)'; use of 'init(initialValue:)' is deprecated}}{{8-20=wrappedValue}}
    self.wrappedValue = value
  }
}


struct TestOldWrapperInits {
  @OldWrapperWithInit var x = 17
  
  @OldWrapperWithInit2 var y = "Hello"
}
