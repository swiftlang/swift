// RUN: %target-typecheck-verify-swift

// https://github.com/apple/swift/issues/59294
// https://github.com/apple/swift/issues/59295

@propertyWrapper
struct WrapperValue<Value> {
  var value: Value
  init(wrappedValue: Value) {
    self.value = wrappedValue
  }

  var projectedValue: Self {
    return self
  }

  var wrappedValue: Value {
    get {
      self.value
    }
    set {
      self.value = newValue
    }
  }

  func printValue() {
    print(value)
  }

  func returnValue() -> Value {
    return value
  }
}

@propertyWrapper
struct OuterWrapper<Value> {
  var value: Value
  init(wrappedValue: Value) {
    self.value = wrappedValue
  }

  var projectedValue: Self {
    return self
  }

  var wrappedValue: Value {
    get {
      self.value
    }
    set {
      self.value = newValue
    }
  }
}

class Test {
  static func test() {
    return [0, 1, 2].compactMap { _ in // expected-error {{unexpected non-void return value in void function}} expected-note {{did you mean to add a return type?}}
      @WrapperValue var value: Bool? = false
      if value != nil {
        $value.printValue()
        return false
      }

      return value ?? false
    }
  }
  static func testProjectedAndWrapperVars() {
    func test(_: (Int) -> Bool) {}
    func test(_: (String) -> Void) {}

    test {
      @WrapperValue var value = $0
      if $value.returnValue() > 0 {
        test {
          return $value.returnValue() == $0 &&
                 _value.wrappedValue == $0
        }
      }
      return false
    }

    test {
      @WrapperValue var value = $0

      test {
        if _value.wrappedValue != $0 { // Ok
          $value.printValue()
        }
      }
    }
  }

  static func testNestedWrappers() {
    func test(_: (Bool) -> Void) {}
    func test(_: () -> Void) {}

    test {
      if true {
        @OuterWrapper @WrapperValue var value = $0
        if true {
          let _: OuterWrapper<WrapperValue<Bool>> = _value
          let _ = _value.wrappedValue.wrappedValue == $0
          let _: OuterWrapper<WrapperValue<Bool>> = $value
          let _: Bool = value
        }
      }
    }
  }

  static func invalidVar() {
    _ = [0, 1, 2].compactMap {
      @WrapperValue var value: Bool? = $0
      // expected-error@-1 {{type 'Int' cannot be used as a boolean; test for '!= 0' instead}}
      if true {
        $value.printValue()
      }
    }
  }
}
