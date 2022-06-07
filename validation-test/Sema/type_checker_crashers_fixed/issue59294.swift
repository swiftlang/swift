// RUN: %target-typecheck-verify-swift

// https://github.com/apple/swift/issues/59294

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
}


class Test {
  static func test() {
    return [0, 1, 2].compactMap { _ in // expected-error {{unexpected non-void return value in void function}} expected-note {{did you mean to add a return type?}}
      @WrapperValue var value: Bool? = false
      if value != nil {
        return false
      }

      return value ?? false
    }
  }
}
