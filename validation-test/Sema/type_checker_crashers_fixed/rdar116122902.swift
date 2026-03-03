// RUN: %target-typecheck-verify-swift

protocol AnyValue {
}

struct Value<T> {}

extension Value: AnyValue where T = { // expected-error {{use '==' for same-type requirements rather than '='}} expected-error {{expected type}}
}

struct Test {
  var tuple: (value: any AnyValue, id: Int)?

  mutating func test<T>(v: Value<T>) {
    _ = {
      self.tuple = (v, 42)
      return 0
    }
  }
}
