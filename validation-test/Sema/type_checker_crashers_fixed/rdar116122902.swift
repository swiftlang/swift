// RUN: %target-typecheck-verify-swift

protocol AnyValue {
}

struct Value<T> {}

extension Value: AnyValue where T = { // expected-cparser-error {{use '==' for same-type requirements rather than '='}} expected-cparser-error {{expected type}} expected-astgen-error {{unexpected code '=' in extension}} expected-astgen-error {{expected ':' or '==' to indicate a conformance or same-type requirement}}
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
