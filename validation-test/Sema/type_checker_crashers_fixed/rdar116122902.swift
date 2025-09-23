// RUN: %target-typecheck-verify-swift

protocol AnyValue {
}

struct Value<T> {}

extension Value: AnyValue where T = { // expected-error {{use '==' for same-type requirements rather than '='}} expected-error {{expected type}}
// expected-note@-1 {{requirement from conditional conformance of 'Value<T>' to 'AnyValue'}}
}

struct Test {
  var tuple: (value: any AnyValue, id: Int)?

  mutating func test<T>(v: Value<T>) {
    _ = {
      // FIXME(diagnostics): This isn't a useful error message
      self.tuple = (v, 42)
      // expected-error@-1 {{generic struct 'Value' requires the types 'T' and '_' be equivalent}}
      return 0
    }
  }
}
