// RUN: %target-typecheck-verify-swift

@propertyWrapper
@dynamicMemberLookup
struct Binding<Value> {
  var wrappedValue: Value

  init(get: @escaping () -> Value, set: @escaping (Value) -> Void) {
    self.wrappedValue = get()
  }

  subscript<Subject>(dynamicMember keyPath: WritableKeyPath<Value, Subject>) -> Binding<Subject> {
    get { fatalError() }
  }
}

class S {
  var value: String = ""
  var buffer: String? = nil

  var body: String {
    let binding = Binding(
      get: { self.buffer ?? self.value },
      set: { self.buffer = $0 }
    )
    return binding.wrappedValue
  }
}
