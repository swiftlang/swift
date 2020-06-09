// RUN: %target-typecheck-verify-swift

@propertyWrapper
@dynamicMemberLookup
struct Binding<Value> {
  var wrappedValue: Value

  subscript<Subject>(dynamicMember keyPath: WritableKeyPath<Value, Subject>) -> Binding<Subject> {
    get { fatalError() }
  }
}

@propertyWrapper
struct Wrapper<Value> {
  var wrappedValue: Value
  var projectedValue: Binding<Value>
}

@dynamicMemberLookup class Foo {
  struct State {
    let value: Bool
  }

  let currentState: State = State(value: false)

  subscript<U>(dynamicMember keyPath: KeyPath<State, U>) -> U {
    return currentState[keyPath: keyPath]
  }
}

struct Test {
  @Wrapper var foo: Foo

  func test() {
    if foo.bar { // expected-error {{value of type 'Foo' has no dynamic member 'bar' using key path from root type 'Foo.State'}}
    }
  }
}
