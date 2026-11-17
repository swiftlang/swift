// rdar://192999919 (https://github.com/swiftlang/swift/issues/94396)
// RUN: %target-typecheck-verify-swift

@propertyWrapper struct State<Value> {
  init(wrappedValue: Value) {}
  var wrappedValue: Value { get { fatalError() } nonmutating set {} }
}

struct ContentView {
  // expected-error@+3 {{failed to produce diagnostic for expression; please submit a bug report}}
  // expected-error@+2 {{invalid character in source file}}
  // expected-error@+1 {{unicode curly quote found, replace with '"'}}
  @State var name: String = „“
}
