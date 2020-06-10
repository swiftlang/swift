// RUN: %target-typecheck-verify-swift -swift-version 5 -disable-availability-checking

protocol P { }

@propertyWrapper
struct WrapperWithDefaultInit<T> {
  private var stored: T?

  var wrappedValue: T {
    get { stored! }
    set { stored = newValue }
  }

  init() {
    self.stored = nil
  }
}

// FB7699647 - crash with opaque result type and property wrappers.
struct FB7699647 {
  @WrapperWithDefaultInit var property: some P // expected-error{{property declares an opaque return type, but cannot infer the underlying type from its initializer expression}}
  @WrapperWithDefaultInit() var otherProperty: some P // expected-error{{property declares an opaque return type, but cannot infer the underlying type from its initializer expression}}
}

struct FB7699647b {
  @WrapperWithDefaultInit var property: some P // expected-error{{property declares an opaque return type, but cannot infer the underlying type from its initializer expression}}
}
