// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -target %target-cpu-apple-macosx10.9 -typecheck -verify %s

// REQUIRES: OS=macosx

@propertyWrapper
struct SetterConditionallyAvailable<T> {
    var wrappedValue: T {
        get { fatalError() }

        @available(macOS 10.10, *)
        set { fatalError() }
    }

    var projectedValue: T {
        get { fatalError() }

        @available(macOS 10.10, *)
        set { fatalError() }
    }
}

@propertyWrapper
struct ModifyConditionallyAvailable<T> {
    var wrappedValue: T {
        get { fatalError() }

        @available(macOS 10.10, *)
        _modify { fatalError() }
    }

    var projectedValue: T {
        get { fatalError() }

        @available(macOS 10.10, *)
        _modify { fatalError() }
    }
}

struct Butt {
    var modify_conditionally_available: Int {
        get { fatalError() }

        @available(macOS 10.10, *)
        _modify { fatalError() }
    }

    @SetterConditionallyAvailable
    var wrapped_setter_conditionally_available: Int

    @ModifyConditionallyAvailable
    var wrapped_modify_conditionally_available: Int
}

func butt(x: inout Butt) { // expected-note*{{}}
    x.modify_conditionally_available = 0 // expected-error{{only available in macOS 10.10 or newer}} expected-note{{}}
    x.wrapped_setter_conditionally_available = 0 // expected-error{{only available in macOS 10.10 or newer}} expected-note{{}}
    x.wrapped_modify_conditionally_available = 0 // expected-error{{only available in macOS 10.10 or newer}} expected-note{{}}
    x.$wrapped_setter_conditionally_available = 0 // expected-error{{only available in macOS 10.10 or newer}} expected-note{{}}
    x.$wrapped_modify_conditionally_available = 0 // expected-error{{only available in macOS 10.10 or newer}} expected-note{{}}

    if #available(macOS 10.10, *) {
        x.modify_conditionally_available = 0
        x.wrapped_setter_conditionally_available = 0
        x.wrapped_modify_conditionally_available = 0
        x.$wrapped_setter_conditionally_available = 0
        x.$wrapped_modify_conditionally_available = 0
    }
}

@available(macOS 11.0, *)
struct LessAvailable {
  @SetterConditionallyAvailable
  var wrapped_setter_more_available: Int

  @ModifyConditionallyAvailable
  var wrapped_modify_more_available: Int

  var nested: Nested

  struct Nested {
    @SetterConditionallyAvailable
    var wrapped_setter_more_available: Int

    @ModifyConditionallyAvailable
    var wrapped_modify_more_available: Int
  }
}

func testInferredAvailability(x: inout LessAvailable) { // expected-error {{'LessAvailable' is only available in macOS 11.0 or newer}} expected-note*{{}}
  x.wrapped_setter_more_available = 0 // expected-error {{setter for 'wrapped_setter_more_available' is only available in macOS 11.0 or newer}} expected-note{{}}
  x.wrapped_modify_more_available = 0 // expected-error {{setter for 'wrapped_modify_more_available' is only available in macOS 11.0 or newer}} expected-note{{}}
  x.$wrapped_setter_more_available = 0 // expected-error {{setter for '$wrapped_setter_more_available' is only available in macOS 11.0 or newer}} expected-note{{}}
  x.$wrapped_modify_more_available = 0 // expected-error {{setter for '$wrapped_modify_more_available' is only available in macOS 11.0 or newer}} expected-note{{}}

  x.nested.wrapped_setter_more_available = 0 // expected-error {{setter for 'wrapped_setter_more_available' is only available in macOS 11.0 or newer}} expected-note{{}}
  x.nested.wrapped_modify_more_available = 0 // expected-error {{setter for 'wrapped_modify_more_available' is only available in macOS 11.0 or newer}} expected-note{{}}
  x.nested.$wrapped_setter_more_available = 0 // expected-error {{setter for '$wrapped_setter_more_available' is only available in macOS 11.0 or newer}} expected-note{{}}
  x.nested.$wrapped_modify_more_available = 0 // expected-error {{setter for '$wrapped_modify_more_available' is only available in macOS 11.0 or newer}} expected-note{{}}

  if #available(macOS 11.0, *) {
    x.wrapped_setter_more_available = 0
    x.wrapped_modify_more_available = 0
    x.$wrapped_setter_more_available = 0
    x.$wrapped_modify_more_available = 0

    x.nested.wrapped_setter_more_available = 0
    x.nested.wrapped_modify_more_available = 0
    x.nested.$wrapped_setter_more_available = 0
    x.nested.$wrapped_modify_more_available = 0
  }
}

@propertyWrapper
struct Observable<Value> {
  private var stored: Value

  init(wrappedValue: Value) { self.stored = wrappedValue }

  @available(*, unavailable)
  var wrappedValue: Value {
    get { fatalError() }
    set { fatalError() }
  }

  static subscript<EnclosingSelf>(
    _enclosingInstance observed: EnclosingSelf,
    wrapped wrappedKeyPath: ReferenceWritableKeyPath<EnclosingSelf, Value>,
    storage storageKeyPath: ReferenceWritableKeyPath<EnclosingSelf, Self>
  ) -> Value {
    get { observed[keyPath: storageKeyPath].stored }
    set { observed[keyPath: storageKeyPath].stored = newValue }
  }
}

protocol P {}

class Base<T: P> {
  @Observable var item: T?
}

struct Item: P {}

class Subclass: Base<Item> {
  // Make sure this override isn't incorrectly diagnosed as
  // unavailable. Wrapper availability inference should infer
  // availability from the static subscript in this case.
  override var item: Item? {
    didSet {}
  }
}
