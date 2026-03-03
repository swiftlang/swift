// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -target %target-cpu-apple-macosx50 -typecheck -verify %s

// REQUIRES: OS=macosx

@propertyWrapper
struct SetterConditionallyAvailable<T> {
    var wrappedValue: T {
        get { fatalError() }

        @available(macOS 51, *)
        set { fatalError() }
    }

    var projectedValue: T {
        get { fatalError() }

        @available(macOS 51, *)
        set { fatalError() }
    }
}

@propertyWrapper
struct ModifyConditionallyAvailable<T> {
    var wrappedValue: T {
        get { fatalError() }

        @available(macOS 51, *)
        _modify { fatalError() }
    }

    var projectedValue: T {
        get { fatalError() }

        @available(macOS 51, *)
        _modify { fatalError() }
    }
}

@propertyWrapper
struct SetterMoreAvailable<T> {
    var wrappedValue: T {
        get { fatalError() }

        @available(macOS 49, *)
        set { fatalError() }
    }

    var projectedValue: T {
        get { fatalError() }

        @available(macOS 49, *)
        set { fatalError() }
    }
}

@propertyWrapper
struct ModifyMoreAvailable<T> {
    var wrappedValue: T {
        get { fatalError() }

        @available(macOS 49, *)
        _modify { fatalError() }
    }

    var projectedValue: T {
        get { fatalError() }

        @available(macOS 49, *)
        _modify { fatalError() }
    }
}

struct Butt {
    var modify_conditionally_available: Int {
        get { fatalError() }

        @available(macOS 51, *)
        _modify { fatalError() }
    }

    @SetterConditionallyAvailable
    var wrapped_setter_conditionally_available: Int

    @ModifyConditionallyAvailable
    var wrapped_modify_conditionally_available: Int

    @SetterMoreAvailable
    var wrapped_setter_more_available: Int

    @ModifyMoreAvailable
    var wrapped_modify_more_available: Int
}

func butt(x: inout Butt) { // expected-note * {{}}
    x.modify_conditionally_available = 0 // expected-error {{only available in macOS 51 or newer}} expected-note{{}}
    x.wrapped_setter_conditionally_available = 0 // expected-error {{only available in macOS 51 or newer}} expected-note{{}}
    x.wrapped_modify_conditionally_available = 0 // expected-error {{only available in macOS 51 or newer}} expected-note{{}}
    x.$wrapped_setter_conditionally_available = 0 // expected-error {{only available in macOS 51 or newer}} expected-note{{}}
    x.$wrapped_modify_conditionally_available = 0 // expected-error {{only available in macOS 51 or newer}} expected-note{{}}
    x.wrapped_setter_more_available = 0
    x.wrapped_modify_more_available = 0
    x.$wrapped_setter_more_available = 0
    x.$wrapped_modify_more_available = 0

    if #available(macOS 51, *) {
        x.modify_conditionally_available = 0
        x.wrapped_setter_conditionally_available = 0
        x.wrapped_modify_conditionally_available = 0
        x.$wrapped_setter_conditionally_available = 0
        x.$wrapped_modify_conditionally_available = 0
    }
}

@available(macOS, unavailable)
extension Butt {
  @available(iOS, unavailable)
  struct Nested { // expected-note {{has been explicitly marked unavailable here}}
    @SetterMoreAvailable
    var wrapped_setter_more_available: Int // expected-note 2 {{has been explicitly marked unavailable here}}

    @ModifyMoreAvailable
    var wrapped_modify_more_available: Int // expected-note 2 {{has been explicitly marked unavailable here}}
  }
}

func testButtNested(x: inout Butt.Nested) { // expected-error {{'Nested' is unavailable in macOS}}
  x.wrapped_setter_more_available = 0 // expected-error {{is unavailable in macOS}}
  x.wrapped_modify_more_available = 0 // expected-error {{is unavailable in macOS}}
  x.$wrapped_setter_more_available = 0 // expected-error {{is unavailable in macOS}}
  x.$wrapped_modify_more_available = 0 // expected-error {{is unavailable in macOS}}
}

@available(iOS, unavailable)
@_spi_available(macOS, introduced: 51)
extension Butt {
  struct NestedInSPIAvailableExtension {
    @available(macOS, unavailable)
    public var unavailable: Int {// expected-note {{'unavailable' has been explicitly marked unavailable here}}
      get { 0 }
      set {}
    }
  }
}

@available(macOS, introduced: 51)
func testButtNested(x: inout Butt.NestedInSPIAvailableExtension) {
  x.unavailable = 0 // expected-error {{is unavailable in macOS}}
}

@available(macOS 51.0, *)
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

func testInferredAvailability(x: inout LessAvailable) { // expected-error {{'LessAvailable' is only available in macOS 51.0 or newer}} expected-note*{{}}
  x.wrapped_setter_more_available = 0 // expected-error {{setter for 'wrapped_setter_more_available' is only available in macOS 51 or newer}} expected-note{{}}
  x.wrapped_modify_more_available = 0 // expected-error {{setter for 'wrapped_modify_more_available' is only available in macOS 51 or newer}} expected-note{{}}
  x.$wrapped_setter_more_available = 0 // expected-error {{setter for '$wrapped_setter_more_available' is only available in macOS 51 or newer}} expected-note{{}}
  x.$wrapped_modify_more_available = 0 // expected-error {{setter for '$wrapped_modify_more_available' is only available in macOS 51 or newer}} expected-note{{}}

  x.nested.wrapped_setter_more_available = 0 // expected-error {{setter for 'wrapped_setter_more_available' is only available in macOS 51 or newer}} expected-note{{}}
  x.nested.wrapped_modify_more_available = 0 // expected-error {{setter for 'wrapped_modify_more_available' is only available in macOS 51 or newer}} expected-note{{}}
  x.nested.$wrapped_setter_more_available = 0 // expected-error {{setter for '$wrapped_setter_more_available' is only available in macOS 51 or newer}} expected-note{{}}
  x.nested.$wrapped_modify_more_available = 0 // expected-error {{setter for '$wrapped_modify_more_available' is only available in macOS 51 or newer}} expected-note{{}}

  if #available(macOS 51.0, *) {
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
