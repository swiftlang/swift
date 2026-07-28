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

@propertyWrapper
struct UnrestrictedProjection<T> {
    var wrappedValue: T {
        get { fatalError() }
        set { fatalError() }
    }

    var projectedValue: T {
        get { fatalError() }
        set { fatalError() }
    }
}

@propertyWrapper
struct ProjectedValueConditionallyAvailable<T> {
    var wrappedValue: T {
        get { fatalError() }
        set { fatalError() }
    }

    @available(macOS 51, *)
    var projectedValue: T {
        get { fatalError() }
        set { fatalError() }
    }
}

@propertyWrapper
struct ProjectedValueUnavailable<T> {
    var wrappedValue: T {
        get { fatalError() }
        set { fatalError() }
    }

    @available(macOS, unavailable)
    var projectedValue: T {
        get { fatalError() }
        set { fatalError() }
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

    @ProjectedValueConditionallyAvailable
    var wrapped_projected_value_conditionally_available: Int

    @ProjectedValueUnavailable
    var wrapped_projected_value_unavailable: Int // expected-note 2 {{'$wrapped_projected_value_unavailable' has been explicitly marked unavailable here}}
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

    _ = x.wrapped_projected_value_conditionally_available
    _ = x.$wrapped_projected_value_conditionally_available // expected-error {{'$wrapped_projected_value_conditionally_available' is only available in macOS 51 or newer}} expected-note{{}}
    x.$wrapped_projected_value_conditionally_available = 0 // expected-error {{'$wrapped_projected_value_conditionally_available' is only available in macOS 51 or newer}} expected-note{{}}

    _ = x.wrapped_projected_value_unavailable
    _ = x.$wrapped_projected_value_unavailable // expected-error {{'$wrapped_projected_value_unavailable' is unavailable in macOS}}
    x.$wrapped_projected_value_unavailable = 0 // expected-error {{'$wrapped_projected_value_unavailable' is unavailable in macOS}}

    if #available(macOS 51, *) {
        x.modify_conditionally_available = 0
        x.wrapped_setter_conditionally_available = 0
        x.wrapped_modify_conditionally_available = 0
        x.$wrapped_setter_conditionally_available = 0
        x.$wrapped_modify_conditionally_available = 0
        _ = x.$wrapped_projected_value_conditionally_available
        x.$wrapped_projected_value_conditionally_available = 0
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

  @UnrestrictedProjection
  var unrestricted_projection: Int

  var nested: Nested

  struct Nested {
    @SetterConditionallyAvailable
    var wrapped_setter_more_available: Int

    @ModifyConditionallyAvailable
    var wrapped_modify_more_available: Int

    @UnrestrictedProjection
    var unrestricted_projection: Int
  }
}

func testInferredAvailability(x: inout LessAvailable) { // expected-error {{'LessAvailable' is only available in macOS 51.0 or newer}} expected-note*{{}}
  x.wrapped_setter_more_available = 0 // expected-error {{setter for 'wrapped_setter_more_available' is only available in macOS 51 or newer}} expected-note{{}}
  x.wrapped_modify_more_available = 0 // expected-error {{setter for 'wrapped_modify_more_available' is only available in macOS 51 or newer}} expected-note{{}}
  x.$wrapped_setter_more_available = 0 // expected-error {{'$wrapped_setter_more_available' is only available in macOS 51.0 or newer}} expected-note{{}}
  x.$wrapped_modify_more_available = 0 // expected-error {{'$wrapped_modify_more_available' is only available in macOS 51.0 or newer}} expected-note{{}}

  // The synthesized projection variable inherits availability from the
  // wrapped property, so reads of the projection should also error.
  _ = x.$wrapped_setter_more_available // expected-error {{'$wrapped_setter_more_available' is only available in macOS 51.0 or newer}} expected-note{{}}
  _ = x.$wrapped_modify_more_available // expected-error {{'$wrapped_modify_more_available' is only available in macOS 51.0 or newer}} expected-note{{}}
  _ = x.$unrestricted_projection // expected-error {{'$unrestricted_projection' is only available in macOS 51.0 or newer}} expected-note{{}}
  x.$unrestricted_projection = 0 // expected-error {{'$unrestricted_projection' is only available in macOS 51.0 or newer}} expected-note{{}}

  x.nested.wrapped_setter_more_available = 0 // expected-error {{setter for 'wrapped_setter_more_available' is only available in macOS 51 or newer}} expected-note{{}}
  x.nested.wrapped_modify_more_available = 0 // expected-error {{setter for 'wrapped_modify_more_available' is only available in macOS 51 or newer}} expected-note{{}}
  x.nested.$wrapped_setter_more_available = 0 // expected-error {{'$wrapped_setter_more_available' is only available in macOS 51.0 or newer}} expected-note{{}}
  x.nested.$wrapped_modify_more_available = 0 // expected-error {{'$wrapped_modify_more_available' is only available in macOS 51.0 or newer}} expected-note{{}}
  _ = x.nested.$unrestricted_projection // expected-error {{'$unrestricted_projection' is only available in macOS 51.0 or newer}} expected-note{{}}
  x.nested.$unrestricted_projection = 0 // expected-error {{'$unrestricted_projection' is only available in macOS 51.0 or newer}} expected-note{{}}

  if #available(macOS 51.0, *) {
    x.wrapped_setter_more_available = 0
    x.wrapped_modify_more_available = 0
    x.$wrapped_setter_more_available = 0
    x.$wrapped_modify_more_available = 0
    _ = x.$wrapped_setter_more_available
    _ = x.$wrapped_modify_more_available
    _ = x.$unrestricted_projection
    x.$unrestricted_projection = 0

    x.nested.wrapped_setter_more_available = 0
    x.nested.wrapped_modify_more_available = 0
    x.nested.$wrapped_setter_more_available = 0
    x.nested.$wrapped_modify_more_available = 0
    _ = x.nested.$unrestricted_projection
    x.nested.$unrestricted_projection = 0
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

struct HasDeprecatedWrappedProperty {
  @available(*, deprecated)
  @UnrestrictedProjection var value: Int

  func use() {
    _ = value  // expected-warning {{'value' is deprecated}}
    _ = $value // expected-warning {{'$value' is deprecated}}
    _ = _value // expected-warning {{'_value' is deprecated}}
  }
}
