// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -target x86_64-apple-macosx10.9 -typecheck -verify %s

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
