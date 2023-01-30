// RUN: %target-typecheck-verify-swift -target %target-cpu-apple-macosx10.50

// REQUIRES: OS=macosx

struct S {}

@propertyWrapper
struct AlwaysAvailableWrapper<T> {
  var wrappedValue: T
}

@available(macOS 10.51, *)
@propertyWrapper
struct Available10_51Wrapper<T> {
  var wrappedValue: T
}

@available(*, unavailable)
@propertyWrapper
struct UnavailableWrapper<T> { // expected-note 6 {{'UnavailableWrapper' has been explicitly marked unavailable here}}
  var wrappedValue: T
}


struct AlwaysAvailableStruct { // expected-note 2 {{add @available attribute to enclosing struct}}
  @AlwaysAvailableWrapper var alwaysAvailableExplicit: S
  @AlwaysAvailableWrapper var alwaysAvailableInferred = S()

  @Available10_51Wrapper var available10_51Explicit: S // expected-error {{'Available10_51Wrapper' is only available in macOS 10.51 or newer}}
  @Available10_51Wrapper var available10_51Inferred = S() // expected-error {{'Available10_51Wrapper' is only available in macOS 10.51 or newer}}
  
  @UnavailableWrapper var unavailableExplicit: S // expected-error {{'UnavailableWrapper' is unavailable}}
  @UnavailableWrapper var unavailableInferred = S() // expected-error {{'UnavailableWrapper' is unavailable}}
}

@available(macOS 10.51, *)
struct Available10_51Struct {
  @AlwaysAvailableWrapper var alwaysAvailableExplicit: S
  @AlwaysAvailableWrapper var alwaysAvailableInferred = S()

  @Available10_51Wrapper var available10_51Explicit: S
  @Available10_51Wrapper var available10_51Inferred = S()
  
  @UnavailableWrapper var unavailableExplicit: S // expected-error {{'UnavailableWrapper' is unavailable}}
  @UnavailableWrapper var unavailableInferred = S() // expected-error {{'UnavailableWrapper' is unavailable}}
}

@available(*, unavailable)
struct UnavailableStruct {
  @AlwaysAvailableWrapper var alwaysAvailableExplicit: S
  @AlwaysAvailableWrapper var alwaysAvailableInferred = S()
  
  @Available10_51Wrapper var available10_51Explicit: S  // expected-error {{'Available10_51Wrapper' is only available in macOS 10.51 or newer}}
  @Available10_51Wrapper var available10_51Inferred = S() // expected-error {{'Available10_51Wrapper' is only available in macOS 10.51 or newer}}

  @UnavailableWrapper var unavailableExplicit: S
  @UnavailableWrapper var unavailableInferred = S()
}

func alwaysAvailableFunc( // expected-note {{add @available attribute to enclosing global function}}
  @AlwaysAvailableWrapper _ alwaysAvailable: S,
  @Available10_51Wrapper _ available10_51: S, // expected-error {{'Available10_51Wrapper' is only available in macOS 10.51 or newer}}
  @UnavailableWrapper _ unavailable: S // expected-error {{'UnavailableWrapper' is unavailable}}
) {}

@available(macOS 10.51, *)
func available10_51Func(
  @AlwaysAvailableWrapper _ alwaysAvailable: S,
  @Available10_51Wrapper _ available10_51: S,
  @UnavailableWrapper _ unavailable: S // expected-error {{'UnavailableWrapper' is unavailable}}
) {}

@available(*, unavailable)
func unavailableFunc(
  @AlwaysAvailableWrapper _ alwaysAvailable: S,
  @Available10_51Wrapper _ available10_51: S,
  @UnavailableWrapper _ unavailable: S
) {}
