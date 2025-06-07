// RUN: %target-typecheck-verify-swift -target %target-cpu-apple-macosx50

// REQUIRES: OS=macosx

struct S {}

@propertyWrapper
struct AlwaysAvailableWrapper<T> {
  var wrappedValue: T
}

@available(macOS 51, *)
@propertyWrapper
struct Available51Wrapper<T> {
  var wrappedValue: T
}

@available(*, deprecated)
@propertyWrapper
struct DeprecatedWrapper<T> {
  var wrappedValue: T
}

@available(*, unavailable)
@propertyWrapper
struct UnavailableWrapper<T> { // expected-note 8 {{'UnavailableWrapper' has been explicitly marked unavailable here}}
  var wrappedValue: T
}

@propertyWrapper
struct WrappedValueUnavailableOnMacOS<T> {
  init(wrappedValue: T) { fatalError() }

  @available(macOS, unavailable)
  var wrappedValue: T { // expected-note 6 {{'wrappedValue' has been explicitly marked unavailable here}}
    get { fatalError() }
    set { fatalError() }
  }
}

@propertyWrapper
struct WrappedValueAvailable51<T> {
  init(wrappedValue: T) { fatalError() }

  @available(macOS 51, *)
  var wrappedValue: T {
    get { fatalError() }
    set { fatalError() }
  }
}

struct AlwaysAvailableStruct { // expected-note 3 {{add '@available' attribute to enclosing struct}}
  @AlwaysAvailableWrapper var alwaysAvailableExplicit: S
  @AlwaysAvailableWrapper var alwaysAvailableInferred = S()

  @Available51Wrapper var available51Explicit: S // expected-error {{'Available51Wrapper' is only available in macOS 51 or newer}}
  @Available51Wrapper var available51Inferred = S() // expected-error {{'Available51Wrapper' is only available in macOS 51 or newer}}

  @DeprecatedWrapper var deprecatedExplicit: S // expected-warning {{'DeprecatedWrapper' is deprecated}}
  @DeprecatedWrapper var deprecatedInferred = S() // expected-warning {{'DeprecatedWrapper' is deprecated}}

  @UnavailableWrapper var unavailableExplicit: S // expected-error {{'UnavailableWrapper' is unavailable}}
  @UnavailableWrapper var unavailableInferred = S() // expected-error {{'UnavailableWrapper' is unavailable}}

  @WrappedValueUnavailableOnMacOS var unavailableWrappedValue: S // expected-error {{'wrappedValue' is unavailable in macOS}}
  @WrappedValueAvailable51 var wrappedValueAavailable51: S // expected-error {{'wrappedValue' is only available in macOS 51 or newer}}
}

@available(macOS 51, *)
struct Available51Struct {
  @AlwaysAvailableWrapper var alwaysAvailableExplicit: S
  @AlwaysAvailableWrapper var alwaysAvailableInferred = S()

  @Available51Wrapper var available51Explicit: S
  @Available51Wrapper var available51Inferred = S()

  @DeprecatedWrapper var deprecatedExplicit: S // expected-warning {{'DeprecatedWrapper' is deprecated}}
  @DeprecatedWrapper var deprecatedInferred = S() // expected-warning {{'DeprecatedWrapper' is deprecated}}

  @UnavailableWrapper var unavailableExplicit: S // expected-error {{'UnavailableWrapper' is unavailable}}
  @UnavailableWrapper var unavailableInferred = S() // expected-error {{'UnavailableWrapper' is unavailable}}

  @WrappedValueUnavailableOnMacOS var unavailableWrappedValue: S // expected-error {{'wrappedValue' is unavailable in macOS}}
  @WrappedValueAvailable51 var wrappedValueAavailable51: S
}

@available(*, unavailable)
struct UnavailableStruct {
  @AlwaysAvailableWrapper var alwaysAvailableExplicit: S
  @AlwaysAvailableWrapper var alwaysAvailableInferred = S()
  
  @Available51Wrapper var available51Explicit: S
  @Available51Wrapper var available51Inferred = S()

  @DeprecatedWrapper var deprecatedExplicit: S
  @DeprecatedWrapper var deprecatedInferred = S()

  @UnavailableWrapper var unavailableExplicit: S
  @UnavailableWrapper var unavailableInferred = S()

  @WrappedValueUnavailableOnMacOS var unavailableWrappedValue: S
  @WrappedValueAvailable51 var wrappedValueAavailable51: S
}

@available(macOS, unavailable)
struct UnavailableOnMacOSStruct {
  @AlwaysAvailableWrapper var alwaysAvailableExplicit: S
  @AlwaysAvailableWrapper var alwaysAvailableInferred = S()

  @Available51Wrapper var available51Explicit: S
  @Available51Wrapper var available51Inferred = S()

  @DeprecatedWrapper var deprecatedExplicit: S
  @DeprecatedWrapper var deprecatedInferred = S()

  @UnavailableWrapper var unavailableExplicit: S
  @UnavailableWrapper var unavailableInferred = S()

  @WrappedValueUnavailableOnMacOS var unavailableWrappedValue: S
  @WrappedValueAvailable51 var wrappedValueAavailable51: S
}

func alwaysAvailableFunc( // expected-note 4 {{add '@available' attribute to enclosing global function}}
  @AlwaysAvailableWrapper _ alwaysAvailable: S,
  @Available51Wrapper _ available51: S, // expected-error {{'Available51Wrapper' is only available in macOS 51 or newer}}
  @DeprecatedWrapper _ deprecated: S, // expected-warning {{'DeprecatedWrapper' is deprecated}}
  @UnavailableWrapper _ unavailable: S, // expected-error {{'UnavailableWrapper' is unavailable}}
  @WrappedValueUnavailableOnMacOS _ unavailableWrappedValue: S, // expected-error {{'wrappedValue' is unavailable in macOS}}
  @WrappedValueAvailable51 _ wrappedValueAavailable51: S // expected-error {{'wrappedValue' is only available in macOS 51 or newer}}
) {
  @AlwaysAvailableWrapper var alwaysAvailableLocal = S()
  @Available51Wrapper var available51Local = S() // expected-error {{'Available51Wrapper' is only available in macOS 51 or newer}}
  @DeprecatedWrapper var deprecatedLocal = S() // expected-warning {{'DeprecatedWrapper' is deprecated}}
  @UnavailableWrapper var unavailableLocal = S() // expected-error {{'UnavailableWrapper' is unavailable}}
  @WrappedValueUnavailableOnMacOS var unavailableWrappedValueLocal = S() // expected-error {{'wrappedValue' is unavailable}}
  @WrappedValueAvailable51 var wrappedValueAavailable51 = S() // expected-error {{'wrappedValue' is only available in macOS 51 or newer}}
}

@available(macOS 51, *)
func available51Func(
  @AlwaysAvailableWrapper _ alwaysAvailable: S,
  @Available51Wrapper _ available51: S,
  @DeprecatedWrapper _ deprecated: S, // expected-warning {{'DeprecatedWrapper' is deprecated}}
  @UnavailableWrapper _ unavailable: S, // expected-error {{'UnavailableWrapper' is unavailable}}
  @WrappedValueUnavailableOnMacOS _ unavailableWrappedValue: S, // expected-error {{'wrappedValue' is unavailable in macOS}}
  @WrappedValueAvailable51 _ wrappedValueAavailable51: S
) {
  @AlwaysAvailableWrapper var alwaysAvailableLocal = S()
  @Available51Wrapper var available51Local = S()
  @DeprecatedWrapper var deprecatedLocal = S() // expected-warning {{'DeprecatedWrapper' is deprecated}}
  @UnavailableWrapper var unavailableLocal = S() // expected-error {{'UnavailableWrapper' is unavailable}}
  @WrappedValueUnavailableOnMacOS var unavailableWrappedValueLocal = S() // expected-error {{'wrappedValue' is unavailable}}
  @WrappedValueAvailable51 var wrappedValueAavailable51 = S()
}

@available(*, unavailable)
func unavailableFunc(
  @AlwaysAvailableWrapper _ alwaysAvailable: S,
  @Available51Wrapper _ available51: S,
  @DeprecatedWrapper _ deprecated: S,
  @UnavailableWrapper _ unavailable: S,
  @WrappedValueUnavailableOnMacOS _ unavailableWrappedValue: S,
  @WrappedValueAvailable51 _ wrappedValueAavailable51: S
) {
  @AlwaysAvailableWrapper var alwaysAvailableLocal = S()
  @Available51Wrapper var available51Local = S()
  @DeprecatedWrapper var deprecatedLocal = S()
  @UnavailableWrapper var unavailableLocal = S()
  @WrappedValueUnavailableOnMacOS var unavailableWrappedValueLocal = S()
  @WrappedValueAvailable51 var wrappedValueAavailable51 = S()
}

@available(macOS, unavailable)
func unavailableOnMacOSFunc(
  @AlwaysAvailableWrapper _ alwaysAvailable: S,
  @Available51Wrapper _ available51: S,
  @DeprecatedWrapper _ deprecated: S,
  @UnavailableWrapper _ unavailable: S,
  @WrappedValueUnavailableOnMacOS _ unavailableWrappedValue: S,
  @WrappedValueAvailable51 _ wrappedValueAavailable51: S
) {
  @AlwaysAvailableWrapper var alwaysAvailableLocal = S()
  @Available51Wrapper var available51Local = S()
  @DeprecatedWrapper var deprecatedLocal = S()
  @UnavailableWrapper var unavailableLocal = S()
  @WrappedValueUnavailableOnMacOS var unavailableWrappedValueLocal = S()
  @WrappedValueAvailable51 var wrappedValueAavailable51 = S()
}
