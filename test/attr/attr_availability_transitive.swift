// RUN: %target-typecheck-verify-swift -parse-as-library

// Allow referencing unavailable API in situations where the caller is marked unavailable in the same circumstances.

struct AlwaysAvailable {}

@available(*, unavailable)
struct NeverAvailable {} // expected-note * {{'NeverAvailable' has been explicitly marked unavailable here}}

@available(swift, obsoleted: 4)
struct UnavailableInSwift4 {} // expected-note * {{'UnavailableInSwift4' was obsoleted in Swift 4}}

@available(swift, introduced: 99)
struct AvailableInFutureSwift {} // expected-note * {{'AvailableInFutureSwift' was introduced in Swift 99}}

@available(*, deprecated)
struct Deprecated {}

@discardableResult
func always() -> AlwaysAvailable {
  AlwaysAvailable()
}

@available(*, unavailable)
@discardableResult
func never() -> NeverAvailable { // expected-note * {{'never()' has been explicitly marked unavailable here}}
  NeverAvailable()
}

@available(swift, obsoleted: 4)
@discardableResult
func unavailableInSwift4() -> UnavailableInSwift4 { // expected-note * {{'unavailableInSwift4()' was obsoleted in Swift 4}}
  UnavailableInSwift4()
}

@available(swift, introduced: 99)
@discardableResult
func availableInFutureSwift() -> AvailableInFutureSwift { // expected-note * {{'availableInFutureSwift()' was introduced in Swift 99}}
  AvailableInFutureSwift()
}

@available(*, deprecated)
@discardableResult
func deprecated() -> Deprecated {
  Deprecated()
}

// MARK: Global functions

func available_func( // expected-note * {{add '@available' attribute to enclosing global function}}
  _: AlwaysAvailable,
  _: NeverAvailable, // expected-error {{'NeverAvailable' is unavailable}}
  _: UnavailableInSwift4, // expected-error {{'UnavailableInSwift4' is unavailable}}
  _: AvailableInFutureSwift, // expected-error {{'AvailableInFutureSwift' is unavailable}}
  _: Deprecated, // expected-warning {{'Deprecated' is deprecated}}
) {
  always()
  never() // expected-error {{'never()' is unavailable}}
  unavailableInSwift4() // expected-error {{'unavailableInSwift4()' is unavailable}}
  availableInFutureSwift() // expected-error {{'availableInFutureSwift()' is unavailable}}
  deprecated() // expected-warning {{'deprecated()' is deprecated}}
}

@available(*, unavailable)
func never_available_func(
  _: AlwaysAvailable,
  _: NeverAvailable,
  _: UnavailableInSwift4,
  _: AvailableInFutureSwift,
  _: Deprecated,
) {
  always()
  never() // expected-error {{'never()' is unavailable}}
  unavailableInSwift4() // expected-error {{'unavailableInSwift4()' is unavailable}}
  availableInFutureSwift() // expected-error {{'availableInFutureSwift()' is unavailable in Swift}}
  deprecated() // expected-warning {{'deprecated()' is deprecated}}
}

@available(swift, obsoleted: 4)
func unavailable_in_swift4_func(
  _: AlwaysAvailable,
  _: NeverAvailable, // expected-error {{'NeverAvailable' is unavailable}}
  _: UnavailableInSwift4,
  _: AvailableInFutureSwift,
  _: Deprecated,
) {
  always()
  never() // expected-error {{'never()' is unavailable}}
  unavailableInSwift4() // expected-error {{'unavailableInSwift4()' is unavailable}}
  availableInFutureSwift() // expected-error {{'availableInFutureSwift()' is unavailable in Swift}}
  deprecated() // expected-warning {{'deprecated()' is deprecated}}
}

@available(swift, introduced: 99)
func introduced_in_future_swift_func(
  _: AlwaysAvailable,
  _: NeverAvailable, // expected-error {{'NeverAvailable' is unavailable}}
  _: UnavailableInSwift4,
  _: AvailableInFutureSwift,
  _: Deprecated,
) {
  always()
  never() // expected-error {{'never()' is unavailable}}
  unavailableInSwift4() // expected-error {{'unavailableInSwift4()' is unavailable}}
  availableInFutureSwift() // expected-error {{'availableInFutureSwift()' is unavailable in Swift}}
  deprecated() // expected-warning {{'deprecated()' is deprecated}}
}

@available(*, deprecated)
func deprecated_func(
  _: AlwaysAvailable,
  _: NeverAvailable, // expected-error {{'NeverAvailable' is unavailable}}
  _: UnavailableInSwift4, // expected-error {{'UnavailableInSwift4' is unavailable}}
  _: AvailableInFutureSwift, // expected-error {{'AvailableInFutureSwift' is unavailable}}
  _: Deprecated,
) {
  always()
  never() // expected-error {{'never()' is unavailable}}
  unavailableInSwift4() // expected-error {{'unavailableInSwift4()' is unavailable}}
  availableInFutureSwift() // expected-error {{'availableInFutureSwift()' is unavailable}}
  deprecated()
}

// MARK: Global vars

var always_var: (
  AlwaysAvailable,
  NeverAvailable, // expected-error {{'NeverAvailable' is unavailable}}
  UnavailableInSwift4, // expected-error {{'UnavailableInSwift4' is unavailable}}
  AvailableInFutureSwift, // expected-error {{'AvailableInFutureSwift' is unavailable}}
  Deprecated // expected-warning {{'Deprecated' is deprecated}}
) = (
  always(),
  never(), // expected-error {{'never()' is unavailable}}
  unavailableInSwift4(), // expected-error {{'unavailableInSwift4()' is unavailable}}
  availableInFutureSwift(), // expected-error {{'availableInFutureSwift()' is unavailable}}
  deprecated() // expected-warning {{'deprecated()' is deprecated}}
)

@available(*, unavailable)
var never_var: (
  AlwaysAvailable,
  NeverAvailable,
  UnavailableInSwift4,
  AvailableInFutureSwift,
  Deprecated
) = (
  always(),
  never(), // expected-error {{'never()' is unavailable}}
  unavailableInSwift4(), // expected-error {{'unavailableInSwift4()' is unavailable}}
  availableInFutureSwift(), // expected-error {{'availableInFutureSwift()' is unavailable in Swift}}
  deprecated() // expected-warning {{'deprecated()' is deprecated}}
)

@available(swift, obsoleted: 4)
var unavailable_in_swift4_var: (
  AlwaysAvailable,
  NeverAvailable, // expected-error {{'NeverAvailable' is unavailable}}
  UnavailableInSwift4,
  AvailableInFutureSwift,
  Deprecated
) = (
  always(),
  never(), // expected-error {{'never()' is unavailable}}
  unavailableInSwift4(), // expected-error {{'unavailableInSwift4()' is unavailable}}
  availableInFutureSwift(), // expected-error {{'availableInFutureSwift()' is unavailable in Swift}}
  deprecated() // expected-warning {{'deprecated()' is deprecated}}
)

@available(swift, introduced: 99)
var available_in_future_swift_var: (
  AlwaysAvailable,
  NeverAvailable, // expected-error {{'NeverAvailable' is unavailable}}
  UnavailableInSwift4,
  AvailableInFutureSwift,
  Deprecated
) = (
  always(),
  never(), // expected-error {{'never()' is unavailable}}
  unavailableInSwift4(), // expected-error {{'unavailableInSwift4()' is unavailable}}
  availableInFutureSwift(), // expected-error {{'availableInFutureSwift()' is unavailable in Swift}}
  deprecated() // expected-warning {{'deprecated()' is deprecated}}
)

@available(*, deprecated)
var deprecated_var: (
  AlwaysAvailable,
  NeverAvailable, // expected-error {{'NeverAvailable' is unavailable}}
  UnavailableInSwift4, // expected-error {{'UnavailableInSwift4' is unavailable}}
  AvailableInFutureSwift, // expected-error {{'AvailableInFutureSwift' is unavailable}}
  Deprecated
) = (
  always(),
  never(), // expected-error {{'never()' is unavailable}}
  unavailableInSwift4(), // expected-error {{'unavailableInSwift4()' is unavailable}}
  availableInFutureSwift(), // expected-error {{'availableInFutureSwift()' is unavailable}}
  deprecated()
)


// MARK: Properties

struct AlwaysAvailableContainer {
  let always_var: AlwaysAvailable = always()
  let never_var: NeverAvailable = never() // expected-error {{'never()' is unavailable}}
  // expected-error@-1 {{'NeverAvailable' is unavailable}}
  let unavailable_in_swift4_var: UnavailableInSwift4 = unavailableInSwift4() // expected-error {{'unavailableInSwift4()' is unavailable}}
  // expected-error@-1 {{'UnavailableInSwift4' is unavailable}}
  let available_in_future_swift_var: AvailableInFutureSwift = availableInFutureSwift() // expected-error {{'availableInFutureSwift()' is unavailable}}
  // expected-error@-1 {{'AvailableInFutureSwift' is unavailable}}
  let deprecated_var: Deprecated = deprecated() // expected-warning {{'deprecated()' is deprecated}}
  // expected-warning@-1 {{'Deprecated' is deprecated}}
}

@available(*, unavailable)
struct NeverAvailableContainer { // expected-note * {{'NeverAvailableContainer' has been explicitly marked unavailable here}}
  let always_var: AlwaysAvailable = always()
  let never_var: NeverAvailable = never() // expected-error {{'never()' is unavailable}}
  let unavailable_in_swift4_var: UnavailableInSwift4 = unavailableInSwift4() // expected-error {{'unavailableInSwift4()' is unavailable}}
  let available_in_future_swift_var: AvailableInFutureSwift = availableInFutureSwift() // expected-error {{'availableInFutureSwift()' is unavailable in Swift}}
  let deprecated_var: Deprecated = deprecated() // expected-warning {{'deprecated()' is deprecated}}
}

@available(swift, obsoleted: 4)
struct UnavailableInSwift4Container { // expected-note * {{'UnavailableInSwift4Container' was obsoleted in Swift 4}}
  let always_var: AlwaysAvailable = always()
  let never_var: NeverAvailable = never() // expected-error {{'never()' is unavailable}}
  // expected-error@-1 {{'NeverAvailable' is unavailable}}
  let unavailable_in_swift4_var: UnavailableInSwift4 = unavailableInSwift4() // expected-error {{'unavailableInSwift4()' is unavailable}}
  let available_in_future_swift_var: AvailableInFutureSwift = availableInFutureSwift() // expected-error {{'availableInFutureSwift()' is unavailable in Swift}}
  let deprecated_var: Deprecated = deprecated() // expected-warning {{'deprecated()' is deprecated}}
}

@available(swift, introduced: 99)
struct AvailableInFutureSwiftContainer { // expected-note * {{'AvailableInFutureSwiftContainer' was introduced in Swift 99}}
  let always_var: AlwaysAvailable = always()
  let never_var: NeverAvailable = never() // expected-error {{'never()' is unavailable}}
  // expected-error@-1 {{'NeverAvailable' is unavailable}}
  let unavailable_in_swift4_var: UnavailableInSwift4 = unavailableInSwift4()  // expected-error {{'unavailableInSwift4()' is unavailable}}
  let available_in_future_swift_var: AvailableInFutureSwift = availableInFutureSwift()  // expected-error {{'availableInFutureSwift()' is unavailable in Swift}}
  let deprecated_var: Deprecated = deprecated() // expected-warning {{'deprecated()' is deprecated}}
}

@available(*, deprecated)
struct DeprecatedContainer {
  let always_var: AlwaysAvailable = always()
  let never_var: NeverAvailable = never() // expected-error {{'never()' is unavailable}}
  // expected-error@-1 {{'NeverAvailable' is unavailable}}
  let unavailable_in_swift4_var: UnavailableInSwift4 = unavailableInSwift4() // expected-error {{'unavailableInSwift4()' is unavailable}}
  // expected-error@-1 {{'UnavailableInSwift4' is unavailable}}
  let available_in_future_swift_var: AvailableInFutureSwift = availableInFutureSwift() // expected-error {{'availableInFutureSwift()' is unavailable}}
  // expected-error@-1 {{'AvailableInFutureSwift' is unavailable}}
  let deprecated_var: Deprecated = deprecated()
}

// MARK: Extensions

extension AlwaysAvailableContainer {}
extension NeverAvailableContainer {} // expected-error {{'NeverAvailableContainer' is unavailable}}
extension UnavailableInSwift4Container {} // expected-error {{'UnavailableInSwift4Container' is unavailable}}
extension AvailableInFutureSwiftContainer {} // expected-error {{'AvailableInFutureSwiftContainer' is unavailable}}
extension DeprecatedContainer {} // expected-warning {{'DeprecatedContainer' is deprecated}}

@available(*, unavailable)
extension AlwaysAvailableContainer {}
@available(*, unavailable)
extension NeverAvailableContainer {}
@available(*, unavailable)
extension UnavailableInSwift4Container {}
@available(*, unavailable)
extension AvailableInFutureSwiftContainer {}
@available(*, unavailable)
extension DeprecatedContainer {}

@available(swift, obsoleted: 4)
extension AlwaysAvailableContainer {}
@available(swift, obsoleted: 4)
extension NeverAvailableContainer {} // expected-error {{'NeverAvailableContainer' is unavailable}}
@available(swift, obsoleted: 4)
extension UnavailableInSwift4Container {}
@available(swift, obsoleted: 4)
extension AvailableInFutureSwiftContainer {}
@available(swift, obsoleted: 4)
extension DeprecatedContainer {}

@available(swift, introduced: 99)
extension AlwaysAvailableContainer {}
@available(swift, introduced: 99)
extension NeverAvailableContainer {} // expected-error {{'NeverAvailableContainer' is unavailable}}
@available(swift, introduced: 99)
extension UnavailableInSwift4Container {}
@available(swift, introduced: 99)
extension AvailableInFutureSwiftContainer {}
@available(swift, introduced: 99)
extension DeprecatedContainer {}

@available(*, deprecated)
extension AlwaysAvailableContainer {}
@available(*, deprecated)
extension NeverAvailableContainer {} // expected-error {{'NeverAvailableContainer' is unavailable}}
@available(*, deprecated)
extension UnavailableInSwift4Container {} // expected-error {{'UnavailableInSwift4Container' is unavailable}}
@available(*, deprecated)
extension AvailableInFutureSwiftContainer {} // expected-error {{'AvailableInFutureSwiftContainer' is unavailable}}
@available(*, deprecated)
extension DeprecatedContainer {}

struct ExtendMe {}

@available(*, unavailable)
extension ExtendMe {
  func never_available_extension_available_method() {} // expected-note * {{has been explicitly marked unavailable here}}

  typealias AvailableAliasInNeverAvailableExtension = Self // expected-note * {{'AvailableAliasInNeverAvailableExtension' has been explicitly marked unavailable here}}

  func never_available_extension_available_method( // expected-note * {{add '@available' attribute to enclosing instance method}}
    _: AlwaysAvailable,
    _: NeverAvailable,
    _: UnavailableInSwift4,
    _: AvailableInFutureSwift,
    _: Deprecated,
  ) {
    always()
    never() // expected-error {{'never()' is unavailable}}
    unavailableInSwift4() // expected-error {{'unavailableInSwift4()' is unavailable}}
    availableInFutureSwift() // expected-error {{'availableInFutureSwift()' is unavailable in Swift}}
    deprecated() // expected-warning {{'deprecated()' is deprecated}}
  }

  @available(*, unavailable)
  func never_available_extension_never_available_method(
    _: AlwaysAvailable,
    _: NeverAvailable,
    _: UnavailableInSwift4,
    _: AvailableInFutureSwift,
    _: Deprecated,
  ) {
    always()
    never() // expected-error {{'never()' is unavailable}}
    unavailableInSwift4() // expected-error {{'unavailableInSwift4()' is unavailable}}
    availableInFutureSwift() // expected-error {{'availableInFutureSwift()' is unavailable in Swift}}
    deprecated() // expected-warning {{'deprecated()' is deprecated}}
  }

  @available(swift, obsoleted: 4)
  func never_available_extension_unavailable_in_swift4_method(
    _: AlwaysAvailable,
    _: NeverAvailable,
    _: UnavailableInSwift4,
    _: AvailableInFutureSwift,
    _: Deprecated,
  ) {
    always()
    never() // expected-error {{'never()' is unavailable}}
    unavailableInSwift4() // expected-error {{'unavailableInSwift4()' is unavailable}}
    availableInFutureSwift() // expected-error {{'availableInFutureSwift()' is unavailable in Swift}}
    deprecated() // expected-warning {{'deprecated()' is deprecated}}
  }

  @available(swift, introduced: 99)
  func never_available_extension_available_in_future_swift_method(
    _: AlwaysAvailable,
    _: NeverAvailable,
    _: UnavailableInSwift4,
    _: AvailableInFutureSwift,
    _: Deprecated,
  ) {
    always()
    never() // expected-error {{'never()' is unavailable}}
    unavailableInSwift4() // expected-error {{'unavailableInSwift4()' is unavailable}}
    availableInFutureSwift() // expected-error {{'availableInFutureSwift()' is unavailable in Swift}}
    deprecated() // expected-warning {{'deprecated()' is deprecated}}
  }

  @available(*, deprecated)
  func never_available_extension_deprecated_method(
    _: AlwaysAvailable,
    _: NeverAvailable,
    _: UnavailableInSwift4,
    _: AvailableInFutureSwift,
    _: Deprecated,
  ) {
    always()
    never() // expected-error {{'never()' is unavailable}}
    unavailableInSwift4() // expected-error {{'unavailableInSwift4()' is unavailable}}
    availableInFutureSwift() // expected-error {{'availableInFutureSwift()' is unavailable in Swift}}
    deprecated()
  }
}

@available(swift, obsoleted: 4)
extension ExtendMe {
  func unavailable_in_swift4_extension_available_method() {} // expected-note * {{'unavailable_in_swift4_extension_available_method()' was obsoleted in Swift 4}}

  typealias AvailableAliasInSwift4ObsoletedExtension = Self // expected-note * {{'AvailableAliasInSwift4ObsoletedExtension' was obsoleted in Swift 4}}

  @available(*, unavailable)
  func unavailable_in_swift4_extension_never_available_method(
    _: AlwaysAvailable,
    _: NeverAvailable,
    _: UnavailableInSwift4,
    _: AvailableInFutureSwift,
    _: Deprecated,
  ) {
    always()
    never() // expected-error {{'never()' is unavailable}}
    unavailableInSwift4() // expected-error {{'unavailableInSwift4()' is unavailable}}
    availableInFutureSwift() // expected-error {{'availableInFutureSwift()' is unavailable in Swift}}
    deprecated() // expected-warning {{'deprecated()' is deprecated}}
  }
}

@available(swift, introduced: 99)
extension ExtendMe {
  func available_in_future_swift_extension_available_method() {} // expected-note * {{'available_in_future_swift_extension_available_method()' was introduced in Swift 99}}

  typealias AvailableAliasInSwift99Extension = Self // expected-note * {{'AvailableAliasInSwift99Extension' was introduced in Swift 99}}

  @available(*, unavailable)
  func available_in_future_swift_extension_never_available_method(
    _: AlwaysAvailable,
    _: NeverAvailable,
    _: UnavailableInSwift4,
    _: AvailableInFutureSwift,
    _: Deprecated,
  ) {
    always()
    never() // expected-error {{'never()' is unavailable}}
    unavailableInSwift4() // expected-error {{'unavailableInSwift4()' is unavailable}}
    availableInFutureSwift() // expected-error {{'availableInFutureSwift()' is unavailable in Swift}}
    deprecated() // expected-warning {{'deprecated()' is deprecated}}
  }
}

@available(*, deprecated)
extension ExtendMe {
  func deprecated_extension_available_method() {} // expected-note * {{'deprecated_extension_available_method()' is deprecated}}

  typealias AvailableAliasInDeprecatedExtension = Self // expected-note * {{'AvailableAliasInDeprecatedExtension' is deprecated}}

  @available(*, unavailable)
  func deprecated_extension_never_available_method(
    _: AlwaysAvailable,
    _: NeverAvailable,
    _: UnavailableInSwift4,
    _: AvailableInFutureSwift,
    _: Deprecated,
  ) {
    always()
    never() // expected-error {{'never()' is unavailable}}
    unavailableInSwift4() // expected-error {{'unavailableInSwift4()' is unavailable}}
    availableInFutureSwift() // expected-error {{'availableInFutureSwift()' is unavailable in Swift}}
    deprecated()
  }
}

func available_func_call_extension_methods(
  _ e: ExtendMe,
  _: ExtendMe.AvailableAliasInNeverAvailableExtension, // expected-error {{'AvailableAliasInNeverAvailableExtension' is unavailable}}
  _: ExtendMe.AvailableAliasInSwift4ObsoletedExtension, // expected-error {{'AvailableAliasInSwift4ObsoletedExtension' is unavailable}}
  _: ExtendMe.AvailableAliasInSwift99Extension, // expected-error {{'AvailableAliasInSwift99Extension' is unavailable in Swift}}
  _: ExtendMe.AvailableAliasInDeprecatedExtension, // expected-warning {{'AvailableAliasInDeprecatedExtension' is deprecated}}
) {
  e.never_available_extension_available_method() // expected-error {{'never_available_extension_available_method()' is unavailable}}
  e.unavailable_in_swift4_extension_available_method() //  expected-error {{'unavailable_in_swift4_extension_available_method()' is unavailable}}
  e.available_in_future_swift_extension_available_method() //  expected-error {{'available_in_future_swift_extension_available_method()' is unavailable}}
  e.deprecated_extension_available_method() // expected-warning {{'deprecated_extension_available_method()' is deprecated}}
}

@available(*, unavailable)
func unavailable_func_call_extension_methods(
  _ e: ExtendMe,
  _: ExtendMe.AvailableAliasInNeverAvailableExtension,
  _: ExtendMe.AvailableAliasInSwift4ObsoletedExtension,
  _: ExtendMe.AvailableAliasInSwift99Extension,
  _: ExtendMe.AvailableAliasInDeprecatedExtension,
) {
  e.never_available_extension_available_method() // expected-error {{'never_available_extension_available_method()' is unavailable}}
  e.unavailable_in_swift4_extension_available_method() //  expected-error {{'unavailable_in_swift4_extension_available_method()' is unavailable}}
  e.available_in_future_swift_extension_available_method() //  expected-error {{'available_in_future_swift_extension_available_method()' is unavailable}}
  e.deprecated_extension_available_method() // expected-warning {{'deprecated_extension_available_method()' is deprecated}}
}

@available(*, deprecated)
func deprecated_func_call_extension_methods(
  _ e: ExtendMe,
  _: ExtendMe.AvailableAliasInNeverAvailableExtension, // expected-error {{'AvailableAliasInNeverAvailableExtension' is unavailable}}
  _: ExtendMe.AvailableAliasInSwift4ObsoletedExtension, // expected-error {{'AvailableAliasInSwift4ObsoletedExtension' is unavailable}}
  _: ExtendMe.AvailableAliasInSwift99Extension, // expected-error {{'AvailableAliasInSwift99Extension' is unavailable in Swift}}
  _: ExtendMe.AvailableAliasInDeprecatedExtension,
) {
  e.never_available_extension_available_method() // expected-error {{'never_available_extension_available_method()' is unavailable}}
  e.unavailable_in_swift4_extension_available_method() //  expected-error {{'unavailable_in_swift4_extension_available_method()' is unavailable}}
  e.available_in_future_swift_extension_available_method() //  expected-error {{'available_in_future_swift_extension_available_method()' is unavailable}}
  e.deprecated_extension_available_method()
}
