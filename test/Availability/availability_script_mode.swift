// RUN: %target-typecheck-verify-swift -target %target-cpu-apple-macosx50

// REQUIRES: OS=macosx

struct AlwaysAvailable {}

@available(macOS, introduced: 50)
struct Available50 {}

@available(macOS, introduced: 51)
struct Available51 {}

@available(macOS, unavailable)
struct UnavailableOnMacOS {} // expected-note {{'UnavailableOnMacOS' has been explicitly marked unavailable here}}

@available(*, unavailable)
struct UnavailableUnconditionally {} // expected-note {{'UnavailableUnconditionally' has been explicitly marked unavailable here}}

var alwaysAvailableVar: AlwaysAvailable = .init() // Ok

@available(macOS, introduced: 50)
var availableOn50Var: Available50 = .init() // Ok

// Script-mode globals have eagerly executed initializers so it isn't safe for
// them to be unavailable.

@available(macOS, introduced: 51) // expected-error {{global variable cannot be marked potentially unavailable with '@available' in script mode}}
var potentiallyUnavailableVar: Available51 = .init()

@available(macOS, unavailable) // expected-error {{global variable cannot be marked unavailable with '@available' in script mode}}
var unavailableOnMacOSVar: UnavailableOnMacOS = .init() // expected-error {{'UnavailableOnMacOS' is unavailable in macOS}}

@available(*, unavailable) // expected-error {{global variable cannot be marked unavailable with '@available' in script mode}}
var unconditionallyUnavailableVar: UnavailableUnconditionally = .init() // expected-error {{'UnavailableUnconditionally' is unavailable}}
