// RUN: %target-typecheck-verify-swift -target %target-cpu-apple-macosx10.50

// REQUIRES: OS=macosx

struct AlwaysAvailable {}

@available(macOS, introduced: 10.50)
struct Available10_50 {}

@available(macOS, introduced: 10.51)
struct Available10_51 {}

@available(macOS, unavailable)
struct UnavailableOnMacOS {}

@available(*, unavailable)
struct UnavailableUnconditionally {}

var alwaysAvailableVar: AlwaysAvailable = .init() // Ok

@available(macOS, introduced: 10.50)
var availableOn10_50Var: Available10_50 = .init() // Ok

// Script-mode globals have eagerly executed initializers so it isn't safe for
// them to be unavailable.

@available(macOS, introduced: 10.51) // expected-error {{global variable cannot be marked potentially unavailable with '@available' in script mode}}
var potentiallyUnavailableVar: Available10_51 = .init()

@available(macOS, unavailable) // expected-error {{global variable cannot be marked unavailable with '@available' in script mode}}
var unavailableOnMacOSVar: UnavailableOnMacOS = .init()

@available(*, unavailable) // expected-error {{global variable cannot be marked unavailable with '@available' in script mode}}
var unconditionallyUnavailableVar: UnavailableUnconditionally = .init()
