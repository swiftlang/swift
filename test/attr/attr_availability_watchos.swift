// RUN: %swift -typecheck -verify -parse-stdlib -target i386-apple-watchos2.0 %s

@available(watchOS, introduced: 1.0, deprecated: 1.5, obsoleted: 2.0,
              message: "you don't want to do that anyway")
func doSomething() { }
// expected-note @-1{{'doSomething()' was obsoleted in watchOS 2.0}}

doSomething() // expected-error{{'doSomething()' is unavailable in watchOS: you don't want to do that anyway}}

// Preservation of major.minor.micro
@available(watchOS, introduced: 1.0, deprecated: 1.5, obsoleted: 1.5.3)
func doSomethingElse() { }
// expected-note @-1{{'doSomethingElse()' was obsoleted in watchOS 1.5.3}}

doSomethingElse() // expected-error{{'doSomethingElse()' is unavailable in watchOS}}

// Preservation of minor-only version
@available(watchOS, introduced: 1.0, deprecated: 1.5, obsoleted: 2)
func doSomethingReallyOld() { }
// expected-note @-1{{'doSomethingReallyOld()' was obsoleted in watchOS 2}}

doSomethingReallyOld() // expected-error{{'doSomethingReallyOld()' is unavailable in watchOS}}

// Test deprecations in 2.0 and later

@available(watchOS, introduced: 1.1, deprecated: 2.0,
              message: "Use another function")
func deprecatedFunctionWithMessage() { }

deprecatedFunctionWithMessage() // expected-warning{{'deprecatedFunctionWithMessage()' was deprecated in watchOS 2.0: Use another function}}


@available(watchOS, introduced: 1.0, deprecated: 2.0)
func deprecatedFunctionWithoutMessage() { }

deprecatedFunctionWithoutMessage() // expected-warning{{'deprecatedFunctionWithoutMessage()' was deprecated in watchOS 2.0}}

@available(watchOS, introduced: 1.0, deprecated: 2.0,
              message: "Use BetterClass instead")
class DeprecatedClass { }

func functionWithDeprecatedParameter(p: DeprecatedClass) { } // expected-warning{{'DeprecatedClass' was deprecated in watchOS 2.0: Use BetterClass instead}}

@available(watchOS, introduced: 2.0, deprecated: 4.0,
              message: "Use BetterClass instead")
class DeprecatedClassIn3_0 { }

// Elements deprecated later than the minimum deployment target (which is 2.0, in this case) should not generate warnings
func functionWithDeprecatedLaterParameter(p: DeprecatedClassIn3_0) { }

// Treat watchOS as distinct from iOS in availability queries

@available(watchOS, introduced: 2.2)
func functionIntroducedOnwatchOS2_2() { }

if #available(iOS 9.3, *) {
  functionIntroducedOnwatchOS2_2() // expected-error {{'functionIntroducedOnwatchOS2_2()' is only available in watchOS 2.2 or newer}}
      // expected-note@-1 {{add 'if #available' version check}}
}

if #available(iOS 9.3, watchOS 2.1, *) {
  functionIntroducedOnwatchOS2_2() // expected-error {{'functionIntroducedOnwatchOS2_2()' is only available in watchOS 2.2 or newer}} {{-1:32-35=2.2}}
}

if #available(iOS 9.1, watchOS 2.2, *) {
  functionIntroducedOnwatchOS2_2()
}

if #available(iOS 8.0, watchOS 2.2, *) {
}

if #available(iOS 9.2, watchOS 1.0, *) { // no-warning
}


// Swift-originated iOS availability attributes should not be transcribed to watchOS

@available(iOS, unavailable)
func swiftOriginatedFunctionUnavailableOnIOS() { }

@available(iOS, introduced: 6.0, deprecated: 9.0)
func swiftOriginatedFunctionDeprecatedOnIOS() { }

@available(iOS, introduced: 10.0)
func swiftOriginatedFunctionPotentiallyUnavailableOnIOS() { }

func useSwiftOriginatedFunctions() {
  // We do not expect diagnostics here because iOS availability attributes coming from
  // Swift should not be transcribed to watchOS.
  swiftOriginatedFunctionUnavailableOnIOS()
  swiftOriginatedFunctionDeprecatedOnIOS()
  swiftOriginatedFunctionPotentiallyUnavailableOnIOS()
}
