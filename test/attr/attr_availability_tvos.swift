// RUN: %swift -parse -verify -parse-stdlib -target i386-apple-tvos9.0 %s

// REQUIRES: enable_target_appletvos

@available(tvOS, introduced=1.0, deprecated=2.0, obsoleted=9.0,
              message="you don't want to do that anyway")
func doSomething() { }
// expected-note @-1{{'doSomething()' was obsoleted in tvOS 9.0}}

doSomething() // expected-error{{'doSomething()' is unavailable: you don't want to do that anyway}}

// Preservation of major.minor.micro
@available(tvOS, introduced=1.0, deprecated=2.0, obsoleted=8.0)
func doSomethingElse() { }
// expected-note @-1{{'doSomethingElse()' was obsoleted in tvOS 8.0}}

doSomethingElse() // expected-error{{'doSomethingElse()' is unavailable}}

// Preservation of minor-only version
@available(tvOS, introduced=1.0, deprecated=1.5, obsoleted=9)
func doSomethingReallyOld() { }
// expected-note @-1{{'doSomethingReallyOld()' was obsoleted in tvOS 9}}

doSomethingReallyOld() // expected-error{{'doSomethingReallyOld()' is unavailable}}

// Test deprecations in 9.0 and later

@available(tvOS, introduced=1.1, deprecated=9.0,
              message="Use another function")
func deprecatedFunctionWithMessage() { }

deprecatedFunctionWithMessage() // expected-warning{{'deprecatedFunctionWithMessage()' was deprecated in tvOS 9.0: Use another function}}


@available(tvOS, introduced=1.0, deprecated=9.0)
func deprecatedFunctionWithoutMessage() { }

deprecatedFunctionWithoutMessage() // expected-warning{{'deprecatedFunctionWithoutMessage()' was deprecated in tvOS 9.0}}

@available(tvOS, introduced=1.0, deprecated=9.0,
              message="Use BetterClass instead")
class DeprecatedClass { }

func functionWithDeprecatedParameter(p: DeprecatedClass) { } // expected-warning{{'DeprecatedClass' was deprecated in tvOS 9.0: Use BetterClass instead}}

@available(tvOS, introduced=7.0, deprecated=10.0,
              message="Use BetterClass instead")
class DeprecatedClassIn8_0 { }

// Elements deprecated later than the minimum deployment target (which is 9.0, in this case) should not generate warnings
func functionWithDeprecatedLaterParameter(p: DeprecatedClassIn8_0) { }

// Treat tvOS as distinct from iOS in availability queries

@available(tvOS, introduced=9.2)
func functionIntroducedOntvOS9_2() { }

if #available(iOS 9.3, *) {
  functionIntroducedOntvOS9_2() // expected-error {{'functionIntroducedOntvOS9_2()' is only available on tvOS 9.2 or newer}}
      // expected-note@-1 {{guard with version check}}
}

if #available(iOS 9.3, tvOS 9.1, *) {
  functionIntroducedOntvOS9_2() // expected-error {{'functionIntroducedOntvOS9_2()' is only available on tvOS 9.2 or newer}}
      // expected-note@-1 {{guard with version check}}
}

if #available(iOS 9.1, tvOS 9.2, *) {
  functionIntroducedOntvOS9_2()
}

if #available(iOS 8.0, tvOS 9.2, *) {
}

@available(iOS 9.0, tvOS 9.0, *)
func availableOn9_0() { // expected-note {{enclosing scope here}}
  if #available(iOS 9.2, tvOS 8.0, *) { // expected-warning {{unnecessary check for 'tvOS'; enclosing scope ensures guard will always be true}}
  }
}

// Swift-originated iOS availability attributes should not be transcribed to tvOS

@available(iOS, unavailable)
func swiftOriginatedFunctionUnavailableOnIOS() { }

@available(iOS, introduced=6.0, deprecated=9.0)
func swiftOriginatedFunctionDeprecatedOnIOS() { }

@available(iOS, introduced=10.0)
func swiftOriginatedFunctionPotentiallyUnavailableOnIOS() { }

func useSwiftOriginatedFunctions() {
  // We do not expect diagnostics here because iOS availability attributes coming from
  // Swift should not be transcribed to tvOS.
  swiftOriginatedFunctionUnavailableOnIOS()
  swiftOriginatedFunctionDeprecatedOnIOS()
  swiftOriginatedFunctionPotentiallyUnavailableOnIOS()
}
