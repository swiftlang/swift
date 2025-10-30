// RUN: %target-typecheck-verify-swift -parse-stdlib -enable-experimental-feature AnyAppleOSAvailability -target %target-cpu-apple-macos26 -verify-additional-prefix apple- -verify-additional-prefix macos-
// RUN: %target-typecheck-verify-swift -parse-stdlib -enable-experimental-feature AnyAppleOSAvailability -target %target-cpu-apple-ios26 -verify-additional-prefix apple- -verify-additional-prefix ios-
// RUN: %target-typecheck-verify-swift -parse-stdlib -enable-experimental-feature AnyAppleOSAvailability -target %target-cpu-apple-watchos26 -verify-additional-prefix apple- -verify-additional-prefix watchos-
// RUN: %target-typecheck-verify-swift -parse-stdlib -enable-experimental-feature AnyAppleOSAvailability -target %target-cpu-apple-tvos26 -verify-additional-prefix apple- -verify-additional-prefix tvos-
// RUN: %target-typecheck-verify-swift -parse-stdlib -enable-experimental-feature AnyAppleOSAvailability -target %target-cpu-apple-visionos26 -verify-additional-prefix apple- -verify-additional-prefix visionos-
// RUN: %target-typecheck-verify-swift -parse-stdlib -enable-experimental-feature AnyAppleOSAvailability -target x86_64-unknown-linux-gnu
// RUN: %target-typecheck-verify-swift -parse-stdlib -enable-experimental-feature AnyAppleOSAvailability -target x86_64-unknown-windows-msvc

// REQUIRES: swift_feature_AnyAppleOSAvailability

@available(anyAppleOS 26.1, *)
func availableInAnyAppleOS26_1() { }

@available(anyAppleOS, deprecated: 26)
func deprecatedInAnyAppleOS26() { }

@available(anyAppleOS, obsoleted: 26)
func obsoletedInAnyAppleOS26() { }
// expected-macos-note@-1 {{'obsoletedInAnyAppleOS26()' was obsoleted in macOS 26}}
// expected-ios-note@-2 {{'obsoletedInAnyAppleOS26()' was obsoleted in iOS 26}}
// expected-watchos-note@-3 {{'obsoletedInAnyAppleOS26()' was obsoleted in watchOS 26}}
// expected-tvos-note@-4 {{'obsoletedInAnyAppleOS26()' was obsoleted in tvOS 26}}
// expected-visionos-note@-5 {{'obsoletedInAnyAppleOS26()' was obsoleted in visionOS 26}}

@available(anyAppleOS 26, macOS 26.1, *)
func availableInAnyAppleOS26AndMacOS26_1() { }

@available(macOS 26.1, anyAppleOS 26, *)
func availableInMacOS26_1AndAnyAppleOS26() { }

@available(macOS 26.1, iOS 26.1, watchOS 26.1, tvOS 26.1, visionOS 26.1, *)
func availableInEveryAppleOS26_1() { }

@available(anyAppleOS, unavailable)
func unavailableInAnyAppleOS() { } // expected-apple-note {{'unavailableInAnyAppleOS()' has been explicitly marked unavailable here}}

// FIXME: [availability] Ensure the fix-it suggests @available(anyAppleOS ...) rdar://163819878
func availableAtDeploymentTarget() {
  // expected-apple-note@-1 {{add '@available' attribute to enclosing global function}}
  // expected-macos-note@-2 2 {{add '@available' attribute to enclosing global function}}

  // FIXME: [availability] Ensure the fix-it suggests if #available(anyAppleOS ...) rdar://163819878
  availableInAnyAppleOS26_1()
  // expected-macos-error@-1 {{'availableInAnyAppleOS26_1()' is only available in macOS 26.1 or newer}}
  // expected-ios-error@-2 {{'availableInAnyAppleOS26_1()' is only available in iOS 26.1 or newer}}
  // expected-watchos-error@-3 {{'availableInAnyAppleOS26_1()' is only available in watchOS 26.1 or newer}}
  // expected-tvos-error@-4 {{'availableInAnyAppleOS26_1()' is only available in tvOS 26.1 or newer}}
  // expected-visionos-error@-5 {{'availableInAnyAppleOS26_1()' is only available in visionOS 26.1 or newer}}
  // expected-apple-note@-6 {{add 'if #available' version check}}

  // FIXME: [availability] Remap domain/version in deprecation diagnostics
  deprecatedInAnyAppleOS26()
  // expected-apple-warning@-1 {{'deprecatedInAnyAppleOS26()' was deprecated in any Apple OS 26}}

  obsoletedInAnyAppleOS26()
  // expected-macos-error@-1 {{'obsoletedInAnyAppleOS26()' is unavailable in macOS}}
  // expected-ios-error@-2 {{'obsoletedInAnyAppleOS26()' is unavailable in iOS}}
  // expected-watchos-error@-3 {{'obsoletedInAnyAppleOS26()' is unavailable in watchOS}}
  // expected-tvos-error@-4 {{'obsoletedInAnyAppleOS26()' is unavailable in tvOS}}
  // expected-visionos-error@-5 {{'obsoletedInAnyAppleOS26()' is unavailable in visionOS}}

  availableInAnyAppleOS26AndMacOS26_1()
  // expected-macos-error@-1 {{'availableInAnyAppleOS26AndMacOS26_1()' is only available in macOS 26.1 or newer}}
  // expected-macos-note@-2 {{add 'if #available' version check}}{{3-40=if #available(macOS 26.1, *) {\n      availableInAnyAppleOS26AndMacOS26_1()\n  \} else {\n      // Fallback on earlier versions\n  \}}}
  availableInMacOS26_1AndAnyAppleOS26()
  // expected-macos-error@-1 {{'availableInMacOS26_1AndAnyAppleOS26()' is only available in macOS 26.1 or newer}}
  // expected-macos-note@-2 {{add 'if #available' version check}}{{3-40=if #available(macOS 26.1, *) {\n      availableInMacOS26_1AndAnyAppleOS26()\n  \} else {\n      // Fallback on earlier versions\n  \}}}

  unavailableInAnyAppleOS()
  // expected-macos-error@-1 {{'unavailableInAnyAppleOS()' is unavailable in macOS}}
  // expected-ios-error@-2 {{'unavailableInAnyAppleOS()' is unavailable in iOS}}
  // expected-watchos-error@-3 {{'unavailableInAnyAppleOS()' is unavailable in watchOS}}
  // expected-tvos-error@-4 {{'unavailableInAnyAppleOS()' is unavailable in tvOS}}
  // expected-visionos-error@-5 {{'unavailableInAnyAppleOS()' is unavailable in visionOS}}

  if #available(anyAppleOS 25, *) { } // expected-warning {{'25' is not a valid version number for any Apple OS}}

  if #available(anyAppleOS 26.1, *) {
    availableInAnyAppleOS26_1()
    availableInEveryAppleOS26_1()
    availableInAnyAppleOS26AndMacOS26_1()
    availableInMacOS26_1AndAnyAppleOS26()
  }
  if #available(macOS 26.1, *) {
    availableInAnyAppleOS26AndMacOS26_1()
    availableInMacOS26_1AndAnyAppleOS26()
  }
  if #available(macOS 26.1, iOS 26.1, watchOS 26.1, tvOS 26.1, visionOS 26.1, *) {
    availableInAnyAppleOS26_1()
    availableInEveryAppleOS26_1()
  }
}

@available(anyAppleOS 26.1, *)
struct AvailableInAnyAppleOS26_1 {
  func method() {
    availableInAnyAppleOS26_1()
  }
}

@available(anyAppleOS, unavailable)
func alsoUnavailableInAnyAppleOS() {
  unavailableInAnyAppleOS()
}
