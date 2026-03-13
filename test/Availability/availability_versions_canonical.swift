// RUN: %swift -typecheck %s -verify -parse-stdlib -module-name Swift -target x86_64-apple-macosx10.15 -verify-additional-prefix macos-
// RUN: %swift -typecheck %s -verify -parse-stdlib -module-name Swift -target arm64-apple-ios13 -verify-additional-prefix ios-
// RUN: %swift -typecheck %s -verify -parse-stdlib -module-name Swift -target arm64-apple-watchos6 -verify-additional-prefix watchos-
// RUN: %swift -typecheck %s -verify -parse-stdlib -module-name Swift -target arm64-apple-tvos13 -verify-additional-prefix tvos-
// RUN: %swift -typecheck %s -verify -parse-stdlib -module-name Swift -target arm64-apple-xros1 -verify-additional-prefix visionos-

@available(OSX 10.16, *)
func introducedOnMacOS10_16() { }

@available(OSX 11.0, *)
func introducedOnMacOS11_0() { }

@available(macOS 16.0, iOS 19.0, watchOS 12.0, tvOS 19.0, visionOS 3.0, *)
func introducedInVersionsMappingTo26_0() { }

@available(macOS 26.0, iOS 26.0, watchOS 26.0, tvOS 26.0, visionOS 26.0, *)
func introducedIn26_0() { }

@available(macOS 17.0, iOS 20.0, watchOS 13.0, tvOS 20.0, visionOS 4.0, *)
// expected-warning@-1 {{'17.0' is not a valid version number for macOS}}
// expected-warning@-2 {{'20.0' is not a valid version number for iOS}}
// expected-warning@-3 {{'13.0' is not a valid version number for watchOS}}
// expected-warning@-4 {{'20.0' is not a valid version number for tvOS}}
// expected-warning@-5 {{'4.0' is not a valid version number for visionOS}}
func introducedInVersionsMappingTo27_0() { }

@available(macOS 27.0, iOS 27.0, watchOS 27.0, tvOS 27.0, visionOS 27.0, *)
func introducedIn27_0() { }

func useUnderPoundAvailable() {
  // expected-note@-1 * {{add '@available' attribute to enclosing global function}}
  introducedOnMacOS10_16()
  // expected-macos-error@-1 {{'introducedOnMacOS10_16()' is only available in macOS 11.0 or newer}}
  // expected-macos-note@-2 {{add 'if #available' version check}}

  introducedOnMacOS11_0()
  // expected-macos-error@-1 {{'introducedOnMacOS11_0()' is only available in macOS 11.0 or newer}}
  // expected-macos-note@-2 {{add 'if #available' version check}}

  introducedInVersionsMappingTo26_0()
  // expected-macos-error@-1 {{'introducedInVersionsMappingTo26_0()' is only available in macOS 26.0 or newer}}
  // expected-ios-error@-2 {{'introducedInVersionsMappingTo26_0()' is only available in iOS 26.0 or newer}}
  // expected-watchos-error@-3 {{'introducedInVersionsMappingTo26_0()' is only available in watchOS 26.0 or newer}}
  // expected-tvos-error@-4 {{'introducedInVersionsMappingTo26_0()' is only available in tvOS 26.0 or newer}}
  // expected-visionos-error@-5 {{'introducedInVersionsMappingTo26_0()' is only available in visionOS 26.0 or newer}}
  // expected-note@-6 {{add 'if #available' version check}}

  introducedIn26_0()
  // expected-macos-error@-1 {{'introducedIn26_0()' is only available in macOS 26.0 or newer}}
  // expected-ios-error@-2 {{'introducedIn26_0()' is only available in iOS 26.0 or newer}}
  // expected-watchos-error@-3 {{'introducedIn26_0()' is only available in watchOS 26.0 or newer}}
  // expected-tvos-error@-4 {{'introducedIn26_0()' is only available in tvOS 26.0 or newer}}
  // expected-visionos-error@-5 {{'introducedIn26_0()' is only available in visionOS 26.0 or newer}}
  // expected-note@-6 {{add 'if #available' version check}}

  introducedInVersionsMappingTo27_0()
  // expected-macos-error@-1 {{'introducedInVersionsMappingTo27_0()' is only available in macOS 27.0 or newer}}
  // expected-ios-error@-2 {{'introducedInVersionsMappingTo27_0()' is only available in iOS 27.0 or newer}}
  // expected-watchos-error@-3 {{'introducedInVersionsMappingTo27_0()' is only available in watchOS 27.0 or newer}}
  // expected-tvos-error@-4 {{'introducedInVersionsMappingTo27_0()' is only available in tvOS 27.0 or newer}}
  // expected-visionos-error@-5 {{'introducedInVersionsMappingTo27_0()' is only available in visionOS 27.0 or newer}}
  // expected-note@-6 {{add 'if #available' version check}}

  introducedIn27_0()
  // expected-macos-error@-1 {{'introducedIn27_0()' is only available in macOS 27.0 or newer}}
  // expected-ios-error@-2 {{'introducedIn27_0()' is only available in iOS 27.0 or newer}}
  // expected-watchos-error@-3 {{'introducedIn27_0()' is only available in watchOS 27.0 or newer}}
  // expected-tvos-error@-4 {{'introducedIn27_0()' is only available in tvOS 27.0 or newer}}
  // expected-visionos-error@-5 {{'introducedIn27_0()' is only available in visionOS 27.0 or newer}}
  // expected-note@-6 {{add 'if #available' version check}}

  if #available(OSX 10.16, *) {
    introducedOnMacOS10_16()
    introducedOnMacOS11_0()
  }

  if #available(macOS 16.0, iOS 19.0, watchOS 12.0, tvOS 19.0, visionOS 3.0, *) {
    introducedInVersionsMappingTo26_0()
    introducedIn26_0()
    introducedInVersionsMappingTo27_0()
    // expected-macos-error@-1 {{'introducedInVersionsMappingTo27_0()' is only available in macOS 27.0 or newer}}
    // expected-ios-error@-2 {{'introducedInVersionsMappingTo27_0()' is only available in iOS 27.0 or newer}}
    // expected-watchos-error@-3 {{'introducedInVersionsMappingTo27_0()' is only available in watchOS 27.0 or newer}}
    // expected-tvos-error@-4 {{'introducedInVersionsMappingTo27_0()' is only available in tvOS 27.0 or newer}}
    // expected-visionos-error@-5 {{'introducedInVersionsMappingTo27_0()' is only available in visionOS 27.0 or newer}}
    // expected-note@-6 {{add 'if #available' version check}}
    introducedIn27_0()
    // expected-macos-error@-1 {{'introducedIn27_0()' is only available in macOS 27.0 or newer}}
    // expected-ios-error@-2 {{'introducedIn27_0()' is only available in iOS 27.0 or newer}}
    // expected-watchos-error@-3 {{'introducedIn27_0()' is only available in watchOS 27.0 or newer}}
    // expected-tvos-error@-4 {{'introducedIn27_0()' is only available in tvOS 27.0 or newer}}
    // expected-visionos-error@-5 {{'introducedIn27_0()' is only available in visionOS 27.0 or newer}}
    // expected-note@-6 {{add 'if #available' version check}}
  }

  if #available(macOS 17.0, iOS 20.0, watchOS 13.0, tvOS 20.0, visionOS 4.0, *) {
    // expected-warning@-1 {{'17.0' is not a valid version number for macOS}}
    // expected-warning@-2 {{'20.0' is not a valid version number for iOS}}
    // expected-warning@-3 {{'13.0' is not a valid version number for watchOS}}
    // expected-warning@-4 {{'20.0' is not a valid version number for tvOS}}
    // expected-warning@-5 {{'4.0' is not a valid version number for visionOS}}
    introducedInVersionsMappingTo27_0()
    introducedIn27_0()
  }
}
