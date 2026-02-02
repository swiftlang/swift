// RUN: %target-typecheck-verify-swift -swift-version 5 -parse-as-library -enable-experimental-feature SwiftRuntimeAvailability
// RUN: %target-typecheck-verify-swift -swift-version 5 -parse-as-library -enable-experimental-feature StandaloneSwiftAvailability

// REQUIRES: swift_feature_SwiftRuntimeAvailability
// REQUIRES: swift_feature_StandaloneSwiftAvailability

@available(Swift 6, *)
func availableInSwiftRuntime6Short() { }

@available(Swift 6.0, *)
func availableInSwiftRuntime6_0Short() { }

@available(Swift 6, macOS 15, iOS 18, watchOS 11, tvOS 18, visionOS 2, *)
func availableInSwiftRuntime6ShortWithPlatforms() { }

@available(Swift, introduced: 6)
func availableInSwiftRuntime6() { }

@available(Swift, introduced: 6.0)
func availableInSwiftRuntime6_0() { }

@available(Swift, introduced: 5.1, obsoleted: 6)
func availableInSwiftRuntime5_1Thru6() { }

@available(Swift, deprecated: 5.9)
func deprecatedInSwiftRuntime5_9() { }

@available(Swift 4, *) // expected-warning {{'4' is not a valid version number for Swift}}
func availableInSwiftRuntime4() { }

@available(Swift 4.9, *) // expected-warning {{'4.9' is not a valid version number for Swift}}
func availableInSwiftRuntime4_9() { }

// Swift 5.0 is the earliest possible runtime version
@available(Swift 5, *)
func availableInSwiftRuntime5() { }

@available(Swift, unavailable) // expected-warning {{'unavailable' cannot be used in '@available' attribute for Swift}}
func unavailableInSwiftRuntime() { }

// MARK: Swift language mode

@available(swift 6) // expected-warning {{'swift' has been renamed to 'SwiftLanguageMode'}}{{12-17=SwiftLanguageMode}}
func swift6OldSpellingShort() { }

@available(swift, introduced: 6) // expected-warning {{'swift' has been renamed to 'SwiftLanguageMode'}}{{12-17=SwiftLanguageMode}}
func swift6OldSpelling() { }

@available(SwiftLanguageMode 6)
func swiftLanguageModeShort6() { }
