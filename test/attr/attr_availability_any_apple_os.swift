// RUN: %target-typecheck-verify-swift -swift-version 5 -parse-as-library -enable-experimental-feature AnyAppleOSAvailability

// REQUIRES: swift_feature_AnyAppleOSAvailability

@available(anyAppleOS 26, *)
func availableIn26Short() { }

@available(anyAppleOS 26.0, *)
func availableIn26_0Short() { }

@available(AnyAppleOS 26, *) // expected-warning {{unrecognized platform name 'AnyAppleOS'; did you mean 'anyAppleOS'}}
func miscapitalized() { }

@available(anyAppleOS 25, *) // expected-warning {{'25' is not a valid version number for any Apple OS}}
func availableIn25Short() { }

@available(anyAppleOS 26, macOS 26, iOS 26, watchOS 26, tvOS 26, visionOS 26, *)
func availableIn26ShortWithPlatforms() { }

@available(anyAppleOS, introduced: 26)
func availableIn26() { }

@available(anyAppleOS, introduced: 26.0)
func availableIn26_0() { }

@available(anyAppleOS, obsoleted: 26)
func obsoletedIn26() { }

@available(anyAppleOS, deprecated: 26)
func deprecatedIn26() { }

@available(anyAppleOS, deprecated)
func deprecated() { }

@available(anyAppleOS, unavailable)
func unavailable() { }
