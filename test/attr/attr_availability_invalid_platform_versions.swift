// RUN: %target-swift-frontend -typecheck -verify -parse-stdlib -module-name Swift %s

@available(macOS, introduced: 17) // expected-warning {{'17' is not a valid version number for macOS}}
@available(iOS, introduced: 20) // expected-warning {{'20' is not a valid version number for iOS}}
@available(macCatalyst, introduced: 20) // expected-warning {{'20' is not a valid version number for Mac Catalyst}}
@available(watchOS, introduced: 13) // expected-warning {{'13' is not a valid version number for watchOS}}
@available(tvOS, introduced: 20) // expected-warning {{'20' is not a valid version number for tvOS}}
@available(visionOS, introduced: 4) // expected-warning {{'4' is not a valid version number for visionOS}}
func invalidIntroduced() { }

@available(macOS, deprecated: 17) // expected-warning {{'17' is not a valid version number for macOS}}
@available(iOS, deprecated: 20) // expected-warning {{'20' is not a valid version number for iOS}}
@available(macCatalyst, deprecated: 20) // expected-warning {{'20' is not a valid version number for Mac Catalyst}}
@available(watchOS, deprecated: 13) // expected-warning {{'13' is not a valid version number for watchOS}}
@available(tvOS, deprecated: 20) // expected-warning {{'20' is not a valid version number for tvOS}}
@available(visionOS, deprecated: 4) // expected-warning {{'4' is not a valid version number for visionOS}}
func invalidDeprecated() { }

@available(macOS, obsoleted: 17) // expected-warning {{'17' is not a valid version number for macOS}}
@available(iOS, obsoleted: 20) // expected-warning {{'20' is not a valid version number for iOS}}
@available(macCatalyst, obsoleted: 20) // expected-warning {{'20' is not a valid version number for Mac Catalyst}}
@available(watchOS, obsoleted: 13) // expected-warning {{'13' is not a valid version number for watchOS}}
@available(tvOS, obsoleted: 20) // expected-warning {{'20' is not a valid version number for tvOS}}
@available(visionOS, obsoleted: 4) // expected-warning {{'4' is not a valid version number for visionOS}}
func invalidObsoleted() { }

@available(macOS 18, iOS 21, macCatalyst 22, watchOS 14, tvOS 23, visionOS 7, *)
// expected-warning@-1 {{'18' is not a valid version number for macOS}}
// expected-warning@-2 {{'21' is not a valid version number for iOS}}
// expected-warning@-3 {{'22' is not a valid version number for Mac Catalyst}}
// expected-warning@-4 {{'14' is not a valid version number for watchOS}}
// expected-warning@-5 {{'23' is not a valid version number for tvOS}}
// expected-warning@-6 {{'7' is not a valid version number for visionOS}}
func invalidIntroducedShort() { }
