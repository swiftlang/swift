// RUN: %target-typecheck-verify-swift -parse-as-library -enable-experimental-feature SwiftRuntimeAvailability

// REQUIRES: swift_feature_SwiftRuntimeAvailability

@available(swift 99) // expected-warning {{'swift' has been renamed to 'SwiftLanguageMode'}}{{12-17=SwiftLanguageMode}}
func availableInSwift99() { }
// expected-note@-1 {{'availableInSwift99()' was introduced in Swift 99}}

@available(SwiftLanguageMode 99)
func availableInSwift99LanguageMode() { }
// expected-note@-1 {{'availableInSwift99LanguageMode()' was introduced in Swift 99}}

func testUses() {
  availableInSwift99() // expected-error {{'availableInSwift99()' is unavailable in Swift}}
  availableInSwift99LanguageMode() // expected-error {{'availableInSwift99LanguageMode()' is unavailable in Swift}}
}
