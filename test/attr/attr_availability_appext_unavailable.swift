// Verify that declarations unavailable to application extensions are diagnosed
// as unavailable when compiling with `-application-extension`
// RUN: %target-typecheck-verify-swift -application-extension -verify-additional-prefix %target-os-

// Remove `-application-extension` and verify no errors are emitted.
// RUN: %target-swift-frontend -typecheck %s

// REQUIRES: OS=macosx || OS=ios || OS=tvos || OS=watchos

// The note points at whichever attribute makes the declaration unavailable on
// the target platform.
@available(macOSApplicationExtension, unavailable) // expected-macosx-note {{'unavailableToExtensions()' has been explicitly marked unavailable here}}
@available(macCatalystApplicationExtension, unavailable) // expected-maccatalyst-note {{'unavailableToExtensions()' has been explicitly marked unavailable here}}
@available(iOSApplicationExtension, unavailable) // expected-ios-note {{'unavailableToExtensions()' has been explicitly marked unavailable here}}
@available(tvOSApplicationExtension, unavailable) // expected-tvos-note {{'unavailableToExtensions()' has been explicitly marked unavailable here}}
@available(watchOSApplicationExtension, unavailable) // expected-watchos-note {{'unavailableToExtensions()' has been explicitly marked unavailable here}}
func unavailableToExtensions() {}

func alwaysAvailable() {
  unavailableToExtensions() // expected-error {{'unavailableToExtensions()' is unavailable in application extensions for}}
}
