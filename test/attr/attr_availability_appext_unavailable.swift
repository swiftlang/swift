// Verify that declarations unavailable to application extensions are diagnosed
// as unavailable when compiling with `-application-extension`
// RUN: %target-typecheck-verify-swift -application-extension

// Remove `-application-extension` and verify no errors are emitted.
// RUN: %target-swift-frontend -typecheck %s

// REQUIRES: OS=macosx || OS=ios || OS=tvos || OS=watchos

@available(macOSApplicationExtension, unavailable)
@available(macCatalystApplicationExtension, unavailable)
@available(iOSApplicationExtension, unavailable)
@available(tvOSApplicationExtension, unavailable)
@available(watchOSApplicationExtension, unavailable)
func unavailableToExtensions() {} // expected-note {{'unavailableToExtensions()' has been explicitly marked unavailable here}}

func alwaysAvailable() {
  unavailableToExtensions() // expected-error {{'unavailableToExtensions()' is unavailable in application extensions for}}
}
