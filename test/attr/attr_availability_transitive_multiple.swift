// RUN: %target-typecheck-verify-swift
// REQUIRES: OS=macosx

// Check that only the platform we are compiling for is considered when allowing access to unavailable declarations.

@available(iOS, unavailable)
@available(OSX, unavailable)
func unavailable() {} // expected-note {{'unavailable()' has been explicitly marked unavailable here}}

@available(iOS, unavailable)
func call_with_ios_unavailable() {
	unavailable() // expected-error {{'unavailable()' is unavailable}}
}

@available(OSX, unavailable)
func call_with_osx_unavailable() {
	unavailable() // OK: same
}
