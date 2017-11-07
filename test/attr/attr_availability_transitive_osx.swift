// RUN: %target-typecheck-verify-swift
// REQUIRES: OS=macosx

// Allow referencing unavailable API in situations where the caller is marked unavailable in the same circumstances.

@available(OSX, unavailable)
func osx() {} // expected-note 2{{'osx()' has been explicitly marked unavailable here}}

@available(OSXApplicationExtension, unavailable)
func osx_extension() {}

func call_osx_extension() {
    osx_extension() // OK; osx_extension is only unavailable if -application-extension is passed.
}
func call_osx() {
    osx() // expected-error {{'osx()' is unavailable}}
}

@available(OSX, unavailable)
func osx_call_osx_extension() {
    osx_extension() // OK; osx_extension is only unavailable if -application-extension is passed.
}

@available(OSX, unavailable)
func osx_call_osx() {
    osx() // OK; same
}

@available(OSXApplicationExtension, unavailable)
func osx_extension_call_osx_extension() {
    osx_extension()
}

@available(OSXApplicationExtension, unavailable)
func osx_extension_call_osx() {
    osx() // expected-error {{'osx()' is unavailable}}
}
