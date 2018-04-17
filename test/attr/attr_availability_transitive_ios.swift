// RUN: %target-typecheck-verify-swift
// REQUIRES: OS=ios

// Allow referencing unavailable API in situations where the caller is marked unavailable in the same circumstances.

@available(iOS, unavailable)
func ios() {} // expected-note 2{{'ios()' has been explicitly marked unavailable here}}

@available(iOSApplicationExtension, unavailable)
func ios_extension() {}

func call_ios_extension() {
    ios_extension() // OK; ios_extension is only unavailable if -application-extension is passed.
}
func call_ios() {
    ios() // expected-error {{'ios()' is unavailable}}
}

@available(iOS, unavailable)
func ios_call_ios_extension() {
    ios_extension() // OK; ios_extension is only unavailable if -application-extension is passed.
}

@available(iOS, unavailable)
func ios_call_ios() {
    ios() // OK; same
}

@available(iOSApplicationExtension, unavailable)
func ios_extension_call_ios_extension() {
    ios_extension()
}

@available(iOSApplicationExtension, unavailable)
func ios_extension_call_ios() {
    ios() // expected-error {{'ios()' is unavailable}}
}
