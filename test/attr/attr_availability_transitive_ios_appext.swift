// RUN: %target-typecheck-verify-swift -application-extension
// RUN: %target-typecheck-verify-swift -application-extension-library
// REQUIRES: OS=ios

// Allow referencing unavailable API in situations where the caller is marked unavailable in the same circumstances.

@available(iOS, unavailable)
func ios() {} // expected-note 2{{'ios()' has been explicitly marked unavailable here}}

@available(iOSApplicationExtension, unavailable)
func ios_extension() {} // expected-note {{'ios_extension()' has been explicitly marked unavailable here}}

func call_ios_extension() {
    ios_extension() // expected-error {{'ios_extension()' is unavailable}}
}
func call_ios() {
    ios() // expected-error {{'ios()' is unavailable}}
}

@available(iOS, unavailable)
func ios_call_ios_extension() {
    ios_extension()
}

@available(iOS, unavailable)
func ios_call_ios() {
    ios()
}

@available(iOSApplicationExtension, unavailable)
func ios_extension_call_ios_extension() {
    ios_extension()
}

@available(iOSApplicationExtension, unavailable)
func ios_extension_call_ios() {
    ios() // expected-error {{'ios()' is unavailable}}
}
