// RUN: %target-typecheck-verify-swift -application-extension -parse-stdlib -target arm64-apple-ios13.0
// RUN: %target-typecheck-verify-swift -application-extension-library -parse-stdlib -target arm64-apple-ios13.0

// expected-warning@<unknown> * {{using sysroot for }}

// Allow referencing unavailable API in situations where the caller is marked unavailable in the same circumstances.

@available(iOS, unavailable) // expected-note {{'ios()' has been explicitly marked unavailable here}}
func ios() {}

@available(iOSApplicationExtension, unavailable) // expected-note {{'ios_extension()' has been explicitly marked unavailable here}}
func ios_extension() {}

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
    ios()
}
