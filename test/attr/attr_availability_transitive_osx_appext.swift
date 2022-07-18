// RUN: %target-typecheck-verify-swift -application-extension
// REQUIRES: OS=macosx

// Allow referencing unavailable API in situations where the caller is marked unavailable in the same circumstances.

@available(OSX, unavailable)
func osx() {} // expected-note 3{{'osx()' has been explicitly marked unavailable here}}

@available(OSXApplicationExtension, unavailable)
func osx_extension() {} // expected-note 3{{'osx_extension()' has been explicitly marked unavailable here}}

func call_osx_extension() {
    osx_extension() // expected-error {{'osx_extension()' is unavailable}}
}
func call_osx() {
    osx() // expected-error {{'osx()' is unavailable}}
}

@available(OSX, unavailable)
func osx_call_osx_extension() {
    osx_extension() // expected-error {{'osx_extension()' is unavailable}}
}

@available(OSX, unavailable)
func osx_call_osx() {
    osx()
}

@available(OSXApplicationExtension, unavailable)
func osx_extension_call_osx_extension() {
    osx_extension()
}

@available(OSXApplicationExtension, unavailable)
func osx_extension_call_osx() {
    osx() // expected-error {{'osx()' is unavailable}}
}

@available(OSX, unavailable)
struct NotOnOSX { }

@available(OSX, unavailable)
extension NotOnOSX {
  func osx_call_osx() {
    osx() // OK
  }

  func osx_call_osx_extension() {
    osx_extension() // expected-error {{'osx_extension()' is unavailable in application extensions for macOS}}
  }
}

@available(OSXApplicationExtension, unavailable)
extension NotOnOSX { }

@available(OSXApplicationExtension, unavailable)
struct NotOnOSXApplicationExtension { }

@available(OSX, unavailable)
extension NotOnOSXApplicationExtension { }

@available(OSXApplicationExtension, unavailable)
extension NotOnOSXApplicationExtension {
  func osx_call_osx() {
    osx() // expected-error {{'osx()' is unavailable in macOS}}
  }

  func osx_call_osx_extension() {
    osx_extension() // OK
  }
}
