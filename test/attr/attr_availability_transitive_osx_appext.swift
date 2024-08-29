// RUN: %target-typecheck-verify-swift -application-extension
// REQUIRES: OS=macosx

// Allow referencing unavailable API in situations where the caller is marked unavailable in the same circumstances.

@available(OSX, unavailable)
func osx() {} // expected-note 3{{'osx()' has been explicitly marked unavailable here}}

@available(OSXApplicationExtension, unavailable)
func osx_extension() {} // expected-note {{'osx_extension()' has been explicitly marked unavailable here}}

func call_osx_extension() {
    osx_extension() // expected-error {{'osx_extension()' is unavailable}}
}
func call_osx() {
    osx() // expected-error {{'osx()' is unavailable}}
}

@available(OSX, unavailable)
func osx_call_osx_extension() {
    osx_extension()
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
    osx_extension()
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

@available(OSXApplicationExtension, introduced: 52)
func osx_introduced_in_macOS_extesnions_52() {}

func call_osx_introduced_in_macOS_extesnions_52() { // expected-note {{add @available attribute to enclosing global function}} {{1-1=@available(macOSApplicationExtension 52, *)\n}}
  osx_introduced_in_macOS_extesnions_52() // expected-error {{'osx_introduced_in_macOS_extesnions_52()' is only available in application extensions for macOS 52 or newer}}
  // expected-note@-1 {{add 'if #available' version check}} {{3-42=if #available(macOS 52, *) {\n      osx_introduced_in_macOS_extesnions_52()\n  \} else {\n      // Fallback on earlier versions\n  \}}}
}
