// RUN: %target-typecheck-verify-swift -parse-as-library
// REQUIRES: OS=macosx

// Allow referencing unavailable API in situations where the caller is marked unavailable in the same circumstances.

@available(OSX, unavailable)
@discardableResult
func osx() -> Int { return 0 } // expected-note * {{'osx()' has been explicitly marked unavailable here}}

@available(OSXApplicationExtension, unavailable)
func osx_extension() {}

@available(OSX, unavailable)
func osx_pair() -> (Int, Int) { return (0, 0) } // expected-note * {{'osx_pair()' has been explicitly marked unavailable here}}

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

@available(OSX, unavailable)
var osx_init_osx = osx() // OK

@available(OSXApplicationExtension, unavailable)
var osx_extension_init_osx = osx() // expected-error {{'osx()' is unavailable}}

@available(OSX, unavailable)
var osx_inner_init_osx = { let inner_var = osx() } // OK

@available(OSXApplicationExtension, unavailable)
var osx_extension_inner_init_osx = { let inner_var = osx() } // expected-error {{'osx()' is unavailable}}

struct Outer {
  @available(OSX, unavailable)
  var osx_init_osx = osx() // OK

  @available(OSX, unavailable)
  lazy var osx_lazy_osx = osx() // OK

  @available(OSXApplicationExtension, unavailable)
  var osx_extension_init_osx = osx() // expected-error {{'osx()' is unavailable}}

  @available(OSXApplicationExtension, unavailable)
  var osx_extension_lazy_osx = osx() // expected-error {{'osx()' is unavailable}}

  @available(OSX, unavailable)
  var osx_init_multi1_osx = osx(), osx_init_multi2_osx = osx() // OK

  @available(OSXApplicationExtension, unavailable)
  var osx_extension_init_multi1_osx = osx(), osx_extension_init_multi2_osx = osx() // expected-error 2 {{'osx()' is unavailable}}

  @available(OSX, unavailable)
  var (osx_init_deconstruct1_osx, osx_init_deconstruct2_osx) = osx_pair() // OK

  @available(OSXApplicationExtension, unavailable)
  var (osx_extension_init_deconstruct1_osx, osx_extension_init_deconstruct2_osx) = osx_pair() // expected-error {{'osx_pair()' is unavailable}}

  @available(OSX, unavailable)
  var (_, osx_init_deconstruct2_only_osx) = osx_pair() // OK
  
  @available(OSXApplicationExtension, unavailable)
  var (_, osx_extension_init_deconstruct2_only_osx) = osx_pair() // expected-error {{'osx_pair()' is unavailable}}

  @available(OSX, unavailable)
  var (osx_init_deconstruct1_only_osx, _) = osx_pair() // OK
  
  @available(OSXApplicationExtension, unavailable)
  var (osx_extension_init_deconstruct1_only_osx, _) = osx_pair() // expected-error {{'osx_pair()' is unavailable}}

  @available(OSX, unavailable)
  var osx_inner_init_osx = { let inner_var = osx() } // OK
  
  @available(OSXApplicationExtension, unavailable)
  var osx_extension_inner_init_osx = { let inner_var = osx() } // expected-error {{'osx()' is unavailable}}
}

extension Outer {
  @available(OSX, unavailable)
  static var also_osx_init_osx = osx() // OK
  
  @available(OSXApplicationExtension, unavailable)
  static var also_osx_extension_init_osx = osx() // expected-error {{'osx()' is unavailable}}
}

@available(OSX, unavailable)
// expected-note@+1 {{enclosing scope has been explicitly marked unavailable here}}
extension Outer {
  // expected-note@+1 {{'outer_osx_init_osx' has been explicitly marked unavailable here}}
  static var outer_osx_init_osx = osx() // OK
  
  // expected-note@+1 {{'osx_call_osx()' has been explicitly marked unavailable here}}
  func osx_call_osx() {
    osx() // OK
  }

  func osx_call_osx_extension() {
    osx_extension() // OK; osx_extension is only unavailable if -application-extension is passed.
  }
  
  func takes_and_returns_osx(_ x: NotOnOSX) -> NotOnOSX {
    return x // OK
  }
  
  // This @available should be ignored; inherited unavailability takes precedence
  @available(OSX 999, *) // expected-warning {{instance method cannot be more available than unavailable enclosing scope}}
  // expected-note@+1 {{'osx_more_available_but_still_unavailable_call_osx()' has been explicitly marked unavailable here}}
  func osx_more_available_but_still_unavailable_call_osx() {
    osx() // OK
  }

  @available(OSX, unavailable)
  func osx_double_unavailable_call_osx() {
    osx() // OK
  }

  @available(*, unavailable)
  func osx_universally_unavailable_call_osx() {
    osx() // OK
  }
  
  // rdar://92551870
  func osx_call_osx_more_available_but_still_unavailable() {
    osx_more_available_but_still_unavailable_call_osx() // OK
  }
}

func takesOuter(_ o: Outer) {
  _ = Outer.outer_osx_init_osx // expected-error {{'outer_osx_init_osx' is unavailable in macOS}}
  o.osx_call_osx() // expected-error {{'osx_call_osx()' is unavailable in macOS}}
  o.osx_more_available_but_still_unavailable_call_osx() // expected-error {{'osx_more_available_but_still_unavailable_call_osx()' is unavailable in macOS}}
}

@available(OSX, unavailable)
struct NotOnOSX { // expected-note {{'NotOnOSX' has been explicitly marked unavailable here}}
  var osx_init_osx = osx() // OK
  lazy var osx_lazy_osx = osx() // OK
  var osx_init_multi1_osx = osx(), osx_init_multi2_osx = osx() // OK
  var (osx_init_deconstruct1_osx, osx_init_deconstruct2_osx) = osx_pair() // OK
  var (_, osx_init_deconstruct2_only_osx) = osx_pair() // OK
  var (osx_init_deconstruct1_only_osx, _) = osx_pair() // OK
}

@available(OSX, unavailable)
extension NotOnOSX {
  func osx_call_osx() {
    osx() // OK
  }

  func osx_call_osx_extension() {
    osx_extension() // OK; osx_extension is only unavailable if -application-extension is passed.
  }
}

@available(OSXApplicationExtension, unavailable)
extension NotOnOSX { } // expected-error {{'NotOnOSX' is unavailable in macOS}}

@available(OSXApplicationExtension, unavailable)
struct NotOnOSXApplicationExtension { }

@available(OSX, unavailable)
extension NotOnOSXApplicationExtension { } // OK; NotOnOSXApplicationExtension is only unavailable if -application-extension is passed.

@available(OSXApplicationExtension, unavailable)
extension NotOnOSXApplicationExtension {
  func osx_call_osx() {
    osx() // expected-error {{'osx()' is unavailable in macOS}}
  }

  func osx_call_osx_extension() {
    osx_extension() // OK
  }
}
