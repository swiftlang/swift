// RUN: %target-typecheck-verify-swift -parse-as-library -application-extension
// REQUIRES: OS=macosx

// Allow referencing unavailable API in situations where the caller is marked unavailable in the same circumstances.

@available(*, unavailable)
struct NeverAvailable {} // expected-note * {{'NeverAvailable' has been explicitly marked unavailable here}}

@available(OSX, unavailable)
struct OSXUnavailable {} // expected-note * {{'OSXUnavailable' has been explicitly marked unavailable here}}

@available(OSXApplicationExtension, unavailable)
struct OSXAppExtensionsUnavailable {} // expected-note * {{'OSXAppExtensionsUnavailable' has been explicitly marked unavailable here}}

@available(*, unavailable)
@discardableResult
func never() -> NeverAvailable { // expected-note * {{'never()' has been explicitly marked unavailable here}}
  NeverAvailable()
}

@available(OSX, unavailable)
@discardableResult
func osx() -> OSXUnavailable { // expected-note * {{'osx()' has been explicitly marked unavailable here}}
  OSXUnavailable()
}

@available(OSXApplicationExtension, unavailable)
@discardableResult
func osx_extension() -> OSXAppExtensionsUnavailable { // expected-note * {{'osx_extension()' has been explicitly marked unavailable here}}
  OSXAppExtensionsUnavailable()
}

// MARK: Global functions

func available_func(
  _: NeverAvailable, // expected-error {{'NeverAvailable' is unavailable}}
  _: OSXUnavailable, // expected-error {{'OSXUnavailable' is unavailable in macOS}}
  _: OSXAppExtensionsUnavailable // expected-error {{'OSXAppExtensionsUnavailable' is unavailable in application extensions for macOS}}
) {
  never() // expected-error {{'never()' is unavailable}}
  osx() // expected-error {{'osx()' is unavailable}}
  osx_extension() // expected-error {{'osx_extension()' is unavailable in application extensions for macOS}}
}

@available(*, unavailable)
func never_available_func(
  _: NeverAvailable,
  _: OSXUnavailable,
  _: OSXAppExtensionsUnavailable
) {
  never() // expected-error {{'never()' is unavailable}}
  osx()
  osx_extension()
}

@available(OSX, unavailable)
func osx_func(
  _: NeverAvailable,
  _: OSXUnavailable,
  _: OSXAppExtensionsUnavailable
) {
  never() // expected-error {{'never()' is unavailable}}
  osx()
  osx_extension()
}

@available(OSXApplicationExtension, unavailable)
func osx_extension_func(
  _: NeverAvailable,
  _: OSXUnavailable,
  _: OSXAppExtensionsUnavailable
) {
  never() // expected-error {{'never()' is unavailable}}
  osx() // expected-error {{'osx()' is unavailable}}
  osx_extension()
}

// MARK: Global vars

var always_var: (
  NeverAvailable, // expected-error {{'NeverAvailable' is unavailable}}
  OSXUnavailable, // expected-error {{'OSXUnavailable' is unavailable in macOS}}
  OSXAppExtensionsUnavailable // expected-error {{'OSXAppExtensionsUnavailable' is unavailable in application extensions for macOS}}
) = (
  never(), // expected-error {{'never()' is unavailable}}
  osx(), // expected-error {{'osx()' is unavailable}}
  osx_extension() // expected-error {{'osx_extension()' is unavailable in application extensions for macOS}}
)

@available(*, unavailable)
var never_var: (
  NeverAvailable,
  OSXUnavailable,
  OSXAppExtensionsUnavailable
) = (
  never(), // expected-error {{'never()' is unavailable}}
  osx(),
  osx_extension()
)

@available(OSX, unavailable)
var osx_var: (
  NeverAvailable,
  OSXUnavailable,
  OSXAppExtensionsUnavailable
) = (
  never(), // expected-error {{'never()' is unavailable}}
  osx(),
  osx_extension()
)

@available(OSXApplicationExtension, unavailable)
var osx_extension_var: (
  NeverAvailable,
  OSXUnavailable,
  OSXAppExtensionsUnavailable
) = (
  never(), // expected-error {{'never()' is unavailable}}
  osx(), // expected-error {{'osx()' is unavailable}}
  osx_extension()
)

// MARK: Properties

struct AlwaysAvailabileContainer {
  let never_var: NeverAvailable = never() // expected-error {{'never()' is unavailable}}
  // expected-error@-1 {{'NeverAvailable' is unavailable}}
  let osx_var: OSXUnavailable = osx() // expected-error {{'osx()' is unavailable}}
  // expected-error@-1 {{'OSXUnavailable' is unavailable in macOS}}
  let osx_extension_var: OSXAppExtensionsUnavailable = osx_extension() // expected-error {{'osx_extension()' is unavailable in application extensions for macOS}}
  // expected-error@-1 {{'OSXAppExtensionsUnavailable' is unavailable in application extensions for macOS}}
}

@available(*, unavailable)
struct NeverAvailableContainer { // expected-note {{'NeverAvailableContainer' has been explicitly marked unavailable here}}
  let never_var: NeverAvailable = never() // expected-error {{'never()' is unavailable}}
  let osx_var: OSXUnavailable = osx()
  let osx_extension_var: OSXAppExtensionsUnavailable = osx_extension()
}

@available(OSX, unavailable)
struct OSXUnavailableContainer { // expected-note {{'OSXUnavailableContainer' has been explicitly marked unavailable here}}
  let never_var: NeverAvailable = never() // expected-error {{'never()' is unavailable}}
  let osx_var: OSXUnavailable = osx()
  let osx_extension_var: OSXAppExtensionsUnavailable = osx_extension()
}

@available(OSXApplicationExtension, unavailable)
struct OSXAppExtensionsUnavailableContainer { // expected-note {{'OSXAppExtensionsUnavailableContainer' has been explicitly marked unavailable here}}
  let never_var: NeverAvailable = never() // expected-error {{'never()' is unavailable}}
  let osx_var: OSXUnavailable = osx() // expected-error {{'osx()' is unavailable}}
  let osx_extension_var: OSXAppExtensionsUnavailable = osx_extension()
}

// MARK: Extensions

extension AlwaysAvailabileContainer {}
extension NeverAvailableContainer {} // expected-error {{'NeverAvailableContainer' is unavailable}}
extension OSXUnavailableContainer {} // expected-error {{'OSXUnavailableContainer' is unavailable in macOS}}
extension OSXAppExtensionsUnavailableContainer {} // expected-error {{'OSXAppExtensionsUnavailableContainer' is unavailable in application extensions for macOS}}

@available(*, unavailable)
extension AlwaysAvailabileContainer {}
@available(*, unavailable)
extension NeverAvailableContainer {}
@available(*, unavailable)
extension OSXUnavailableContainer {}
@available(*, unavailable)
extension OSXAppExtensionsUnavailableContainer {}

@available(OSX, unavailable)
extension AlwaysAvailabileContainer {}
@available(OSX, unavailable)
extension NeverAvailableContainer {}
@available(OSX, unavailable)
extension OSXUnavailableContainer {}
@available(OSX, unavailable)
extension OSXAppExtensionsUnavailableContainer {}

@available(OSXApplicationExtension, unavailable)
extension AlwaysAvailabileContainer {}
@available(OSXApplicationExtension, unavailable)
extension NeverAvailableContainer {}
@available(OSXApplicationExtension, unavailable)
extension OSXUnavailableContainer {}
@available(OSXApplicationExtension, unavailable)
extension OSXAppExtensionsUnavailableContainer {}

struct ExtendMe {}

@available(*, unavailable)
extension ExtendMe {
  func never_available_extension_available_method() {} // expected-note 3 {{has been explicitly marked unavailable here}}

  @available(OSX 99, *)
  func never_available_extension_osx_future_method() {} // expected-note 3 {{has been explicitly marked unavailable here}}

  func never_available_extension_available_method(
    _: NeverAvailable,
    _: OSXUnavailable,
    _: OSXAppExtensionsUnavailable
  ) {
    never() // expected-error {{'never()' is unavailable}}
    osx()
    osx_extension()
  }

  @available(*, unavailable)
  func never_available_extension_never_available_method(
    _: NeverAvailable,
    _: OSXUnavailable,
    _: OSXAppExtensionsUnavailable
  ) {
    never() // expected-error {{'never()' is unavailable}}
    osx()
    osx_extension()
  }

  @available(OSX, unavailable)
  func never_available_extension_osx_method(
    _: NeverAvailable,
    _: OSXUnavailable,
    _: OSXAppExtensionsUnavailable
  ) {
    never() // expected-error {{'never()' is unavailable}}
    osx()
    osx_extension()
  }

  @available(OSXApplicationExtension, unavailable)
  func never_available_extension_osx_app_extension_method(
    _: NeverAvailable,
    _: OSXUnavailable,
    _: OSXAppExtensionsUnavailable
  ) {
    never() // expected-error {{'never()' is unavailable}}
    osx()
    osx_extension()
  }
}

@available(OSX, unavailable)
extension ExtendMe {
  func osx_extension_available_method() {} // expected-note 2 {{has been explicitly marked unavailable here}}

  @available(OSX 99, *)
  func osx_extension_osx_future_method() {} // expected-note 2 {{has been explicitly marked unavailable here}}

  @available(*, unavailable)
  func osx_extension_never_available_method() {} // expected-note 3 {{'osx_extension_never_available_method()' has been explicitly marked unavailable here}}

  @available(OSX, unavailable)
  func osx_extension_osx_method() {} // expected-note 2 {{'osx_extension_osx_method()' has been explicitly marked unavailable here}}

  @available(OSXApplicationExtension, unavailable)
  func osx_extension_osx_app_extension_method() {} // expected-note 2 {{'osx_extension_osx_app_extension_method()' has been explicitly marked unavailable here}}

  func osx_extension_available_method(
    _: NeverAvailable,
    _: OSXUnavailable,
    _: OSXAppExtensionsUnavailable
  ) {
    never() // expected-error {{'never()' is unavailable}}
    osx()
    osx_extension()
  }

  @available(*, unavailable)
  func osx_extension_never_available_method(
    _: NeverAvailable,
    _: OSXUnavailable,
    _: OSXAppExtensionsUnavailable
  ) {
    never() // expected-error {{'never()' is unavailable}}
    osx()
    osx_extension()
  }

  @available(OSX, unavailable)
  func osx_extension_osx_method(
    _: NeverAvailable,
    _: OSXUnavailable,
    _: OSXAppExtensionsUnavailable
  ) {
    never() // expected-error {{'never()' is unavailable}}
    osx()
    osx_extension()
  }

  @available(OSXApplicationExtension, unavailable)
  func osx_extension_osx_app_extension_method(
    _: NeverAvailable,
    _: OSXUnavailable,
    _: OSXAppExtensionsUnavailable
  ) {
    never() // expected-error {{'never()' is unavailable}}
    osx()
    osx_extension()
  }
}

@available(OSXApplicationExtension, unavailable)
extension ExtendMe {
  func osx_app_extension_extension_available_method() {} // expected-note {{'osx_app_extension_extension_available_method()' has been explicitly marked unavailable here}}

  @available(OSX 99, *)
  func osx_app_extension_extension_osx_future_method() {} // expected-note {{'osx_app_extension_extension_osx_future_method()'}}

  @available(*, unavailable)
  func osx_app_extension_extension_never_available_method() {} // expected-note 3 {{'osx_app_extension_extension_never_available_method()' has been explicitly marked unavailable here}}

  @available(OSX, unavailable)
  func osx_app_extension_extension_osx_method() {} // expected-note 2 {{'osx_app_extension_extension_osx_method()' has been explicitly marked unavailable here}}

  @available(OSXApplicationExtension, unavailable)
  func osx_app_extension_extension_osx_app_extension_method() {} // expected-note {{'osx_app_extension_extension_osx_app_extension_method()' has been explicitly marked unavailable here}}

  func osx_app_extension_extension_available_method(
    _: NeverAvailable,
    _: OSXUnavailable,
    _: OSXAppExtensionsUnavailable
  ) {
    never() // expected-error {{'never()' is unavailable}}
    osx() // expected-error {{'osx()' is unavailable}}
    osx_extension()
  }

  @available(*, unavailable)
  func osx_app_extension_extension_never_available_method(
    _: NeverAvailable,
    _: OSXUnavailable,
    _: OSXAppExtensionsUnavailable
  ) {
    never() // expected-error {{'never()' is unavailable}}
    osx()
    osx_extension()
  }

  @available(OSX, unavailable)
  func osx_app_extension_extension_osx_method(
    _: NeverAvailable,
    _: OSXUnavailable,
    _: OSXAppExtensionsUnavailable
  ) {
    never() // expected-error {{'never()' is unavailable}}
    osx()
    osx_extension()
  }

  @available(OSXApplicationExtension, unavailable)
  func osx_app_extension_extension_osx_app_extension_method(
    _: NeverAvailable,
    _: OSXUnavailable,
    _: OSXAppExtensionsUnavailable
  ) {
    never() // expected-error {{'never()' is unavailable}}
    osx() // expected-error {{'osx()' is unavailable}}
    osx_extension()
  }
}

func available_func_call_extension_methods(_ e: ExtendMe) {
  e.never_available_extension_available_method() // expected-error {{'never_available_extension_available_method()' is unavailable}}
  e.osx_extension_available_method() // expected-error {{'osx_extension_available_method()' is unavailable in macOS}}
  e.osx_app_extension_extension_available_method() // expected-error {{'osx_app_extension_extension_available_method()' is unavailable in application extensions for macOS}}
  e.osx_extension_never_available_method() // expected-error {{'osx_extension_never_available_method()' is unavailable in macOS}}
  e.osx_extension_osx_method() // expected-error {{'osx_extension_osx_method()' is unavailable in macOS}}
  e.osx_extension_osx_app_extension_method() // expected-error {{'osx_extension_osx_app_extension_method()' is unavailable in application extensions for macOS}}

  e.never_available_extension_osx_future_method() // expected-error {{'never_available_extension_osx_future_method()' is unavailable}}
  e.osx_extension_osx_future_method() // expected-error {{'osx_extension_osx_future_method()' is unavailable in macOS}}
  e.osx_app_extension_extension_osx_future_method() // expected-error {{'osx_app_extension_extension_osx_future_method()' is unavailable in application extensions for macOS}}
  e.osx_app_extension_extension_never_available_method() // expected-error {{'osx_app_extension_extension_never_available_method()' is unavailable in application extensions for macOS}}
  e.osx_app_extension_extension_osx_method() // expected-error {{'osx_app_extension_extension_osx_method()' is unavailable in application extensions for macOS}}
  e.osx_app_extension_extension_osx_app_extension_method() // expected-error {{'osx_app_extension_extension_osx_app_extension_method()' is unavailable in application extensions for macOS}}
}

@available(OSX, unavailable)
func osx_func_call_extension_methods(_ e: ExtendMe) {
  e.never_available_extension_available_method() // expected-error {{'never_available_extension_available_method()' is unavailable}}
  e.osx_extension_available_method()
  e.osx_app_extension_extension_available_method()
  e.osx_extension_never_available_method() // expected-error {{'osx_extension_never_available_method()' is unavailable}}
  e.osx_extension_osx_method()
  e.osx_extension_osx_app_extension_method()

  e.never_available_extension_osx_future_method() // expected-error {{'never_available_extension_osx_future_method()' is unavailable}}
  e.osx_extension_osx_future_method()
  e.osx_app_extension_extension_osx_future_method()
  e.osx_app_extension_extension_never_available_method() // expected-error {{'osx_app_extension_extension_never_available_method()' is unavailable}}
  e.osx_app_extension_extension_osx_method()
  e.osx_app_extension_extension_osx_app_extension_method()
}

@available(OSXApplicationExtension, unavailable)
func osx_app_ext_func_call_extension_methods(_ e: ExtendMe) {
  e.never_available_extension_available_method() // expected-error {{'never_available_extension_available_method()' is unavailable}}
  e.osx_extension_available_method() // expected-error {{'osx_extension_available_method()' is unavailable in macOS}}
  e.osx_app_extension_extension_available_method()
  e.osx_extension_never_available_method() // expected-error {{'osx_extension_never_available_method()' is unavailable in macOS}}
  e.osx_extension_osx_method() // expected-error {{'osx_extension_osx_method()' is unavailable in macOS}}
  e.osx_extension_osx_app_extension_method() // expected-error {{'osx_extension_osx_app_extension_method()' is unavailable in macOS}}

  e.never_available_extension_osx_future_method() // expected-error {{'never_available_extension_osx_future_method()' is unavailable}}
  e.osx_extension_osx_future_method() // expected-error {{'osx_extension_osx_future_method()' is unavailable in macOS}}
  e.osx_app_extension_extension_osx_future_method() // expected-error {{'osx_app_extension_extension_osx_future_method()' is only available in macOS 99 or newer}}
  // expected-note@-1 {{add 'if #available' version check}}
  e.osx_app_extension_extension_never_available_method() // expected-error {{'osx_app_extension_extension_never_available_method()' is unavailable}}
  e.osx_app_extension_extension_osx_method() // expected-error {{'osx_app_extension_extension_osx_method()' is unavailable in macOS}}
  e.osx_app_extension_extension_osx_app_extension_method()
}

@available(OSXApplicationExtension, introduced: 99)
func osx_app_extensions_future() {}

func call_osx_app_extensions_future() { // expected-note {{add '@available' attribute to enclosing global function}} {{1-1=@available(macOSApplicationExtension 99, *)\n}}
  osx_app_extensions_future() // expected-error {{'osx_app_extensions_future()' is only available in application extensions for macOS 99 or newer}}
  // expected-note@-1 {{add 'if #available' version check}} {{3-30=if #available(macOS 99, *) {\n      osx_app_extensions_future()\n  \} else {\n      // Fallback on earlier versions\n  \}}}
}
