// RUN: %target-typecheck-verify-swift -parse-as-library -application-extension -parse-stdlib -target arm64-apple-macos11

// expected-warning@<unknown> * {{using sysroot for }}

// Allow referencing unavailable API in situations where the caller is marked unavailable in the same circumstances.

struct AlwaysAvailable {}

@available(*, unavailable) // expected-note * {{'NeverAvailable' has been explicitly marked unavailable here}}
struct NeverAvailable {}

@available(anyAppleOS, unavailable) // expected-note * {{'AnyAppleOSUnavailable' has been explicitly marked unavailable here}}
struct AnyAppleOSUnavailable {}

@available(OSX, unavailable) // expected-note * {{'OSXUnavailable' has been explicitly marked unavailable here}}
struct OSXUnavailable {}

@available(OSXApplicationExtension, unavailable) // expected-note * {{'OSXAppExtensionsUnavailable' has been explicitly marked unavailable here}}
struct OSXAppExtensionsUnavailable {}

@available(*, unavailable) // expected-note * {{'never()' has been explicitly marked unavailable here}}
@discardableResult
func never() -> NeverAvailable {
  NeverAvailable()
}

@available(anyAppleOS, unavailable) // expected-note * {{'any_apple_os()' has been explicitly marked unavailable here}}
@discardableResult
func any_apple_os() -> AnyAppleOSUnavailable {
  AnyAppleOSUnavailable()
}

@available(OSX, unavailable) // expected-note * {{'osx()' has been explicitly marked unavailable here}}
@discardableResult
func osx() -> OSXUnavailable {
  OSXUnavailable()
}

@available(OSXApplicationExtension, unavailable) // expected-note * {{'osx_extension()' has been explicitly marked unavailable here}}
@discardableResult
func osx_extension() -> OSXAppExtensionsUnavailable {
  OSXAppExtensionsUnavailable()
}

@available(anyAppleOS 99, *)
@discardableResult
func any_apple_os_future() -> AlwaysAvailable {
  AlwaysAvailable()
}

@available(OSX 99, *)
@discardableResult
func osx_future() -> AlwaysAvailable {
  AlwaysAvailable()
}

@available(OSXApplicationExtension 99, *)
@discardableResult
func osx_extension_future() -> AlwaysAvailable {
  AlwaysAvailable()
}

// MARK: Global functions

func available_func( // expected-note 3 {{add '@available' attribute to enclosing global function}}
  _: NeverAvailable, // expected-error {{'NeverAvailable' is unavailable}}
  _: AnyAppleOSUnavailable, // expected-error {{'AnyAppleOSUnavailable' is unavailable in macOS}}
  _: OSXUnavailable, // expected-error {{'OSXUnavailable' is unavailable in macOS}}
  _: OSXAppExtensionsUnavailable // expected-error {{'OSXAppExtensionsUnavailable' is unavailable in application extensions for macOS}}
) {
  never() // expected-error {{'never()' is unavailable}}
  any_apple_os() // expected-error {{'any_apple_os()' is unavailable in macOS}}
  osx() // expected-error {{'osx()' is unavailable}}
  osx_extension() // expected-error {{'osx_extension()' is unavailable in application extensions for macOS}}
  any_apple_os_future() // expected-error {{'any_apple_os_future()' is only available in macOS 99 or newer}}
  // expected-note@-1 {{add 'if #available' version check}}
  osx_future() // expected-error {{'osx_future()' is only available in macOS 99 or newer}}
  // expected-note@-1 {{add 'if #available' version check}}
  osx_extension_future() // expected-error {{'osx_extension_future()' is only available in application extensions for macOS 99 or newer}}
  // expected-note@-1 {{add 'if #available' version check}}
}

@available(*, unavailable)
func never_available_func(
  _: NeverAvailable,
  _: AnyAppleOSUnavailable,
  _: OSXUnavailable,
  _: OSXAppExtensionsUnavailable
) {
  never() // expected-error {{'never()' is unavailable}}
  any_apple_os()
  osx()
  osx_extension()
  any_apple_os_future()
  osx_future()
  osx_extension_future()
}

@available(OSX, unavailable)
func osx_func(
  _: NeverAvailable, // expected-error {{'NeverAvailable' is unavailable}}
  _: AnyAppleOSUnavailable,
  _: OSXUnavailable,
  _: OSXAppExtensionsUnavailable
) {
  never() // expected-error {{'never()' is unavailable}}
  any_apple_os()
  osx()
  osx_extension()
  any_apple_os_future()
  osx_future()
  osx_extension_future()
}

@available(OSXApplicationExtension, unavailable)
func osx_extension_func(
  _: NeverAvailable, // expected-error {{'NeverAvailable' is unavailable}}
  _: AnyAppleOSUnavailable, // expected-error {{'AnyAppleOSUnavailable' is unavailable in macOS}}
  _: OSXUnavailable, // expected-error {{'OSXUnavailable' is unavailable in macOS}}
  _: OSXAppExtensionsUnavailable
) {
  never() // expected-error {{'never()' is unavailable}}
  any_apple_os() // expected-error {{'any_apple_os()' is unavailable in macOS}}
  osx() // expected-error {{'osx()' is unavailable}}
  osx_extension()
  any_apple_os_future() // expected-error {{'any_apple_os_future()' is only available in macOS 99 or newer}}
  // expected-note@-1 {{add 'if #available' version check}}
  osx_future() // expected-error {{'osx_future()' is only available in macOS 99 or newer}}
  // expected-note@-1 {{add 'if #available' version check}}
  osx_extension_future()
}

// MARK: Global vars

var always_var: ( // expected-note 3 {{add '@available' attribute to enclosing var}}
  NeverAvailable, // expected-error {{'NeverAvailable' is unavailable}}
  AnyAppleOSUnavailable, // expected-error {{'AnyAppleOSUnavailable' is unavailable in macOS}}
  OSXUnavailable, // expected-error {{'OSXUnavailable' is unavailable in macOS}}
  OSXAppExtensionsUnavailable, // expected-error {{'OSXAppExtensionsUnavailable' is unavailable in application extensions for macOS}}
  AlwaysAvailable,
  AlwaysAvailable,
  AlwaysAvailable
) = (
  never(), // expected-error {{'never()' is unavailable}}
  any_apple_os(), // expected-error {{'any_apple_os()' is unavailable in macOS}}
  osx(), // expected-error {{'osx()' is unavailable}}
  osx_extension(), // expected-error {{'osx_extension()' is unavailable in application extensions for macOS}}
  any_apple_os_future(), // expected-error {{'any_apple_os_future()' is only available in macOS 99 or newer}}
  osx_future(), // expected-error {{'osx_future()' is only available in macOS 99 or newer}}
  osx_extension_future() // expected-error {{'osx_extension_future()' is only available in application extensions for macOS 99 or newer}}
)

@available(*, unavailable)
var never_var: (
  NeverAvailable,
  AnyAppleOSUnavailable,
  OSXUnavailable,
  OSXAppExtensionsUnavailable,
  AlwaysAvailable,
  AlwaysAvailable,
  AlwaysAvailable
) = (
  never(), // expected-error {{'never()' is unavailable}}
  any_apple_os(),
  osx(),
  osx_extension(),
  any_apple_os_future(),
  osx_future(),
  osx_extension_future()
)

@available(OSX, unavailable)
var osx_var: (
  NeverAvailable, // expected-error {{'NeverAvailable' is unavailable}}
  AnyAppleOSUnavailable,
  OSXUnavailable,
  OSXAppExtensionsUnavailable,
  AlwaysAvailable,
  AlwaysAvailable,
  AlwaysAvailable
) = (
  never(), // expected-error {{'never()' is unavailable}}
  any_apple_os(),
  osx(),
  osx_extension(),
  any_apple_os_future(),
  osx_future(),
  osx_extension_future()
)

@available(OSXApplicationExtension, unavailable)
var osx_extension_var: (
  NeverAvailable, // expected-error {{'NeverAvailable' is unavailable}}
  AnyAppleOSUnavailable, // expected-error {{'AnyAppleOSUnavailable' is unavailable in macOS}}
  OSXUnavailable, // expected-error {{'OSXUnavailable' is unavailable in macOS}}
  OSXAppExtensionsUnavailable,
  AlwaysAvailable,
  AlwaysAvailable,
  AlwaysAvailable
) = (
  never(), // expected-error {{'never()' is unavailable}}
  any_apple_os(), // expected-error {{'any_apple_os()' is unavailable in macOS}}
  osx(), // expected-error {{'osx()' is unavailable}}
  osx_extension(),
  any_apple_os_future(), // expected-error {{'any_apple_os_future()' is only available in macOS 99 or newer}}
  osx_future(), // expected-error {{'osx_future()' is only available in macOS 99 or newer}}
  osx_extension_future()
)

// MARK: Properties

struct AlwaysAvailabileContainer { // expected-note 3 {{add '@available' attribute to enclosing struct}}
  let never_var: NeverAvailable = never() // expected-error {{'never()' is unavailable}}
  // expected-error@-1 {{'NeverAvailable' is unavailable}}
  let osx_var: OSXUnavailable = osx() // expected-error {{'osx()' is unavailable}}
  // expected-error@-1 {{'OSXUnavailable' is unavailable in macOS}}
  let osx_extension_var: OSXAppExtensionsUnavailable = osx_extension() // expected-error {{'osx_extension()' is unavailable in application extensions for macOS}}
  // expected-error@-1 {{'OSXAppExtensionsUnavailable' is unavailable in application extensions for macOS}}
  let any_apple_os_future_var: AlwaysAvailable = any_apple_os_future() // expected-error {{'any_apple_os_future()' is only available in macOS 99 or newer}}
  let osx_future_var: AlwaysAvailable = osx_future() // expected-error {{'osx_future()' is only available in macOS 99 or newer}}
  let osx_extension_future_var: AlwaysAvailable = osx_extension_future() // expected-error {{'osx_extension_future()' is only available in application extensions for macOS 99 or newer}}
}

@available(*, unavailable) // expected-note 3 {{'NeverAvailableContainer' has been explicitly marked unavailable here}}
struct NeverAvailableContainer {
  let never_var: NeverAvailable = never() // expected-error {{'never()' is unavailable}}
  let any_apple_os_var: AnyAppleOSUnavailable = any_apple_os()
  let osx_var: OSXUnavailable = osx()
  let osx_extension_var: OSXAppExtensionsUnavailable = osx_extension()
  let any_apple_os_future_var: AlwaysAvailable = any_apple_os_future()
  let osx_future_var: AlwaysAvailable = osx_future()
  let osx_extension_future_var: AlwaysAvailable = osx_extension_future()
}

@available(anyAppleOS, unavailable) // expected-note * {{'AnyAppleOSUnavailableContainer' has been explicitly marked unavailable here}}
struct AnyAppleOSUnavailableContainer {
  let never_var: NeverAvailable = never() // expected-error {{'never()' is unavailable}}
  // expected-error@-1 {{'NeverAvailable' is unavailable}}
  let any_apple_os_var: AnyAppleOSUnavailable = any_apple_os()
  let osx_var: OSXUnavailable = osx()
  let osx_extension_var: OSXAppExtensionsUnavailable = osx_extension()
  let any_apple_os_future_var: AlwaysAvailable = any_apple_os_future()
  let osx_future_var: AlwaysAvailable = osx_future()
  let osx_extension_future_var: AlwaysAvailable = osx_extension_future()
}

@available(OSX, unavailable) // expected-note 2 {{'OSXUnavailableContainer' has been explicitly marked unavailable here}}
struct OSXUnavailableContainer {
  let never_var: NeverAvailable = never() // expected-error {{'never()' is unavailable}}
  // expected-error@-1 {{'NeverAvailable' is unavailable}}
  let any_apple_os_var: AnyAppleOSUnavailable = any_apple_os()
  let osx_var: OSXUnavailable = osx()
  let osx_extension_var: OSXAppExtensionsUnavailable = osx_extension()
  let any_apple_os_future_var: AlwaysAvailable = any_apple_os_future()
  let osx_future_var: AlwaysAvailable = osx_future()
  let osx_extension_future_var: AlwaysAvailable = osx_extension_future()
}

@available(OSXApplicationExtension, unavailable) // expected-note {{'OSXAppExtensionsUnavailableContainer' has been explicitly marked unavailable here}}
struct OSXAppExtensionsUnavailableContainer {
  let never_var: NeverAvailable = never() // expected-error {{'never()' is unavailable}}
  // expected-error@-1 {{'NeverAvailable' is unavailable}}
  let any_apple_os_var: AnyAppleOSUnavailable = any_apple_os() // expected-error {{'any_apple_os()' is unavailable in macOS}}
  // expected-error@-1 {{'AnyAppleOSUnavailable' is unavailable in macOS}}
  let osx_var: OSXUnavailable = osx() // expected-error {{'osx()' is unavailable}}
  // expected-error@-1 {{'OSXUnavailable' is unavailable in macOS}}
  let osx_extension_var: OSXAppExtensionsUnavailable = osx_extension()
  let any_apple_os_future_var: AlwaysAvailable = any_apple_os_future() // expected-error {{'any_apple_os_future()' is only available in macOS 99 or newer}}
  let osx_future_var: AlwaysAvailable = osx_future() // expected-error {{'osx_future()' is only available in macOS 99 or newer}}
  let osx_extension_future_var: AlwaysAvailable = osx_extension_future()
}

// MARK: Extensions

extension AlwaysAvailabileContainer {}
extension NeverAvailableContainer {} // expected-error {{'NeverAvailableContainer' is unavailable}}
extension AnyAppleOSUnavailableContainer {} // expected-error {{'AnyAppleOSUnavailableContainer' is unavailable in macOS}}
extension OSXUnavailableContainer {} // expected-error {{'OSXUnavailableContainer' is unavailable in macOS}}
extension OSXAppExtensionsUnavailableContainer {} // expected-error {{'OSXAppExtensionsUnavailableContainer' is unavailable in application extensions for macOS}}

@available(*, unavailable)
extension AlwaysAvailabileContainer {}
@available(*, unavailable)
extension NeverAvailableContainer {}
@available(*, unavailable)
extension AnyAppleOSUnavailableContainer {}
@available(*, unavailable)
extension OSXUnavailableContainer {}
@available(*, unavailable)
extension OSXAppExtensionsUnavailableContainer {}

@available(OSX, unavailable)
extension AlwaysAvailabileContainer {}
@available(OSX, unavailable)
extension NeverAvailableContainer {} // expected-error {{'NeverAvailableContainer' is unavailable}}
@available(OSX, unavailable)
extension AnyAppleOSUnavailableContainer {}
@available(OSX, unavailable)
extension OSXUnavailableContainer {}
@available(OSX, unavailable)
extension OSXAppExtensionsUnavailableContainer {}

@available(OSXApplicationExtension, unavailable)
extension AlwaysAvailabileContainer {}
@available(OSXApplicationExtension, unavailable)
extension NeverAvailableContainer {} // expected-error {{'NeverAvailableContainer' is unavailable}}
@available(OSXApplicationExtension, unavailable)
extension AnyAppleOSUnavailableContainer {} // expected-error {{'AnyAppleOSUnavailableContainer' is unavailable in macOS}}
@available(OSXApplicationExtension, unavailable)
extension OSXUnavailableContainer {} // expected-error {{'OSXUnavailableContainer' is unavailable in macOS}}
@available(OSXApplicationExtension, unavailable)
extension OSXAppExtensionsUnavailableContainer {}

struct ExtendMe {}

@available(*, unavailable)
extension ExtendMe {
  func never_available_extension_available_method() {} // expected-note@-2 3 {{has been explicitly marked unavailable here}}

  @available(OSX 99, *)
  func never_available_extension_osx_future_method() {} // expected-note@-5 3 {{has been explicitly marked unavailable here}}

  func never_available_extension_available_method(
    _: NeverAvailable,
    _: OSXUnavailable,
    _: OSXAppExtensionsUnavailable
  ) {
    never() // expected-error {{'never()' is unavailable}}
    osx()
    osx_extension()
    any_apple_os_future()
    osx_future()
    osx_extension_future()
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
    any_apple_os_future()
    osx_future()
    osx_extension_future()
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
    any_apple_os_future()
    osx_future()
    osx_extension_future()
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
    any_apple_os_future()
    osx_future()
    osx_extension_future()
  }
}

@available(OSX, unavailable)
extension ExtendMe {
  func osx_extension_available_method() {} // expected-note@-2 2 {{'osx_extension_available_method()' has been explicitly marked unavailable here}}

  @available(OSX 99, *)
  func osx_extension_osx_future_method() {} // expected-note@-5 2 {{'osx_extension_osx_future_method()' has been explicitly marked unavailable here}}

  @available(*, unavailable) // expected-note {{'osx_extension_never_available_method()' has been explicitly marked unavailable here}}
  func osx_extension_never_available_method() {} // expected-note@-8 2 {{'osx_extension_never_available_method()' has been explicitly marked unavailable here}}

  @available(OSX, unavailable) // expected-note 2 {{'osx_extension_osx_method()' has been explicitly marked unavailable here}}
  func osx_extension_osx_method() {}

  @available(OSXApplicationExtension, unavailable) // expected-note {{'osx_extension_osx_app_extension_method()' has been explicitly marked unavailable here}}
  func osx_extension_osx_app_extension_method() {} // expected-note@-14 {{'osx_extension_osx_app_extension_method()' has been explicitly marked unavailable here}}

  func osx_extension_available_method(
    _: NeverAvailable, // expected-error {{'NeverAvailable' is unavailable}}
    _: OSXUnavailable,
    _: OSXAppExtensionsUnavailable
  ) {
    never() // expected-error {{'never()' is unavailable}}
    osx()
    osx_extension()
    any_apple_os_future()
    osx_future()
    osx_extension_future()
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
    any_apple_os_future()
    osx_future()
    osx_extension_future()
  }

  @available(OSX, unavailable)
  func osx_extension_osx_method(
    _: NeverAvailable, // expected-error {{'NeverAvailable' is unavailable}}
    _: OSXUnavailable,
    _: OSXAppExtensionsUnavailable
  ) {
    never() // expected-error {{'never()' is unavailable}}
    osx()
    osx_extension()
    any_apple_os_future()
    osx_future()
    osx_extension_future()
  }

  @available(OSXApplicationExtension, unavailable)
  func osx_extension_osx_app_extension_method(
    _: NeverAvailable, // expected-error {{'NeverAvailable' is unavailable}}
    _: OSXUnavailable,
    _: OSXAppExtensionsUnavailable
  ) {
    never() // expected-error {{'never()' is unavailable}}
    osx()
    osx_extension()
    any_apple_os_future()
    osx_future()
    osx_extension_future()
  }
}

@available(OSXApplicationExtension, unavailable)
extension ExtendMe {
  func osx_app_extension_extension_available_method() {} // expected-note@-2 {{'osx_app_extension_extension_available_method()' has been explicitly marked unavailable here}}

  @available(OSX 99, *)
  func osx_app_extension_extension_osx_future_method() {} // expected-note@-5 {{'osx_app_extension_extension_osx_future_method()'}}

  @available(*, unavailable) // expected-note 2 {{'osx_app_extension_extension_never_available_method()' has been explicitly marked unavailable here}}
  func osx_app_extension_extension_never_available_method() {} // expected-note@-8 {{'osx_app_extension_extension_never_available_method()' has been explicitly marked unavailable here}}

  @available(OSX, unavailable)
  func osx_app_extension_extension_osx_method() {} // expected-note@-11 {{'osx_app_extension_extension_osx_method()' has been explicitly marked unavailable here}}
  // expected-note@-2 {{'osx_app_extension_extension_osx_method()' has been explicitly marked unavailable here}}

  @available(OSXApplicationExtension, unavailable) // expected-note {{'osx_app_extension_extension_osx_app_extension_method()' has been explicitly marked unavailable here}}
  func osx_app_extension_extension_osx_app_extension_method() {}

  func osx_app_extension_extension_available_method( // expected-note 2 {{add '@available' attribute to enclosing instance method}}
    _: NeverAvailable, // expected-error {{'NeverAvailable' is unavailable}}
    _: OSXUnavailable, // expected-error {{'OSXUnavailable' is unavailable in macOS}}
    _: OSXAppExtensionsUnavailable
  ) {
    never() // expected-error {{'never()' is unavailable}}
    osx() // expected-error {{'osx()' is unavailable}}
    osx_extension()
    any_apple_os_future() // expected-error {{'any_apple_os_future()' is only available in macOS 99 or newer}}
    // expected-note@-1 {{add 'if #available' version check}}
    osx_future() // expected-error {{'osx_future()' is only available in macOS 99 or newer}}
    // expected-note@-1 {{add 'if #available' version check}}
    osx_extension_future()
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
    any_apple_os_future()
    osx_future()
    osx_extension_future()
  }

  @available(OSX, unavailable)
  func osx_app_extension_extension_osx_method(
    _: NeverAvailable, // expected-error {{'NeverAvailable' is unavailable}}
    _: OSXUnavailable,
    _: OSXAppExtensionsUnavailable
  ) {
    never() // expected-error {{'never()' is unavailable}}
    osx()
    osx_extension()
    any_apple_os_future()
    osx_future()
    osx_extension_future()
  }

  @available(OSXApplicationExtension, unavailable)
  func osx_app_extension_extension_osx_app_extension_method(
    _: NeverAvailable, // expected-error {{'NeverAvailable' is unavailable}}
    _: OSXUnavailable, // expected-error {{'OSXUnavailable' is unavailable in macOS}}
    _: OSXAppExtensionsUnavailable
  ) {
    never() // expected-error {{'never()' is unavailable}}
    osx() // expected-error {{'osx()' is unavailable}}
    osx_extension()
    any_apple_os_future() // expected-error {{'any_apple_os_future()' is only available in macOS 99 or newer}}
    // expected-note@-1 {{add 'if #available' version check}}
    osx_future() // expected-error {{'osx_future()' is only available in macOS 99 or newer}}
    // expected-note@-1 {{add 'if #available' version check}}
    osx_extension_future()
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
