// RUN: %target-typecheck-verify-swift -parse-as-library
// REQUIRES: OS=macosx

// Allow referencing unavailable API in situations where the caller is marked unavailable in the same circumstances.

struct AlwaysAvailabile {}

@available(*, unavailable)
struct NeverAvailable {} // expected-note * {{'NeverAvailable' has been explicitly marked unavailable here}}

@available(OSX 99, *)
struct OSXFutureAvailable {}

@available(OSX, unavailable)
struct OSXUnavailable {} // expected-note * {{'OSXUnavailable' has been explicitly marked unavailable here}}

@available(iOS, unavailable)
@available(OSX, unavailable)
struct MultiPlatformUnavailable {} // expected-note * {{'MultiPlatformUnavailable' has been explicitly marked unavailable here}}

@available(OSXApplicationExtension, unavailable)
struct OSXAppExtensionsUnavailable {}

@discardableResult
func always() -> AlwaysAvailabile {
  AlwaysAvailabile()
}

@available(*, unavailable)
@discardableResult
func never() -> NeverAvailable { // expected-note * {{'never()' has been explicitly marked unavailable here}}
  NeverAvailable()
}

@available(OSX 99, *)
@discardableResult
func osx_future() -> OSXFutureAvailable {
  OSXFutureAvailable()
}

@available(OSX, unavailable)
@discardableResult
func osx() -> OSXUnavailable { // expected-note * {{'osx()' has been explicitly marked unavailable here}}
  OSXUnavailable()
}

@available(iOS, unavailable)
@available(OSX, unavailable)
@discardableResult
func osx_ios() -> MultiPlatformUnavailable { // expected-note * {{'osx_ios()' has been explicitly marked unavailable here}}
  MultiPlatformUnavailable()
}

@available(OSXApplicationExtension, unavailable)
@discardableResult
func osx_extension() -> OSXAppExtensionsUnavailable {
  OSXAppExtensionsUnavailable()
}

// MARK: Global functions

func available_func( // expected-note * {{add @available attribute to enclosing global function}}
  _: AlwaysAvailabile,
  _: NeverAvailable, // expected-error {{'NeverAvailable' is unavailable}}
  _: OSXFutureAvailable, // expected-error {{'OSXFutureAvailable' is only available in macOS 99 or newer}}
  _: OSXUnavailable, // expected-error {{'OSXUnavailable' is unavailable in macOS}}
  _: MultiPlatformUnavailable, // expected-error {{'MultiPlatformUnavailable' is unavailable in macOS}}
  _: OSXAppExtensionsUnavailable
) {
  always()
  never() // expected-error {{'never()' is unavailable}}
  osx_future() // expected-error {{'osx_future()' is only available in macOS 99 or newer}}
  // expected-note@-1 {{add 'if #available' version check}}
  osx() // expected-error {{'osx()' is unavailable}}
  osx_ios() // expected-error {{'osx_ios()' is unavailable}}
  osx_extension()
}

@available(*, unavailable)
func never_available_func(
  _: AlwaysAvailabile,
  _: NeverAvailable,
  _: OSXFutureAvailable,
  _: OSXUnavailable,
  _: MultiPlatformUnavailable,
  _: OSXAppExtensionsUnavailable
) {
  always()
  never() // expected-error {{'never()' is unavailable}}
  osx_future() // expected-error {{'osx_future()' is only available in macOS 99 or newer}}
  // expected-note@-1 {{add 'if #available' version check}}
  osx()
  osx_ios()
  osx_extension()
}

@available(OSX, unavailable)
func osx_func(
  _: AlwaysAvailabile,
  _: NeverAvailable,
  _: OSXFutureAvailable,
  _: OSXUnavailable,
  _: MultiPlatformUnavailable,
  _: OSXAppExtensionsUnavailable
) {
  always()
  never() // expected-error {{'never()' is unavailable}}
  osx_future() // expected-error {{'osx_future()' is only available in macOS 99 or newer}}
  // expected-note@-1 {{add 'if #available' version check}}
  osx()
  osx_ios()
  osx_extension()
}

@available(OSXApplicationExtension, unavailable)
func osx_extension_func( // expected-note 2 {{add @available attribute to enclosing global function}}
  _: AlwaysAvailabile,
  _: NeverAvailable, // expected-error {{'NeverAvailable' is unavailable}}
  _: OSXFutureAvailable, // expected-error {{'OSXFutureAvailable' is only available in macOS 99 or newer}}
  _: OSXUnavailable, // expected-error {{'OSXUnavailable' is unavailable in macOS}}
  _: MultiPlatformUnavailable, // expected-error {{'MultiPlatformUnavailable' is unavailable in macOS}}
  _: OSXAppExtensionsUnavailable
) {
  always()
  never() // expected-error {{'never()' is unavailable}}
  osx_future() // expected-error {{'osx_future()' is only available in macOS 99 or newer}}
  // expected-note@-1 {{add 'if #available' version check}}
  osx() // expected-error {{'osx()' is unavailable}}
  osx_ios() // expected-error {{'osx_ios()' is unavailable}}
  osx_extension()
}

// MARK: Global vars

var always_var: ( // expected-note 2 {{add @available attribute to enclosing var}}
  AlwaysAvailabile,
  NeverAvailable, // expected-error {{'NeverAvailable' is unavailable}}
  OSXFutureAvailable, // expected-error {{'OSXFutureAvailable' is only available in macOS 99 or newer}}
  OSXUnavailable, // expected-error {{'OSXUnavailable' is unavailable in macOS}}
  MultiPlatformUnavailable, // expected-error {{'MultiPlatformUnavailable' is unavailable in macOS}}
  OSXAppExtensionsUnavailable
) = (
  always(),
  never(), // expected-error {{'never()' is unavailable}}
  osx_future(), // expected-error {{'osx_future()' is only available in macOS 99 or newer}}
  osx(), // expected-error {{'osx()' is unavailable}}
  osx_ios(), // expected-error {{'osx_ios()' is unavailable}}
  osx_extension()
)

@available(*, unavailable)
var never_var: (
  AlwaysAvailabile,
  NeverAvailable,
  OSXFutureAvailable,
  OSXUnavailable,
  MultiPlatformUnavailable,
  OSXAppExtensionsUnavailable
) = (
  always(),
  never(), // expected-error {{'never()' is unavailable}}
  osx_future(), // expected-error {{'osx_future()' is only available in macOS 99 or newer}}
  osx(),
  osx_ios(),
  osx_extension()
)

@available(OSX, unavailable)
var osx_var: (
  AlwaysAvailabile,
  NeverAvailable,
  OSXFutureAvailable,
  OSXUnavailable,
  MultiPlatformUnavailable,
  OSXAppExtensionsUnavailable
) = (
  always(),
  never(), // expected-error {{'never()' is unavailable}}
  osx_future(), // expected-error {{'osx_future()' is only available in macOS 99 or newer}}
  osx(),
  osx_ios(),
  osx_extension()
)

@available(OSXApplicationExtension, unavailable)
var osx_extension_var: ( // expected-note 2 {{add @available attribute to enclosing var}}
  AlwaysAvailabile,
  NeverAvailable, // expected-error {{'NeverAvailable' is unavailable}}
  OSXFutureAvailable, // expected-error {{'OSXFutureAvailable' is only available in macOS 99 or newer}}
  OSXUnavailable, // expected-error {{'OSXUnavailable' is unavailable in macOS}}
  MultiPlatformUnavailable, // expected-error {{'MultiPlatformUnavailable' is unavailable in macOS}}
  OSXAppExtensionsUnavailable
) = (
  always(),
  never(), // expected-error {{'never()' is unavailable}}
  osx_future(), // expected-error {{'osx_future()' is only available in macOS 99 or newer}}
  osx(), // expected-error {{'osx()' is unavailable}}
  osx_ios(), // expected-error {{'osx_ios()' is unavailable}}
  osx_extension()
)

// MARK: Properties

struct AlwaysAvailabileContainer { // expected-note 2 {{add @available attribute to enclosing struct}}
  let always_var: AlwaysAvailabile = always()
  let never_var: NeverAvailable = never() // expected-error {{'never()' is unavailable}}
  // expected-error@-1 {{'NeverAvailable' is unavailable}}
  let osx_future_var: OSXFutureAvailable = osx_future() // expected-error {{'osx_future()' is only available in macOS 99 or newer}}
  // expected-error@-1 {{'OSXFutureAvailable' is only available in macOS 99 or newer}}
  let osx_var: OSXUnavailable = osx() // expected-error {{'osx()' is unavailable}}
  // expected-error@-1 {{'OSXUnavailable' is unavailable in macOS}}
  let osx_ios_var: MultiPlatformUnavailable = osx_ios() // expected-error {{'osx_ios()' is unavailable}}
  // expected-error@-1 {{'MultiPlatformUnavailable' is unavailable in macOS}}
  let osx_extension_var: OSXAppExtensionsUnavailable = osx_extension()
}

@available(*, unavailable)
struct NeverAvailableContainer { // expected-note 2 {{'NeverAvailableContainer' has been explicitly marked unavailable here}}
  let always_var: AlwaysAvailabile = always()
  let never_var: NeverAvailable = never() // expected-error {{'never()' is unavailable}}
  let osx_future_var: OSXFutureAvailable = osx_future() // expected-error {{'osx_future()' is only available in macOS 99 or newer}}
  let osx_var: OSXUnavailable = osx()
  let osx_ios_var: MultiPlatformUnavailable = osx_ios()
  let osx_extension_var: OSXAppExtensionsUnavailable = osx_extension()
}

@available(OSX, unavailable)
struct OSXUnavailableContainer { // expected-note 2 {{'OSXUnavailableContainer' has been explicitly marked unavailable here}}
  let always_var: AlwaysAvailabile = always()
  let never_var: NeverAvailable = never() // expected-error {{'never()' is unavailable}}
  let osx_future_var: OSXFutureAvailable = osx_future() // expected-error {{'osx_future()' is only available in macOS 99 or newer}}
  let osx_var: OSXUnavailable = osx()
  let osx_ios_var: MultiPlatformUnavailable = osx_ios()
  let osx_extension_var: OSXAppExtensionsUnavailable = osx_extension()
}

@available(OSXApplicationExtension, unavailable)
struct OSXAppExtensionsUnavailableContainer { // expected-note 2 {{add @available attribute to enclosing struct}}
  let always_var: AlwaysAvailabile = always()
  let never_var: NeverAvailable = never() // expected-error {{'never()' is unavailable}}
  // expected-error@-1 {{'NeverAvailable' is unavailable}}
  let osx_future_var: OSXFutureAvailable = osx_future() // expected-error {{'osx_future()' is only available in macOS 99 or newer}}
  // expected-error@-1 {{'OSXFutureAvailable' is only available in macOS 99 or newer}}
  let osx_var: OSXUnavailable = osx() // expected-error {{'osx()' is unavailable}}
  // expected-error@-1 {{'OSXUnavailable' is unavailable in macOS}}
  let osx_ios_var: MultiPlatformUnavailable = osx_ios() // expected-error {{'osx_ios()' is unavailable}}
  // expected-error@-1 {{'MultiPlatformUnavailable' is unavailable in macOS}}
  let osx_extension_var: OSXAppExtensionsUnavailable = osx_extension()
}

// MARK: Extensions

extension AlwaysAvailabileContainer {}
extension NeverAvailableContainer {} // expected-error {{'NeverAvailableContainer' is unavailable}}
extension OSXUnavailableContainer {} // expected-error {{'OSXUnavailableContainer' is unavailable in macOS}}
extension OSXAppExtensionsUnavailableContainer {}

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
extension NeverAvailableContainer {} // expected-error {{'NeverAvailableContainer' is unavailable}}
@available(OSXApplicationExtension, unavailable)
extension OSXUnavailableContainer {} // expected-error {{'OSXUnavailableContainer' is unavailable in macOS}}
@available(OSXApplicationExtension, unavailable)
extension OSXAppExtensionsUnavailableContainer {}

struct ExtendMe {}

@available(*, unavailable)
extension ExtendMe {
  func never_available_extension_available_method() {} // expected-note {{has been explicitly marked unavailable here}}

  @available(OSX 99, *)
  func never_available_extension_osx_future_method() {} // expected-note {{has been explicitly marked unavailable here}}

  func never_available_extension_available_method( // expected-note * {{add @available attribute to enclosing instance method}}
    _: AlwaysAvailabile,
    _: NeverAvailable,
    _: OSXFutureAvailable,
    _: OSXUnavailable,
    _: MultiPlatformUnavailable,
    _: OSXAppExtensionsUnavailable
  ) {
    always()
    never() // expected-error {{'never()' is unavailable}}
    osx_future() // expected-error {{'osx_future()' is only available in macOS 99 or newer}}
    // expected-note@-1 {{add 'if #available' version check}}
    osx()
    osx_ios()
    osx_extension()
  }

  @available(*, unavailable)
  func never_available_extension_never_available_method(
    _: AlwaysAvailabile,
    _: NeverAvailable,
    _: OSXFutureAvailable,
    _: OSXUnavailable,
    _: MultiPlatformUnavailable,
    _: OSXAppExtensionsUnavailable
  ) {
    always()
    never() // expected-error {{'never()' is unavailable}}
    osx_future() // expected-error {{'osx_future()' is only available in macOS 99 or newer}}
    // expected-note@-1 {{add 'if #available' version check}}
    osx()
    osx_ios()
    osx_extension()
  }

  @available(OSX, unavailable)
  func never_available_extension_osx_method(
    _: AlwaysAvailabile,
    _: NeverAvailable,
    _: OSXFutureAvailable,
    _: OSXUnavailable,
    _: MultiPlatformUnavailable,
    _: OSXAppExtensionsUnavailable
  ) {
    always()
    never() // expected-error {{'never()' is unavailable}}
    osx_future() // expected-error {{'osx_future()' is only available in macOS 99 or newer}}
    // expected-note@-1 {{add 'if #available' version check}}
    osx()
    osx_ios()
    osx_extension()
  }

  @available(OSXApplicationExtension, unavailable)
  func never_available_extension_osx_app_extension_method( // expected-note {{add @available attribute to enclosing instance method}}
    _: AlwaysAvailabile,
    _: NeverAvailable,
    _: OSXFutureAvailable,
    _: OSXUnavailable,
    _: MultiPlatformUnavailable,
    _: OSXAppExtensionsUnavailable
  ) {
    always()
    never() // expected-error {{'never()' is unavailable}}
    osx_future() // expected-error {{'osx_future()' is only available in macOS 99 or newer}}
    // expected-note@-1 {{add 'if #available' version check}}
    osx()
    osx_ios()
    osx_extension()
  }
}

@available(OSX, unavailable)
extension ExtendMe {
  func osx_extension_available_method() {} // expected-note {{has been explicitly marked unavailable here}}

  @available(OSX 99, *)
  func osx_extension_osx_future_method() {} // expected-note {{has been explicitly marked unavailable here}}

  func osx_extension_available_method( // expected-note * {{add @available attribute to enclosing instance method}}
    _: AlwaysAvailabile,
    _: NeverAvailable,
    _: OSXFutureAvailable,
    _: OSXUnavailable,
    _: MultiPlatformUnavailable,
    _: OSXAppExtensionsUnavailable
  ) {
    always()
    never() // expected-error {{'never()' is unavailable}}
    osx_future() // expected-error {{'osx_future()' is only available in macOS 99 or newer}}
    // expected-note@-1 {{add 'if #available' version check}}
    osx()
    osx_ios()
    osx_extension()
  }

  @available(*, unavailable)
  func osx_extension_never_available_method(
    _: AlwaysAvailabile,
    _: NeverAvailable,
    _: OSXFutureAvailable,
    _: OSXUnavailable,
    _: MultiPlatformUnavailable,
    _: OSXAppExtensionsUnavailable
  ) {
    always()
    never() // expected-error {{'never()' is unavailable}}
    osx_future() // expected-error {{'osx_future()' is only available in macOS 99 or newer}}
    // expected-note@-1 {{add 'if #available' version check}}
    osx()
    osx_ios()
    osx_extension()
  }

  @available(OSX, unavailable)
  func osx_extension_osx_method(
    _: AlwaysAvailabile,
    _: NeverAvailable,
    _: OSXFutureAvailable,
    _: OSXUnavailable,
    _: MultiPlatformUnavailable,
    _: OSXAppExtensionsUnavailable
  ) {
    always()
    never() // expected-error {{'never()' is unavailable}}
    osx_future() // expected-error {{'osx_future()' is only available in macOS 99 or newer}}
    // expected-note@-1 {{add 'if #available' version check}}
    osx()
    osx_ios()
    osx_extension()
  }

  @available(OSXApplicationExtension, unavailable)
  func osx_extension_osx_app_extension_method( // expected-note {{add @available attribute to enclosing instance method}}
    _: AlwaysAvailabile,
    _: NeverAvailable,
    _: OSXFutureAvailable,
    _: OSXUnavailable,
    _: MultiPlatformUnavailable,
    _: OSXAppExtensionsUnavailable
  ) {
    always()
    never() // expected-error {{'never()' is unavailable}}
    osx_future() // expected-error {{'osx_future()' is only available in macOS 99 or newer}}
    // expected-note@-1 {{add 'if #available' version check}}
    osx()
    osx_ios()
    osx_extension()
  }
}

@available(OSXApplicationExtension, unavailable)
extension ExtendMe { // expected-note * {{add @available attribute to enclosing extension}}
  func osx_app_extension_extension_available_method() {}

  @available(OSX 99, *)
  func osx_app_extension_extension_osx_future_method() {}

  func osx_app_extension_extension_available_method( // expected-note * {{add @available attribute to enclosing instance method}}
    _: AlwaysAvailabile,
    _: NeverAvailable, // expected-error {{'NeverAvailable' is unavailable}}
    _: OSXFutureAvailable, // expected-error {{'OSXFutureAvailable' is only available in macOS 99 or newer}}
    _: OSXUnavailable, // expected-error {{'OSXUnavailable' is unavailable in macOS}}
    _: MultiPlatformUnavailable, // expected-error {{'MultiPlatformUnavailable' is unavailable in macOS}}
    _: OSXAppExtensionsUnavailable
  ) {
    always()
    never() // expected-error {{'never()' is unavailable}}
    osx_future() // expected-error {{'osx_future()' is only available in macOS 99 or newer}}
    // expected-note@-1 {{add 'if #available' version check}}
    osx() // expected-error {{'osx()' is unavailable}}
    osx_ios() // expected-error {{'osx_ios()' is unavailable}}
    osx_extension()
  }

  @available(*, unavailable)
  func osx_app_extension_extension_never_available_method(
    _: AlwaysAvailabile,
    _: NeverAvailable,
    _: OSXFutureAvailable,
    _: OSXUnavailable,
    _: MultiPlatformUnavailable,
    _: OSXAppExtensionsUnavailable
  ) {
    always()
    never() // expected-error {{'never()' is unavailable}}
    osx_future() // expected-error {{'osx_future()' is only available in macOS 99 or newer}}
    // expected-note@-1 {{add 'if #available' version check}}
    osx()
    osx_ios()
    osx_extension()
  }

  @available(OSX, unavailable)
  func osx_app_extension_extension_osx_method(
    _: AlwaysAvailabile,
    _: NeverAvailable,
    _: OSXFutureAvailable,
    _: OSXUnavailable,
    _: MultiPlatformUnavailable,
    _: OSXAppExtensionsUnavailable
  ) {
    always()
    never() // expected-error {{'never()' is unavailable}}
    osx_future() // expected-error {{'osx_future()' is only available in macOS 99 or newer}}
    // expected-note@-1 {{add 'if #available' version check}}
    osx()
    osx_ios()
    osx_extension()
  }

  @available(OSXApplicationExtension, unavailable)
  func osx_app_extension_extension_osx_app_extension_method( // expected-note 2 {{add @available attribute to enclosing instance method}}
    _: AlwaysAvailabile,
    _: NeverAvailable, // expected-error {{'NeverAvailable' is unavailable}}
    _: OSXFutureAvailable, // expected-error {{'OSXFutureAvailable' is only available in macOS 99 or newer}}
    _: OSXUnavailable, // expected-error {{'OSXUnavailable' is unavailable in macOS}}
    _: MultiPlatformUnavailable, // expected-error {{'MultiPlatformUnavailable' is unavailable in macOS}}
    _: OSXAppExtensionsUnavailable
  ) {
    always()
    never() // expected-error {{'never()' is unavailable}}
    osx_future() // expected-error {{'osx_future()' is only available in macOS 99 or newer}}
    // expected-note@-1 {{add 'if #available' version check}}
    osx() // expected-error {{'osx()' is unavailable}}
    osx_ios() // expected-error {{'osx_ios()' is unavailable}}
    osx_extension()
  }
}

func available_func_call_extension_methods(_ e: ExtendMe) { // expected-note {{add @available attribute to enclosing global function}}
  e.never_available_extension_available_method() // expected-error {{'never_available_extension_available_method()' is unavailable}}
  e.osx_extension_available_method() // expected-error {{'osx_extension_available_method()' is unavailable in macOS}}
  e.osx_app_extension_extension_available_method()

  // rdar://92551870
  e.never_available_extension_osx_future_method() // expected-error {{'never_available_extension_osx_future_method()' is unavailable}}
  e.osx_extension_osx_future_method() // expected-error {{'osx_extension_osx_future_method()' is unavailable in macOS}}
  e.osx_app_extension_extension_osx_future_method() // expected-error {{'osx_app_extension_extension_osx_future_method()' is only available in macOS 99 or newer}}
  // expected-note@-1 {{add 'if #available' version check}}
}
