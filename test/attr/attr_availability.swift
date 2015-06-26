// RUN: %target-parse-verify-swift

// REQUIRES: enable_target_appletvos

@available(*, unavailable)
func unavailable_func() {}

@available(*, unavailable, message="message")
func unavailable_func_with_message() {}

@available(tvOS, unavailable)
@available(watchOS, unavailable)
@available(iOS, unavailable)
@available(OSX, unavailable)
func unavailable_multiple_platforms() {}

@available // expected-error {{expected '(' in 'available' attribute}}
func noArgs() {}
@available(*) // expected-error {{expected ',' in 'available' attribute}}
func noKind() {}

@available(badPlatform, unavailable) // expected-error {{unknown platform 'badPlatform' for attribute 'available'}}
func unavailable_bad_platform() {}

// Handle unknown platform.
@available(HAL9000, unavailable) // expected-error {{unknown platform 'HAL9000'}}
func availabilityUnknownPlatform() {}

// <rdar://problem/17669805> Availability can't appear on a typealias
@available(*, unavailable, message="oh no you dont")
typealias int = Int // expected-note {{'int' has been explicitly marked unavailable here}}

@available(*, unavailable, renamed="Float")
typealias float = Float // expected-note {{'float' has been explicitly marked unavailable here}}

struct MyCollection<Element> {
  @available(*, unavailable, renamed="Element")
  typealias T = Element // expected-note 2{{'T' has been explicitly marked unavailable here}}

  func foo(x: T) { } // expected-error {{'T' has been renamed to Element}}
}

extension MyCollection {
  func append(element: T) { } // expected-error {{'T' has been renamed to Element}}
}

var x : int // expected-error {{'int' is unavailable: oh no you dont}}
var y : float // expected-error {{'float' has been renamed to Float}}{{9-14=Float}}

// Encoded message
@available(*, unavailable, message="This message has a double quote \"")
func unavailableWithDoubleQuoteInMessage() {} // expected-note {{'unavailableWithDoubleQuoteInMessage()' has been explicitly marked unavailable here}}

func useWithEscapedMessage() {
  unavailableWithDoubleQuoteInMessage() // expected-error {{'unavailableWithDoubleQuoteInMessage()' is unavailable: This message has a double quote \"}}
}


// More complicated parsing.
@available(OSX, message="x", unavailable)
let _: Int;

@available(OSX, introduced=1, deprecated=2.0, obsoleted=3.0.0)
let _: Int

@available(OSX, introduced=1.0.0, deprecated=2.0, obsoleted=3, unavailable, renamed="x")
let _: Int

// Meaningless but accepted.
@available(OSX, message="x")
let _: Int;


// Parse errors.
@available() // expected-error{{expected platform name or '*' for 'available' attribute}}
let _: Int

@available(OSX,) // expected-error{{expected 'available' option such as 'unavailable', 'introduced', 'deprecated', 'obsoleted', 'message', or 'renamed'}}
let _: Int

@available(OSX, message) // expected-error{{expected '=' after 'message' in 'available' attribute}}
let _: Int

@available(OSX, message=) // expected-error{{expected string literal in 'available' attribute}} expected-error{{postfix '=' is reserved}}
let _: Int

@available(OSX, message=x) // expected-error{{expected string literal in 'available' attribute}}
let _: Int

@available(OSX, unavailable=) // expected-error{{expected ')' in 'available' attribute}} expected-error{{postfix '=' is reserved}} expected-error{{expected declaration}}
let _: Int

@available(OSX, introduced) // expected-error{{expected '=' after 'introduced' in 'available' attribute}}
let _: Int

@available(OSX, introduced=) // expected-error{{expected version number in 'available' attribute}} expected-error{{postfix '=' is reserved}}
let _: Int

@available(OSX, introduced=x) // expected-error{{expected version number in 'available' attribute}}
let _: Int

@available(OSX, introduced=1.x) // expected-error{{expected ')' in 'available' attribute}} expected-error {{expected declaration}}
let _: Int

@available(OSX, introduced=1.0.x) // expected-error{{expected version number in 'available' attribute}}
let _: Int

@available(OSX, introduced=0x1) // expected-error{{expected version number in 'available' attribute}}
let _: Int

@available(OSX, introduced=1.0e4) // expected-error{{expected version number in 'available' attribute}}
let _: Int

@available(OSX, introduced=-1) // expected-error{{expected '=' after 'introduced' in 'available' attribute}} expected-error{{expected declaration}}
let _: Int

@available(OSX, introduced=1.0.1e4) // expected-error{{expected version number in 'available' attribute}}
let _: Int

@available(OSX, introduced=1.0.0x4) // expected-error{{expected version number in 'available' attribute}}
let _: Int

@available(*, deprecated, unavailable, message="message") // expected-error{{'available' attribute cannot be both unconditionally 'unavailable' and 'deprecated'}}
struct BadUnconditionalAvailability { };

// Encoding in messages
@available(*, deprecated, message="Say \"Hi\"")
func deprecated_func_with_message() {}

// 'PANDA FACE' (U+1F43C)
@available(*, deprecated, message="Pandas \u{1F43C} are cute")
struct DeprecatedTypeWithMessage { }

func use_deprecated_with_message() {
  deprecated_func_with_message() // expected-warning{{'deprecated_func_with_message()' is deprecated: Say \"Hi\"}}
  var _: DeprecatedTypeWithMessage // expected-warning{{'DeprecatedTypeWithMessage' is deprecated: Pandas \u{1F43C} are cute}}
}

@available(*, deprecated, message="message")
func use_deprecated_func_with_message2() {
 deprecated_func_with_message() // no diagnostic
}

@available(*, deprecated, renamed="blarg")
func deprecated_func_with_renamed() {}

@available(*, deprecated, message="blarg is your friend", renamed="blarg")
func deprecated_func_with_message_renamed() {}

@available(*, deprecated, renamed="wobble")
struct DeprecatedTypeWithRename { }

func use_deprecated_with_renamed() {
  deprecated_func_with_renamed() // expected-warning{{'deprecated_func_with_renamed()' is deprecated: renamed to 'blarg'}}
  // expected-note@-1{{use 'blarg'}}{{3-31=blarg}}

  deprecated_func_with_message_renamed() //expected-warning{{'deprecated_func_with_message_renamed()' is deprecated: blarg is your friend}}
  // expected-note@-1{{use 'blarg'}}{{3-39=blarg}}

  var _: DeprecatedTypeWithRename // expected-warning{{'DeprecatedTypeWithRename' is deprecated: renamed to 'wobble'}}
  // expected-note@-1{{use 'wobble'}}{{10-34=wobble}}
}

// Short form of @available()

@available(iOS 8.0, *)
func functionWithShortFormIOSAvailable() {}

@available(iOS 8, *)
func functionWithShortFormIOSVersionNoPointAvailable() {}

@available(iOS 8.0, OSX 10.10.3, *)
func functionWithShortFormIOSOSXAvailable() {}

@available(iOS 8.0 // expected-error {{must handle potential future platforms with '*'}}
func shortFormMissingParen() { // expected-error {{expected ')' in 'available' attribute}}
}

@available(iOS 8.0, // expected-error {{expected platform name}}
func shortFormMissingPlatform() {
}

@available(iOS 8.0, *
func shortFormMissingParenAfterWildcard() { // expected-error {{expected ')' in 'available' attribute}}
}

@available(*) // expected-error {{expected ',' in 'available' attribute}}
func onlyWildcardInAvailable() {}

@available(iOS 8.0, *, OSX 10.10.3)
func shortFormWithWildcardInMiddle() {}

@available(iOS 8.0, OSX 10.10.3) // expected-error {{must handle potential future platforms with '*'}}
func shortFormMissingWildcard() {}

@availability(OSX, introduced=10.10) // expected-error {{@availability has been renamed to @available}} {{2-14=available}}
func someFuncUsingOldAttribute() { }
