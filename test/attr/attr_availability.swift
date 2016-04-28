// RUN: %target-parse-verify-swift

@available(*, unavailable)
func unavailable_func() {}

@available(*, unavailable, message: "message")
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

@available(badPlatform, unavailable) // expected-warning {{unknown platform 'badPlatform' for attribute 'available'}}
func unavailable_bad_platform() {}

// Handle unknown platform.
@available(HAL9000, unavailable) // expected-warning {{unknown platform 'HAL9000'}}
func availabilityUnknownPlatform() {}

// <rdar://problem/17669805> Availability can't appear on a typealias
@available(*, unavailable, message: "oh no you don't")
typealias int = Int // expected-note {{'int' has been explicitly marked unavailable here}}

@available(*, unavailable, renamed: "Float")
typealias float = Float // expected-note {{'float' has been explicitly marked unavailable here}}

struct MyCollection<Element> {
  @available(*, unavailable, renamed: "Element")
  typealias T = Element // expected-note 2{{'T' has been explicitly marked unavailable here}}

  func foo(x: T) { } // expected-error {{'T' has been renamed to 'Element'}} {{15-16=Element}}
}

extension MyCollection {
  func append(element: T) { } // expected-error {{'T' has been renamed to 'Element'}} {{24-25=Element}}
}

var x : int // expected-error {{'int' is unavailable: oh no you don't}}
var y : float // expected-error {{'float' has been renamed to 'Float'}}{{9-14=Float}}

// Encoded message
@available(*, unavailable, message: "This message has a double quote \"")
func unavailableWithDoubleQuoteInMessage() {} // expected-note {{'unavailableWithDoubleQuoteInMessage()' has been explicitly marked unavailable here}}

func useWithEscapedMessage() {
  unavailableWithDoubleQuoteInMessage() // expected-error {{'unavailableWithDoubleQuoteInMessage()' is unavailable: This message has a double quote \"}}
}


// More complicated parsing.
@available(OSX, message: "x", unavailable)
let _: Int

@available(OSX, introduced: 1, deprecated: 2.0, obsoleted: 3.0.0)
let _: Int

@available(OSX, introduced: 1.0.0, deprecated: 2.0, obsoleted: 3, unavailable, renamed: "x")
let _: Int

// Meaningless but accepted.
@available(OSX, message: "x")
let _: Int


// Parse errors.
@available() // expected-error{{expected platform name or '*' for 'available' attribute}}
let _: Int

@available(OSX,) // expected-error{{expected 'available' option such as 'unavailable', 'introduced', 'deprecated', 'obsoleted', 'message', or 'renamed'}}
let _: Int

@available(OSX, message) // expected-error{{expected ':' after 'message' in 'available' attribute}}
let _: Int


@available(OSX, message: ) // expected-error{{expected string literal in 'available' attribute}}
let _: Int

@available(OSX, message: x) // expected-error{{expected string literal in 'available' attribute}}
let _: Int

@available(OSX, unavailable:) // expected-error{{expected ')' in 'available' attribute}} expected-error{{expected declaration}}
let _: Int

@available(OSX, introduced) // expected-error{{expected ':' after 'introduced' in 'available' attribute}}
let _: Int

@available(OSX, introduced: ) // expected-error{{expected version number in 'available' attribute}}
let _: Int

@available(OSX, introduced: x) // expected-error{{expected version number in 'available' attribute}}
let _: Int

@available(OSX, introduced: 1.x) // expected-error{{expected ')' in 'available' attribute}} expected-error {{expected declaration}}
let _: Int

@available(OSX, introduced: 1.0.x) // expected-error{{expected version number in 'available' attribute}}
let _: Int

@available(OSX, introduced: 0x1) // expected-error{{expected version number in 'available' attribute}}
let _: Int

@available(OSX, introduced: 1.0e4) // expected-error{{expected version number in 'available' attribute}}
let _: Int

@available(OSX, introduced: -1) // expected-error{{expected version number in 'available' attribute}} expected-error{{expected declaration}}
let _: Int

@available(OSX, introduced: 1.0.1e4) // expected-error{{expected version number in 'available' attribute}}
let _: Int

@available(OSX, introduced: 1.0.0x4) // expected-error{{expected version number in 'available' attribute}}
let _: Int

@available(*, renamed: "bad name") // expected-error{{'renamed' argument of 'available' attribute must be an operator, identifier, or full function name, optionally prefixed by a type name}}
let _: Int

@available(*, renamed: "Overly.Nested.Name") // expected-error{{'renamed' argument of 'available' attribute must be an operator, identifier, or full function name, optionally prefixed by a type name}}
let _: Int

@available(*, renamed: "_") // expected-error{{'renamed' argument of 'available' attribute must be an operator, identifier, or full function name, optionally prefixed by a type name}}
let _: Int

@available(*, renamed: "a+b") // expected-error{{'renamed' argument of 'available' attribute must be an operator, identifier, or full function name, optionally prefixed by a type name}}
let _: Int

@available(*, renamed: "a(") // expected-error{{'renamed' argument of 'available' attribute must be an operator, identifier, or full function name, optionally prefixed by a type name}}
let _: Int

@available(*, renamed: "a(:)") // expected-error{{'renamed' argument of 'available' attribute must be an operator, identifier, or full function name, optionally prefixed by a type name}}
let _: Int

@available(*, renamed: "a(:b:)") // expected-error{{'renamed' argument of 'available' attribute must be an operator, identifier, or full function name, optionally prefixed by a type name}}
let _: Int

@available(*, deprecated, unavailable, message: "message") // expected-error{{'available' attribute cannot be both unconditionally 'unavailable' and 'deprecated'}}
struct BadUnconditionalAvailability { };

// Encoding in messages
@available(*, deprecated, message: "Say \"Hi\"")
func deprecated_func_with_message() {}

// 'PANDA FACE' (U+1F43C)
@available(*, deprecated, message: "Pandas \u{1F43C} are cute")
struct DeprecatedTypeWithMessage { }

func use_deprecated_with_message() {
  deprecated_func_with_message() // expected-warning{{'deprecated_func_with_message()' is deprecated: Say \"Hi\"}}
  var _: DeprecatedTypeWithMessage // expected-warning{{'DeprecatedTypeWithMessage' is deprecated: Pandas \u{1F43C} are cute}}
}

@available(*, deprecated, message: "message")
func use_deprecated_func_with_message2() {
 deprecated_func_with_message() // no diagnostic
}

@available(*, deprecated, renamed: "blarg")
func deprecated_func_with_renamed() {}

@available(*, deprecated, message: "blarg is your friend", renamed: "blarg")
func deprecated_func_with_message_renamed() {}

@available(*, deprecated, renamed: "wobble")
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

@available(iOS 8.0 // expected-error {{must handle potential future platforms with '*'}} {{19-19=, *}}
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

@available(iOS 8.0, OSX 10.10.3) // expected-error {{must handle potential future platforms with '*'}} {{32-32=, *}}
func shortFormMissingWildcard() {}

@availability(OSX, introduced: 10.10) // expected-error {{@availability has been renamed to @available}} {{2-14=available}}
func someFuncUsingOldAttribute() { }


// <rdar://problem/23853709> Compiler crash on call to unavailable "print"
func OutputStreamTest(message: String, to: inout OutputStream) {
  print(message, &to)  // expected-error {{'print' is unavailable: Please use the 'to' label for the target stream: 'print((...), to: &...)'}}
}

// expected-note@+1{{'T' has been explicitly marked unavailable here}}
struct UnavailableGenericParam<@available(*, unavailable, message: "nope") T> {
  func f(t: T) { } // expected-error{{'T' is unavailable: nope}}
}


struct DummyType {}

@available(*, unavailable, renamed: "&+")
func +(x: DummyType, y: DummyType) {} // expected-note {{here}}
@available(*, deprecated, renamed: "&-")
func -(x: DummyType, y: DummyType) {}

func testOperators(x: DummyType, y: DummyType) {
  x + y // expected-error {{'+' has been renamed to '&+'}} {{5-6=&+}}
  x - y // expected-warning {{'-' is deprecated: renamed to '&-'}} expected-note {{use '&-' instead}} {{5-6=&-}}
}

@available(*, unavailable, renamed: "DummyType.foo")
func unavailableMember() {} // expected-note {{here}}
@available(*, deprecated, renamed: "DummyType.bar")
func deprecatedMember() {}

@available(*, unavailable, renamed: "DummyType.Foo")
struct UnavailableType {} // expected-note {{here}}
@available(*, deprecated, renamed: "DummyType.Bar")
typealias DeprecatedType = Int

func testGlobalToMembers() {
  unavailableMember() // expected-error {{'unavailableMember()' has been renamed to 'DummyType.foo'}} {{3-20=DummyType.foo}}
  deprecatedMember() // expected-warning {{'deprecatedMember()' is deprecated: renamed to 'DummyType.bar'}} expected-note {{use 'DummyType.bar' instead}} {{3-19=DummyType.bar}}
  let x: UnavailableType? = nil // expected-error {{'UnavailableType' has been renamed to 'DummyType.Foo'}} {{10-25=DummyType.Foo}}
  _ = x
  let y: DeprecatedType? = nil // expected-warning {{'DeprecatedType' is deprecated: renamed to 'DummyType.Bar'}} expected-note {{use 'DummyType.Bar' instead}} {{10-24=DummyType.Bar}}
  _ = y
}


@available(*, unavailable, renamed: "shinyLabeledArguments(example:)")
func unavailableArgNames(a: Int) {} // expected-note {{here}}
@available(*, deprecated, renamed: "moreShinyLabeledArguments(example:)")
func deprecatedArgNames(b: Int) {}
@available(*, unavailable, renamed: "DummyType.shinyLabeledArguments(example:)")
func unavailableMemberArgNames(a: Int) {} // expected-note {{here}}
@available(*, deprecated, renamed: "DummyType.moreShinyLabeledArguments(example:)")
func deprecatedMemberArgNames(b: Int) {}
@available(*, unavailable, renamed: "DummyType.shinyLabeledArguments(example:)", message: "ha")
func unavailableMemberArgNamesMsg(a: Int) {} // expected-note {{here}}
@available(*, deprecated, renamed: "DummyType.moreShinyLabeledArguments(example:)", message: "ha")
func deprecatedMemberArgNamesMsg(b: Int) {}

func testArgNames() {
  // FIXME: These are all wrong.
  unavailableArgNames(a: 0) // expected-error {{'unavailableArgNames(a:)' has been renamed to 'shinyLabeledArguments(example:)'}} {{3-22=shinyLabeledArguments(example:)}}
  deprecatedArgNames(b: 1) // expected-warning {{'deprecatedArgNames(b:)' is deprecated: renamed to 'moreShinyLabeledArguments(example:)'}} expected-note {{use 'moreShinyLabeledArguments(example:)' instead}} {{3-21=moreShinyLabeledArguments(example:)}}

  unavailableMemberArgNames(a: 0) // expected-error {{'unavailableMemberArgNames(a:)' has been renamed to 'DummyType.shinyLabeledArguments(example:)'}} {{3-28=DummyType.shinyLabeledArguments(example:)}}
  deprecatedMemberArgNames(b: 1) // expected-warning {{'deprecatedMemberArgNames(b:)' is deprecated: renamed to 'DummyType.moreShinyLabeledArguments(example:)'}} expected-note {{use 'DummyType.moreShinyLabeledArguments(example:)' instead}} {{3-27=DummyType.moreShinyLabeledArguments(example:)}}

  unavailableMemberArgNamesMsg(a: 0) // expected-error {{'unavailableMemberArgNamesMsg(a:)' has been renamed to 'DummyType.shinyLabeledArguments(example:)': ha}} {{3-31=DummyType.shinyLabeledArguments(example:)}}
  deprecatedMemberArgNamesMsg(b: 1) // expected-warning {{'deprecatedMemberArgNamesMsg(b:)' is deprecated: ha}} expected-note {{use 'DummyType.moreShinyLabeledArguments(example:)' instead}} {{3-30=DummyType.moreShinyLabeledArguments(example:)}}
}
