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

@available(*, unavailable, renamed: "shinyLabeledArguments()")
func unavailableNoArgs() {} // expected-note {{here}}

@available(*, unavailable, renamed: "shinyLabeledArguments(a:)")
func unavailableSame(a: Int) {} // expected-note {{here}}
@available(*, unavailable, renamed: "shinyLabeledArguments(example:)")
func unavailableUnnamed(_ a: Int) {} // expected-note {{here}}
@available(*, unavailable, renamed: "shinyLabeledArguments(_:)")
func unavailableUnnamedSame(_ a: Int) {} // expected-note {{here}}
@available(*, unavailable, renamed: "shinyLabeledArguments(_:)")
func unavailableNewlyUnnamed(a: Int) {} // expected-note {{here}}
@available(*, unavailable, renamed: "shinyLabeledArguments(veryLongNameToOverflowASmallStringABCDEFGHIJKLMNOPQRSTUVWXYZ:)")
func unavailableVeryLongArgNames(a: Int) {} // expected-note {{here}}

@available(*, unavailable, renamed: "shinyLabeledArguments(a:b:)")
func unavailableMultiSame(a: Int, b: Int) {} // expected-note {{here}}
@available(*, unavailable, renamed: "shinyLabeledArguments(example:another:)")
func unavailableMultiUnnamed(_ a: Int, _ b: Int) {} // expected-note {{here}}
@available(*, unavailable, renamed: "shinyLabeledArguments(_:_:)")
func unavailableMultiUnnamedSame(_ a: Int, _ b: Int) {} // expected-note {{here}}
@available(*, unavailable, renamed: "shinyLabeledArguments(_:_:)")
func unavailableMultiNewlyUnnamed(a: Int, b: Int) {} // expected-note {{here}}


func testArgNames() {
  unavailableArgNames(a: 0) // expected-error {{'unavailableArgNames(a:)' has been renamed to 'shinyLabeledArguments(example:)'}} {{3-22=shinyLabeledArguments}} {{23-24=example}}
  deprecatedArgNames(b: 1) // expected-warning {{'deprecatedArgNames(b:)' is deprecated: renamed to 'moreShinyLabeledArguments(example:)'}} expected-note {{use 'moreShinyLabeledArguments(example:)' instead}} {{3-21=moreShinyLabeledArguments}} {{22-23=example}}

  unavailableMemberArgNames(a: 0) // expected-error {{'unavailableMemberArgNames(a:)' has been renamed to 'DummyType.shinyLabeledArguments(example:)'}} {{3-28=DummyType.shinyLabeledArguments}} {{29-30=example}}
  deprecatedMemberArgNames(b: 1) // expected-warning {{'deprecatedMemberArgNames(b:)' is deprecated: renamed to 'DummyType.moreShinyLabeledArguments(example:)'}} expected-note {{use 'DummyType.moreShinyLabeledArguments(example:)' instead}} {{3-27=DummyType.moreShinyLabeledArguments}} {{28-29=example}}

  unavailableMemberArgNamesMsg(a: 0) // expected-error {{'unavailableMemberArgNamesMsg(a:)' has been renamed to 'DummyType.shinyLabeledArguments(example:)': ha}} {{3-31=DummyType.shinyLabeledArguments}} {{32-33=example}}
  deprecatedMemberArgNamesMsg(b: 1) // expected-warning {{'deprecatedMemberArgNamesMsg(b:)' is deprecated: ha}} expected-note {{use 'DummyType.moreShinyLabeledArguments(example:)' instead}} {{3-30=DummyType.moreShinyLabeledArguments}} {{31-32=example}}

  unavailableNoArgs() // expected-error {{'unavailableNoArgs()' has been renamed to 'shinyLabeledArguments()'}} {{3-20=shinyLabeledArguments}}
  unavailableSame(a: 0) // expected-error {{'unavailableSame(a:)' has been renamed to 'shinyLabeledArguments(a:)'}} {{3-18=shinyLabeledArguments}}
  unavailableUnnamed(0) // expected-error {{'unavailableUnnamed' has been renamed to 'shinyLabeledArguments(example:)'}} {{3-21=shinyLabeledArguments}} {{22-22=example: }}
  unavailableUnnamedSame(0) // expected-error {{'unavailableUnnamedSame' has been renamed to 'shinyLabeledArguments(_:)'}} {{3-25=shinyLabeledArguments}}
  unavailableNewlyUnnamed(a: 0) // expected-error {{'unavailableNewlyUnnamed(a:)' has been renamed to 'shinyLabeledArguments(_:)'}} {{3-26=shinyLabeledArguments}} {{27-30=}}
  unavailableVeryLongArgNames(a: 0) // expected-error {{'unavailableVeryLongArgNames(a:)' has been renamed to 'shinyLabeledArguments(veryLongNameToOverflowASmallStringABCDEFGHIJKLMNOPQRSTUVWXYZ:)'}} {{3-30=shinyLabeledArguments}} {{31-32=veryLongNameToOverflowASmallStringABCDEFGHIJKLMNOPQRSTUVWXYZ}}
  unavailableMultiSame(a: 0, b: 1) // expected-error {{'unavailableMultiSame(a:b:)' has been renamed to 'shinyLabeledArguments(a:b:)'}} {{3-23=shinyLabeledArguments}}
  unavailableMultiUnnamed(0, 1) // expected-error {{'unavailableMultiUnnamed' has been renamed to 'shinyLabeledArguments(example:another:)'}} {{3-26=shinyLabeledArguments}} {{27-27=example: }} {{30-30=another: }}
  unavailableMultiUnnamedSame(0, 1) // expected-error {{'unavailableMultiUnnamedSame' has been renamed to 'shinyLabeledArguments(_:_:)'}} {{3-30=shinyLabeledArguments}}
  unavailableMultiNewlyUnnamed(a: 0, b: 1) // expected-error {{'unavailableMultiNewlyUnnamed(a:b:)' has been renamed to 'shinyLabeledArguments(_:_:)'}} {{3-31=shinyLabeledArguments}} {{32-35=}} {{38-41=}}
}

@available(*, unavailable, renamed: "shinyLabeledArguments()")
func unavailableTooFew(a: Int) {} // expected-note {{here}}
@available(*, unavailable, renamed: "shinyLabeledArguments()")
func unavailableTooFewUnnamed(_ a: Int) {} // expected-note {{here}}
@available(*, unavailable, renamed: "shinyLabeledArguments(a:b:)")
func unavailableTooMany(a: Int) {} // expected-note {{here}}
@available(*, unavailable, renamed: "shinyLabeledArguments(a:b:)")
func unavailableTooManyUnnamed(_ a: Int) {} // expected-note {{here}}
@available(*, unavailable, renamed: "shinyLabeledArguments(a:)")
func unavailableNoArgsTooMany() {} // expected-note {{here}}

func testRenameArgMismatch() {
  unavailableTooFew(a: 0) // expected-error{{'unavailableTooFew(a:)' has been renamed to 'shinyLabeledArguments()'}} {{3-20=shinyLabeledArguments}}
  unavailableTooFewUnnamed(0) // expected-error{{'unavailableTooFewUnnamed' has been renamed to 'shinyLabeledArguments()'}} {{3-27=shinyLabeledArguments}}
  unavailableTooMany(a: 0) // expected-error{{'unavailableTooMany(a:)' has been renamed to 'shinyLabeledArguments(a:b:)'}} {{3-21=shinyLabeledArguments}}
  unavailableTooManyUnnamed(0) // expected-error{{'unavailableTooManyUnnamed' has been renamed to 'shinyLabeledArguments(a:b:)'}} {{3-28=shinyLabeledArguments}}
  unavailableNoArgsTooMany() // expected-error{{'unavailableNoArgsTooMany()' has been renamed to 'shinyLabeledArguments(a:)'}} {{3-27=shinyLabeledArguments}}
}

@available(*, unavailable, renamed: "Int.foo(self:)")
func unavailableInstance(a: Int) {} // expected-note 2 {{here}}
@available(*, unavailable, renamed: "Int.foo(self:)")
func unavailableInstanceUnlabeled(_ a: Int) {} // expected-note {{here}}
@available(*, unavailable, renamed: "Int.foo(self:other:)")
func unavailableInstanceFirst(a: Int, b: Int) {} // expected-note {{here}}
@available(*, unavailable, renamed: "Int.foo(other:self:)")
func unavailableInstanceSecond(a: Int, b: Int) {} // expected-note {{here}}
@available(*, unavailable, renamed: "Int.foo(_:self:c:)")
func unavailableInstanceSecondOfThree(a: Int, b: Int, c: Int) {} // expected-note {{here}}

func testRenameInstance() {
  unavailableInstance(a: 0) // expected-error{{'unavailableInstance(a:)' has been renamed to 'Int.foo(self:)'}} {{3-22=0.foo}} {{23-27=}}
  unavailableInstanceUnlabeled(0) // expected-error{{'unavailableInstanceUnlabeled' has been renamed to 'Int.foo(self:)'}} {{3-31=0.foo}} {{31-34=}}
  unavailableInstanceFirst(a: 0, b: 1) // expected-error{{'unavailableInstanceFirst(a:b:)' has been renamed to 'Int.foo(self:other:)'}} {{3-27=0.foo}} {{28-32=}} {{34-35=other}}
  unavailableInstanceSecond(a: 0, b: 1) // expected-error{{'unavailableInstanceSecond(a:b:)' has been renamed to 'Int.foo(other:self:)'}} {{3-28=1.foo}} {{29-30=other}} {{33-39=}}
  unavailableInstanceSecondOfThree(a: 0, b: 1, c: 2) // expected-error{{'unavailableInstanceSecondOfThree(a:b:c:)' has been renamed to 'Int.foo(_:self:c:)'}} {{3-35=1.foo}} {{36-39=}} {{40-46=}}

  unavailableInstance(a: 0 + 0) // expected-error{{'unavailableInstance(a:)' has been renamed to 'Int.foo(self:)'}} {{3-22=(0 + 0).foo}} {{23-31=}}
}

@available(*, unavailable, renamed: "Int.shinyLabeledArguments(self:)")
func unavailableInstanceTooFew(a: Int, b: Int) {} // expected-note {{here}}
@available(*, unavailable, renamed: "Int.shinyLabeledArguments(self:)")
func unavailableInstanceTooFewUnnamed(_ a: Int, _ b: Int) {} // expected-note {{here}}
@available(*, unavailable, renamed: "Int.shinyLabeledArguments(self:b:)")
func unavailableInstanceTooMany(a: Int) {} // expected-note {{here}}
@available(*, unavailable, renamed: "Int.shinyLabeledArguments(self:b:)")
func unavailableInstanceTooManyUnnamed(_ a: Int) {} // expected-note {{here}}
@available(*, unavailable, renamed: "Int.shinyLabeledArguments(self:)")
func unavailableInstanceNoArgsTooMany() {} // expected-note {{here}}

func testRenameInstanceArgMismatch() {
  unavailableInstanceTooFew(a: 0, b: 1) // expected-error{{'unavailableInstanceTooFew(a:b:)' has been renamed to 'Int.shinyLabeledArguments(self:)'}} {{none}}
  unavailableInstanceTooFewUnnamed(0, 1) // expected-error{{'unavailableInstanceTooFewUnnamed' has been renamed to 'Int.shinyLabeledArguments(self:)'}} {{none}}
  unavailableInstanceTooMany(a: 0) // expected-error{{'unavailableInstanceTooMany(a:)' has been renamed to 'Int.shinyLabeledArguments(self:b:)'}} {{none}}
  unavailableInstanceTooManyUnnamed(0) // expected-error{{'unavailableInstanceTooManyUnnamed' has been renamed to 'Int.shinyLabeledArguments(self:b:)'}} {{none}}
  unavailableInstanceNoArgsTooMany() // expected-error{{'unavailableInstanceNoArgsTooMany()' has been renamed to 'Int.shinyLabeledArguments(self:)'}} {{none}}
}
