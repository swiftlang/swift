// RUN: %target-typecheck-verify-swift -module-name Test

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

@available(badPlatform, unavailable) // expected-warning {{unrecognized platform name 'badPlatform'}}
func unavailable_bad_platform() {}

@available(macos, unavailable) // expected-warning {{unrecognized platform name 'macos'; did you mean 'macOS'?}} {{12-17=macOS}}
func incorrect_platform_case() {}

@available(mscos, unavailable) // expected-warning {{unrecognized platform name 'mscos'; did you mean 'macOS'?}} {{12-17=macOS}}
func incorrect_platform_similar1() {}

@available(macoss, unavailable) // expected-warning {{unrecognized platform name 'macoss'; did you mean 'macOS'?}} {{12-18=macOS}}
func incorrect_platform_similar2() {}

@available(mac, unavailable) // expected-warning {{unrecognized platform name 'mac'; did you mean 'macOS'?}} {{12-15=macOS}}
func incorrect_platform_similar3() {}

@available(notValid, unavailable) // expected-warning {{unrecognized platform name 'notValid'}} {{none}}
func incorrect_platform_not_similar() {}

// Handle unknown platform.
@available(HAL9000, unavailable) // expected-warning {{unrecognized platform name 'HAL9000'}}
func availabilityUnknownPlatform() {}

// <rdar://problem/17669805> Availability can't appear on a typealias
@available(*, unavailable, message: "oh no you don't")
typealias int = Int // expected-note {{'int' has been explicitly marked unavailable here}}

@available(*, unavailable, renamed: "Float")
typealias float = Float // expected-note {{'float' has been explicitly marked unavailable here}}

protocol MyNewerProtocol {}

@available(*, unavailable, renamed: "MyNewerProtocol")
protocol MyOlderProtocol {} // expected-note {{'MyOlderProtocol' has been explicitly marked unavailable here}}

extension Int: MyOlderProtocol {} // expected-error {{'MyOlderProtocol' has been renamed to 'MyNewerProtocol'}} 

struct MyCollection<Element> {
  @available(*, unavailable, renamed: "Element")
  typealias T = Element // expected-note 2{{'T' has been explicitly marked unavailable here}}

  func foo(x: T) { } // expected-error {{'T' has been renamed to 'Element'}} {{15-16=Element}}
}

extension MyCollection {
  func append(element: T) { } // expected-error {{'T' has been renamed to 'Element'}} {{24-25=Element}}
}

@available(*, unavailable, renamed: "MyCollection")
typealias YourCollection<Element> = MyCollection<Element> // expected-note {{'YourCollection' has been explicitly marked unavailable here}}

var x : YourCollection<Int> // expected-error {{'YourCollection' has been renamed to 'MyCollection'}}{{9-23=MyCollection}}

var y : int // expected-error {{'int' is unavailable: oh no you don't}}
var z : float // expected-error {{'float' has been renamed to 'Float'}}{{9-14=Float}}

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

@available(OSX, introduced: 0) // expected-warning{{expected version number in 'available' attribute; this is an error in the Swift 6 language mode}}
let _: Int

@available(OSX, introduced: 0.0) // expected-warning{{expected version number in 'available' attribute; this is an error in the Swift 6 language mode}}
let _: Int

@available(OSX, introduced: 0.0.0) // expected-warning{{expected version number in 'available' attribute; this is an error in the Swift 6 language mode}}
let _: Int

@available(OSX, introduced: 2147483646)
let _: Int

@available(OSX, introduced: 2147483647)
let _: Int

@available(*, renamed: "bad name") // expected-error{{'renamed' argument of 'available' attribute must be an operator, identifier, or full function name, optionally prefixed by a type name}}
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

@available(*, deprecated, unavailable, message: "message") // expected-error{{'available' attribute cannot be both 'unavailable' and 'deprecated'}}
struct BadUnconditionalAvailability { };

@available(*, unavailable, message="oh no you don't") // expected-error {{'=' has been replaced with ':' in attribute arguments}} {{35-36=: }}
typealias EqualFixIt1 = Int
@available(*, unavailable, message = "oh no you don't") // expected-error {{'=' has been replaced with ':' in attribute arguments}} {{36-37=:}}
typealias EqualFixIt2 = Int


// Encoding in messages
@available(*, deprecated, message: "Say \"Hi\"")
func deprecated_func_with_message() {}

// 'PANDA FACE' (U+1F43C)
@available(*, deprecated, message: "Pandas \u{1F43C} are cute")
struct DeprecatedTypeWithMessage { }

func use_deprecated_with_message() {
  deprecated_func_with_message() // expected-warning{{'deprecated_func_with_message()' is deprecated: Say \"Hi\"}}{{documentation-file=deprecated-declaration}}
  var _: DeprecatedTypeWithMessage // expected-warning{{'DeprecatedTypeWithMessage' is deprecated: Pandas \u{1F43C} are cute}}{{documentation-file=deprecated-declaration}}
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
  deprecated_func_with_renamed() // expected-warning{{'deprecated_func_with_renamed()' is deprecated: renamed to 'blarg'}}{{documentation-file=deprecated-declaration}}
  // expected-note@-1{{use 'blarg'}}{{3-31=blarg}}

  Test.deprecated_func_with_renamed() // expected-warning{{'deprecated_func_with_renamed()' is deprecated: renamed to 'blarg'}}{{documentation-file=deprecated-declaration}}
  // expected-note@-1{{use 'blarg' instead}}

  deprecated_func_with_message_renamed() //expected-warning{{'deprecated_func_with_message_renamed()' is deprecated: blarg is your friend}}{{documentation-file=deprecated-declaration}}
  // expected-note@-1{{use 'blarg'}}{{3-39=blarg}}

  var _: DeprecatedTypeWithRename // expected-warning{{'DeprecatedTypeWithRename' is deprecated: renamed to 'wobble'}}{{documentation-file=deprecated-declaration}}
  // expected-note@-1{{use 'wobble'}}{{10-34=wobble}}
}

// Short form of @available()

@available(iOS 8.0, *)
func functionWithShortFormIOSAvailable() {}

@available(iOS 8, *)
func functionWithShortFormIOSVersionNoPointAvailable() {}

@available(iOS 8.0, OSX 10.10.3, *)
func functionWithShortFormIOSOSXAvailable() {}

@available(iOS 8.0
func shortFormMissingParen() { // expected-error {{expected ')' in 'available' attribute}}
}

@available(iOS 8.0, // expected-error {{expected platform name}}
func shortFormMissingPlatform() {
}

@available(iOS 8.0, iDishwasherOS 22.0, *) // expected-warning {{unrecognized platform name 'iDishwasherOS'}}
func shortFormWithUnrecognizedPlatform() {
}

@available(iOS 8.0, iDishwasherOS 22.0, iRefrigeratorOS 18.0, *)
// expected-warning@-1 {{unrecognized platform name 'iDishwasherOS'}}
// expected-warning@-2 {{unrecognized platform name 'iRefrigeratorOS'}}
func shortFormWithTwoUnrecognizedPlatforms() {
}

@available(macOS 10.9, iOS 7.0, watchOS 2.0, tvOS 9.0, iDishwasherOS 22.0, visionOS 1.0, *)
// expected-warning@-1 {{unrecognized platform name 'iDishwasherOS'}}
func shortFormWithInteriorUnrecognizedPlatform() {
}

@available(ios 8.0, macos 10.12, *)
// expected-warning@-1 {{unrecognized platform name 'ios'; did you mean 'iOS'?}}
// expected-warning@-2 {{unrecognized platform name 'macos'; did you mean 'macOS'?}}
func shortFormWithTwoPlatformsIncorrectCase() {
}

@available(os 8.0, *)
// expected-warning@-1 {{unrecognized platform name 'os'; did you mean 'iOS'?}} {{12-14=iOS}}
func iosIsClosestThanMacOS() {}

// Make sure that even after the parser hits an unrecognized
// platform it validates the availability.
@available(iOS 8.0, iDishwasherOS 22.0, iOS 9.0, *)
// expected-warning@-1 {{unrecognized platform name 'iDishwasherOS'}}
// expected-error@-2 {{version for iOS already specified}}
func shortFormWithUnrecognizedPlatformContinueValidating() {
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

@availability(OSX, introduced: 10.10) // expected-error {{'@availability' has been renamed to '@available'}} {{2-14=available}}
func someFuncUsingOldAttribute() { }


// <rdar://problem/23853709> Compiler crash on call to unavailable "print"
@available(*, unavailable, message: "Please use the 'to' label for the target stream: 'print((...), to: &...)'")		
func print<T>(_: T, _: inout TextOutputStream) {} // expected-note {{}}
func TextOutputStreamTest(message: String, to: inout TextOutputStream) {
  print(message, &to)  // expected-error {{'print' is unavailable: Please use the 'to' label for the target stream: 'print((...), to: &...)'}}
}


struct DummyType {}

@available(*, unavailable, renamed: "&+")
func +(x: DummyType, y: DummyType) {} // expected-note {{here}}
@available(*, deprecated, renamed: "&-")
func -(x: DummyType, y: DummyType) {}

func testOperators(x: DummyType, y: DummyType) {
  x + y // expected-error {{'+' has been renamed to '&+'}} {{5-6=&+}}
  x - y // expected-warning {{'-' is deprecated: renamed to '&-'}}{{documentation-file=deprecated-declaration}} expected-note {{use '&-' instead}} {{5-6=&-}}
}

@available(*, unavailable, renamed: "DummyType.foo")
func unavailableMember() {} // expected-note {{here}}
@available(*, deprecated, renamed: "DummyType.bar")
func deprecatedMember() {}
@available(*, unavailable, renamed: "DummyType.Inner.foo")
func unavailableNestedMember() {} // expected-note {{here}}

@available(*, unavailable, renamed: "DummyType.Foo")
struct UnavailableType {} // expected-note {{here}}
@available(*, deprecated, renamed: "DummyType.Bar")
typealias DeprecatedType = Int

func testGlobalToMembers() {
  unavailableMember() // expected-error {{'unavailableMember()' has been renamed to 'DummyType.foo'}} {{3-20=DummyType.foo}}
  deprecatedMember() // expected-warning {{'deprecatedMember()' is deprecated: renamed to 'DummyType.bar'}}{{documentation-file=deprecated-declaration}} expected-note {{use 'DummyType.bar' instead}} {{3-19=DummyType.bar}}
  unavailableNestedMember() // expected-error {{'unavailableNestedMember()' has been renamed to 'DummyType.Inner.foo'}} {{3-26=DummyType.Inner.foo}}
  let x: UnavailableType? = nil // expected-error {{'UnavailableType' has been renamed to 'DummyType.Foo'}} {{10-25=DummyType.Foo}}
  _ = x
  let y: DeprecatedType? = nil // expected-warning {{'DeprecatedType' is deprecated: renamed to 'DummyType.Bar'}}{{documentation-file=deprecated-declaration}} expected-note {{use 'DummyType.Bar' instead}} {{10-24=DummyType.Bar}}
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

@available(*, unavailable, renamed: "Int.init(other:)")
func unavailableInit(a: Int) {} // expected-note 2 {{here}}
@available(*, unavailable, renamed: "Foo.Bar.init(other:)")
func unavailableNestedInit(a: Int) {} // expected-note 2 {{here}}


func testArgNames() {
  unavailableArgNames(a: 0) // expected-error {{'unavailableArgNames(a:)' has been renamed to 'shinyLabeledArguments(example:)'}} {{3-22=shinyLabeledArguments}} {{23-24=example}}
  deprecatedArgNames(b: 1) // expected-warning {{'deprecatedArgNames(b:)' is deprecated: renamed to 'moreShinyLabeledArguments(example:)'}}{{documentation-file=deprecated-declaration}} expected-note {{use 'moreShinyLabeledArguments(example:)' instead}} {{3-21=moreShinyLabeledArguments}} {{22-23=example}}

  unavailableMemberArgNames(a: 0) // expected-error {{'unavailableMemberArgNames(a:)' has been replaced by 'DummyType.shinyLabeledArguments(example:)'}} {{3-28=DummyType.shinyLabeledArguments}} {{29-30=example}}
  deprecatedMemberArgNames(b: 1) // expected-warning {{'deprecatedMemberArgNames(b:)' is deprecated: replaced by 'DummyType.moreShinyLabeledArguments(example:)'}}{{documentation-file=deprecated-declaration}} expected-note {{use 'DummyType.moreShinyLabeledArguments(example:)' instead}} {{3-27=DummyType.moreShinyLabeledArguments}} {{28-29=example}}

  unavailableMemberArgNamesMsg(a: 0) // expected-error {{'unavailableMemberArgNamesMsg(a:)' has been replaced by 'DummyType.shinyLabeledArguments(example:)': ha}} {{3-31=DummyType.shinyLabeledArguments}} {{32-33=example}}
  deprecatedMemberArgNamesMsg(b: 1) // expected-warning {{'deprecatedMemberArgNamesMsg(b:)' is deprecated: ha}}{{documentation-file=deprecated-declaration}} expected-note {{use 'DummyType.moreShinyLabeledArguments(example:)' instead}} {{3-30=DummyType.moreShinyLabeledArguments}} {{31-32=example}}

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

  unavailableInit(a: 0) // expected-error {{'unavailableInit(a:)' has been replaced by 'Int.init(other:)'}} {{3-18=Int}} {{19-20=other}}
  let fn = unavailableInit // expected-error {{'unavailableInit(a:)' has been replaced by 'Int.init(other:)'}} {{12-27=Int.init}}
  fn(1)

  unavailableNestedInit(a: 0) // expected-error {{'unavailableNestedInit(a:)' has been replaced by 'Foo.Bar.init(other:)'}} {{3-24=Foo.Bar}} {{25-26=other}}
  let fn2 = unavailableNestedInit // expected-error {{'unavailableNestedInit(a:)' has been replaced by 'Foo.Bar.init(other:)'}} {{13-34=Foo.Bar.init}}
  fn2(1)
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

@available(*, unavailable, renamed: "Int.foo(self:)", message: "blah")
func unavailableInstanceMessage(a: Int) {} // expected-note {{here}}
@available(*, deprecated, renamed: "Int.foo(self:)")
func deprecatedInstance(a: Int) {}
@available(*, deprecated, renamed: "Int.foo(self:)", message: "blah")
func deprecatedInstanceMessage(a: Int) {}

@available(*, unavailable, renamed: "Foo.Bar.foo(self:)")
func unavailableNestedInstance(a: Int) {} // expected-note {{here}}

func testRenameInstance() {
  unavailableInstance(a: 0) // expected-error{{'unavailableInstance(a:)' has been replaced by instance method 'Int.foo()'}} {{3-22=0.foo}} {{23-27=}}
  unavailableInstanceUnlabeled(0) // expected-error{{'unavailableInstanceUnlabeled' has been replaced by instance method 'Int.foo()'}} {{3-31=0.foo}} {{32-33=}}
  unavailableInstanceFirst(a: 0, b: 1) // expected-error{{'unavailableInstanceFirst(a:b:)' has been replaced by instance method 'Int.foo(other:)'}} {{3-27=0.foo}} {{28-34=}} {{34-35=other}}
  unavailableInstanceSecond(a: 0, b: 1) // expected-error{{'unavailableInstanceSecond(a:b:)' has been replaced by instance method 'Int.foo(other:)'}} {{3-28=1.foo}} {{29-30=other}} {{33-39=}}
  unavailableInstanceSecondOfThree(a: 0, b: 1, c: 2) // expected-error{{'unavailableInstanceSecondOfThree(a:b:c:)' has been replaced by instance method 'Int.foo(_:c:)'}} {{3-35=1.foo}} {{36-39=}} {{42-48=}}

  unavailableInstance(a: 0 + 0) // expected-error{{'unavailableInstance(a:)' has been replaced by instance method 'Int.foo()'}} {{3-22=(0 + 0).foo}} {{23-31=}}

  unavailableInstanceMessage(a: 0) // expected-error{{'unavailableInstanceMessage(a:)' has been replaced by instance method 'Int.foo()': blah}} {{3-29=0.foo}} {{30-34=}}
  deprecatedInstance(a: 0) // expected-warning{{'deprecatedInstance(a:)' is deprecated: replaced by instance method 'Int.foo()'}}{{documentation-file=deprecated-declaration}} expected-note{{use 'Int.foo()' instead}} {{3-21=0.foo}} {{22-26=}}
  deprecatedInstanceMessage(a: 0) // expected-warning{{'deprecatedInstanceMessage(a:)' is deprecated: blah}}{{documentation-file=deprecated-declaration}} expected-note{{use 'Int.foo()' instead}} {{3-28=0.foo}} {{29-33=}}

  unavailableNestedInstance(a: 0) // expected-error{{'unavailableNestedInstance(a:)' has been replaced by instance method 'Foo.Bar.foo()'}} {{3-28=0.foo}} {{29-33=}}
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
  unavailableInstanceTooFew(a: 0, b: 1) // expected-error{{'unavailableInstanceTooFew(a:b:)' has been replaced by instance method 'Int.shinyLabeledArguments()'}} {{none}}
  unavailableInstanceTooFewUnnamed(0, 1) // expected-error{{'unavailableInstanceTooFewUnnamed' has been replaced by instance method 'Int.shinyLabeledArguments()'}} {{none}}
  unavailableInstanceTooMany(a: 0) // expected-error{{'unavailableInstanceTooMany(a:)' has been replaced by instance method 'Int.shinyLabeledArguments(b:)'}} {{none}}
  unavailableInstanceTooManyUnnamed(0) // expected-error{{'unavailableInstanceTooManyUnnamed' has been replaced by instance method 'Int.shinyLabeledArguments(b:)'}} {{none}}
  unavailableInstanceNoArgsTooMany() // expected-error{{'unavailableInstanceNoArgsTooMany()' has been replaced by instance method 'Int.shinyLabeledArguments()'}} {{none}}
}

@available(*, unavailable, renamed: "getter:Int.prop(self:)")
func unavailableInstanceProperty(a: Int) {} // expected-note 2 {{here}}
@available(*, unavailable, renamed: "getter:Int.prop(self:)")
func unavailableInstancePropertyUnlabeled(_ a: Int) {} // expected-note 2 {{here}}
@available(*, unavailable, renamed: "getter:Int.prop()")
func unavailableClassProperty() {} // expected-note {{here}}
@available(*, unavailable, renamed: "getter:global()")
func unavailableGlobalProperty() {} // expected-note {{here}}

@available(*, unavailable, renamed: "getter:Int.prop(self:)", message: "blah")
func unavailableInstancePropertyMessage(a: Int) {} // expected-note {{here}}
@available(*, unavailable, renamed: "getter:Int.prop()", message: "blah")
func unavailableClassPropertyMessage() {} // expected-note {{here}}
@available(*, unavailable, renamed: "getter:global()", message: "blah")
func unavailableGlobalPropertyMessage() {} // expected-note {{here}}

@available(*, deprecated, renamed: "getter:Int.prop(self:)")
func deprecatedInstanceProperty(a: Int) {}
@available(*, deprecated, renamed: "getter:Int.prop()")
func deprecatedClassProperty() {}
@available(*, deprecated, renamed: "getter:global()")
func deprecatedGlobalProperty() {}

@available(*, deprecated, renamed: "getter:Int.prop(self:)", message: "blah")
func deprecatedInstancePropertyMessage(a: Int) {}
@available(*, deprecated, renamed: "getter:Int.prop()", message: "blah")
func deprecatedClassPropertyMessage() {}
@available(*, deprecated, renamed: "getter:global()", message: "blah")
func deprecatedGlobalPropertyMessage() {}

func testRenameGetters() {
  unavailableInstanceProperty(a: 1) // expected-error{{'unavailableInstanceProperty(a:)' has been replaced by property 'Int.prop'}} {{3-30=1.prop}} {{30-36=}}
  unavailableInstancePropertyUnlabeled(1) // expected-error{{'unavailableInstancePropertyUnlabeled' has been replaced by property 'Int.prop'}} {{3-39=1.prop}} {{39-42=}}
  unavailableInstanceProperty(a: 1 + 1) // expected-error{{'unavailableInstanceProperty(a:)' has been replaced by property 'Int.prop'}} {{3-30=(1 + 1).prop}} {{30-40=}}
  unavailableInstancePropertyUnlabeled(1 + 1) // expected-error{{'unavailableInstancePropertyUnlabeled' has been replaced by property 'Int.prop'}} {{3-39=(1 + 1).prop}} {{39-46=}}
  unavailableClassProperty() // expected-error{{'unavailableClassProperty()' has been replaced by property 'Int.prop'}} {{3-27=Int.prop}} {{27-29=}}
  unavailableGlobalProperty() // expected-error{{'unavailableGlobalProperty()' has been replaced by 'global'}} {{3-28=global}} {{28-30=}}

  unavailableInstancePropertyMessage(a: 1) // expected-error{{'unavailableInstancePropertyMessage(a:)' has been replaced by property 'Int.prop': blah}} {{3-37=1.prop}} {{37-43=}}
  unavailableClassPropertyMessage() // expected-error{{'unavailableClassPropertyMessage()' has been replaced by property 'Int.prop': blah}} {{3-34=Int.prop}} {{34-36=}}
  unavailableGlobalPropertyMessage() // expected-error{{'unavailableGlobalPropertyMessage()' has been replaced by 'global': blah}} {{3-35=global}} {{35-37=}}

  deprecatedInstanceProperty(a: 1) // expected-warning {{'deprecatedInstanceProperty(a:)' is deprecated: replaced by property 'Int.prop'}}{{documentation-file=deprecated-declaration}} expected-note{{use 'Int.prop' instead}} {{3-29=1.prop}} {{29-35=}}
  deprecatedClassProperty() // expected-warning {{'deprecatedClassProperty()' is deprecated: replaced by property 'Int.prop'}}{{documentation-file=deprecated-declaration}} expected-note{{use 'Int.prop' instead}} {{3-26=Int.prop}} {{26-28=}}
  deprecatedGlobalProperty() // expected-warning {{'deprecatedGlobalProperty()' is deprecated: replaced by 'global'}}{{documentation-file=deprecated-declaration}} expected-note{{use 'global' instead}} {{3-27=global}} {{27-29=}}

  deprecatedInstancePropertyMessage(a: 1) // expected-warning {{'deprecatedInstancePropertyMessage(a:)' is deprecated: blah}}{{documentation-file=deprecated-declaration}} expected-note{{use 'Int.prop' instead}} {{3-36=1.prop}} {{36-42=}}
  deprecatedClassPropertyMessage() // expected-warning {{'deprecatedClassPropertyMessage()' is deprecated: blah}}{{documentation-file=deprecated-declaration}} expected-note{{use 'Int.prop' instead}} {{3-33=Int.prop}} {{33-35=}}
  deprecatedGlobalPropertyMessage() // expected-warning {{'deprecatedGlobalPropertyMessage()' is deprecated: blah}}{{documentation-file=deprecated-declaration}} expected-note{{use 'global' instead}} {{3-34=global}} {{34-36=}}
}

@available(*, unavailable, renamed: "setter:Int.prop(self:_:)")
func unavailableSetInstanceProperty(a: Int, b: Int) {} // expected-note 2 {{here}}
@available(*, unavailable, renamed: "setter:Int.prop(_:self:)")
func unavailableSetInstancePropertyReverse(a: Int, b: Int) {} // expected-note 2 {{here}}
@available(*, unavailable, renamed: "setter:Int.prop(self:newValue:)")
func unavailableSetInstancePropertyUnlabeled(_ a: Int, _ b: Int) {} // expected-note 2 {{here}}
@available(*, unavailable, renamed: "setter:Int.prop(newValue:self:)")
func unavailableSetInstancePropertyUnlabeledReverse(_ a: Int, _ b: Int) {} // expected-note 2 {{here}}
@available(*, unavailable, renamed: "setter:Int.prop(x:)")
func unavailableSetClassProperty(a: Int) {} // expected-note {{here}}
@available(*, unavailable, renamed: "setter:global(_:)")
func unavailableSetGlobalProperty(_ a: Int) {} // expected-note {{here}}

@available(*, unavailable, renamed: "setter:Int.prop(self:_:)")
func unavailableSetInstancePropertyInout(a: inout Int, b: Int) {} // expected-note {{here}}

func testRenameSetters() {
  unavailableSetInstanceProperty(a: 1, b: 2) // expected-error{{'unavailableSetInstanceProperty(a:b:)' has been replaced by property 'Int.prop'}} {{3-33=1.prop}} {{33-43= = }} {{44-45=}}
  unavailableSetInstancePropertyUnlabeled(1, 2) // expected-error{{'unavailableSetInstancePropertyUnlabeled' has been replaced by property 'Int.prop'}} {{3-42=1.prop}} {{42-46= = }} {{47-48=}}
  unavailableSetInstancePropertyReverse(a: 1, b: 2) // expected-error{{'unavailableSetInstancePropertyReverse(a:b:)' has been replaced by property 'Int.prop'}} {{3-40=2.prop}} {{40-44= = }} {{45-52=}}
  unavailableSetInstancePropertyUnlabeledReverse(1, 2) // expected-error{{'unavailableSetInstancePropertyUnlabeledReverse' has been replaced by property 'Int.prop'}} {{3-49=2.prop}} {{49-50= = }} {{51-55=}}
  unavailableSetInstanceProperty(a: 1 + 1, b: 2 + 2) // expected-error{{'unavailableSetInstanceProperty(a:b:)' has been replaced by property 'Int.prop'}} {{3-33=(1 + 1).prop}} {{33-47= = }} {{52-53=}}
  unavailableSetInstancePropertyUnlabeled(1 + 1, 2 + 2) // expected-error{{'unavailableSetInstancePropertyUnlabeled' has been replaced by property 'Int.prop'}} {{3-42=(1 + 1).prop}} {{42-50= = }} {{55-56=}}
  unavailableSetInstancePropertyReverse(a: 1 + 1, b: 2 + 2) // expected-error{{'unavailableSetInstancePropertyReverse(a:b:)' has been replaced by property 'Int.prop'}} {{3-40=(2 + 2).prop}} {{40-44= = }} {{49-60=}}
  unavailableSetInstancePropertyUnlabeledReverse(1 + 1, 2 + 2) // expected-error{{'unavailableSetInstancePropertyUnlabeledReverse' has been replaced by property 'Int.prop'}} {{3-49=(2 + 2).prop}} {{49-50= = }} {{55-63=}}
  unavailableSetClassProperty(a: 1) // expected-error{{'unavailableSetClassProperty(a:)' has been replaced by property 'Int.prop'}} {{3-30=Int.prop}} {{30-34= = }} {{35-36=}}
  unavailableSetGlobalProperty(1) // expected-error{{'unavailableSetGlobalProperty' has been replaced by 'global'}} {{3-31=global}} {{31-32= = }} {{33-34=}}

  var x = 0
  unavailableSetInstancePropertyInout(a: &x, b: 2) // expected-error{{'unavailableSetInstancePropertyInout(a:b:)' has been replaced by property 'Int.prop'}} {{3-38=x.prop}} {{38-49= = }} {{50-51=}}
}

@available(*, unavailable, renamed: "Int.foo(self:execute:)")
func trailingClosure(_ value: Int, fn: () -> Void) {} // expected-note {{here}}
@available(*, unavailable, renamed: "Int.foo(self:bar:execute:)")
func trailingClosureArg(_ value: Int, _ other: Int, fn: () -> Void) {} // expected-note {{here}}
@available(*, unavailable, renamed: "Int.foo(bar:self:execute:)")
func trailingClosureArg2(_ value: Int, _ other: Int, fn: () -> Void) {} // expected-note {{here}}

func testInstanceTrailingClosure() {
  // FIXME: regression in fixit due to noescape-by-default
  trailingClosure(0) {} // expected-error {{'trailingClosure(_:fn:)' has been replaced by instance method 'Int.foo(execute:)'}} // FIXME: {{3-18=0.foo}} {{19-20=}}
  trailingClosureArg(0, 1) {} // expected-error {{'trailingClosureArg(_:_:fn:)' has been replaced by instance method 'Int.foo(bar:execute:)'}} // FIXME: {{3-21=0.foo}} {{22-25=}} {{25-25=bar: }}
  trailingClosureArg2(0, 1) {} // expected-error {{'trailingClosureArg2(_:_:fn:)' has been replaced by instance method 'Int.foo(bar:execute:)'}} // FIXME: {{3-22=1.foo}} {{23-23=bar: }} {{24-27=}}
}

@available(*, unavailable, renamed: "+")
func add(_ value: Int, _ other: Int) {} // expected-note {{here}}

infix operator ***
@available(*, unavailable, renamed: "add")
func ***(value: (), other: ()) {} // expected-note {{here}}
@available(*, unavailable, renamed: "Int.foo(self:_:)")
func ***(value: Int, other: Int) {} // expected-note {{here}}

prefix operator ***
@available(*, unavailable, renamed: "add")
prefix func ***(value: Int?) {} // expected-note {{here}}
@available(*, unavailable, renamed: "Int.foo(self:)")
prefix func ***(value: Int) {} // expected-note {{here}}

postfix operator ***
@available(*, unavailable, renamed: "add")
postfix func ***(value: Int?) {} // expected-note {{here}}
@available(*, unavailable, renamed: "Int.foo(self:)")
postfix func ***(value: Int) {} // expected-note {{here}}

func testOperators() {
  add(0, 1) // expected-error {{'add' has been renamed to '+'}} {{none}}
  () *** () // expected-error {{'***' has been renamed to 'add'}} {{none}}
  0 *** 1 // expected-error {{'***' has been replaced by instance method 'Int.foo(_:)'}} {{none}}

  ***nil // expected-error {{'***' has been renamed to 'add'}} {{none}}
  ***0 // expected-error {{'***' has been replaced by instance method 'Int.foo()'}} {{none}}
  
  nil*** // expected-error {{'***' has been renamed to 'add'}} {{none}}
  0*** // expected-error {{'***' has been replaced by instance method 'Int.foo()'}} {{none}}
}

extension Int {
  @available(*, unavailable, renamed: "init(other:)")
  @discardableResult
  static func factory(other: Int) -> Int { return other } // expected-note 2 {{here}}

  @available(*, unavailable, renamed: "Int.init(other:)")
  @discardableResult
  static func factory2(other: Int) -> Int { return other } // expected-note 2 {{here}}

  static func testFactoryMethods() {
    factory(other: 1) // expected-error {{'factory(other:)' has been replaced by 'init(other:)'}} {{none}}
    factory2(other: 1) // expected-error {{'factory2(other:)' has been replaced by 'Int.init(other:)'}} {{5-13=Int}}
  }
}

func testFactoryMethods() {
  Int.factory(other: 1) // expected-error {{'factory(other:)' has been replaced by 'init(other:)'}} {{6-14=}}
  Int.factory2(other: 1) // expected-error {{'factory2(other:)' has been replaced by 'Int.init(other:)'}} {{3-15=Int}}
}

class DeprecatedInitBase {
  @available(*, deprecated, renamed: "init(new:)")
  init(old: Int) {}

  init(new: Int) {}

  convenience init(testSelf: Int) {
    // https://github.com/apple/swift/issues/57354
    // The fix-it should not remove `.init`
    self.init(old: testSelf) // expected-warning {{'init(old:)' is deprecated: replaced by 'init(new:)'}}{{documentation-file=deprecated-declaration}} expected-note {{use 'init(new:)' instead}} {{15-18=new}}
  }

  init(testSuper: Int) {}

  @available(*, deprecated, renamed: "init(new:)")
  @available(*, deprecated, renamed: "init(new:)")
  init(multipleEqualAvailabilityAttributes: Int) {}

  @available(*, deprecated, renamed: "init(old:)")
  @available(*, deprecated, renamed: "init(testSuper:)")
  @available(*, deprecated, renamed: "init(new:)")
  init(multipleUnequalAvailabilityAttributes: Int) {}
}

class DeprecatedInitSub1: DeprecatedInitBase {
  override init(testSuper: Int) {
    // https://github.com/apple/swift/issues/57354
    // The fix-it should not remove `.init`
    super.init(old: testSuper) // expected-warning {{'init(old:)' is deprecated: replaced by 'init(new:)'}}{{documentation-file=deprecated-declaration}} expected-note {{use 'init(new:)' instead}} {{16-19=new}}
  }
}

class DeprecatedInitSub2: DeprecatedInitBase { }

_ = DeprecatedInitBase(old: 0) // expected-warning {{'init(old:)' is deprecated: replaced by 'init(new:)'}}{{documentation-file=deprecated-declaration}} expected-note {{use 'init(new:)' instead}} {{24-27=new}}
_ = DeprecatedInitBase.init(old: 0) // expected-warning {{'init(old:)' is deprecated: replaced by 'init(new:)'}}{{documentation-file=deprecated-declaration}} expected-note {{use 'init(new:)' instead}} {{29-32=new}}
let _: DeprecatedInitBase = .init(old: 0) // expected-warning {{'init(old:)' is deprecated: replaced by 'init(new:)'}}{{documentation-file=deprecated-declaration}} expected-note {{use 'init(new:)' instead}} {{35-38=new}}
_ = DeprecatedInitSub2(old: 0) // expected-warning {{'init(old:)' is deprecated: replaced by 'init(new:)'}}{{documentation-file=deprecated-declaration}} expected-note {{use 'init(new:)' instead}} {{24-27=new}}
_ = DeprecatedInitSub2.init(old: 0) // expected-warning {{'init(old:)' is deprecated: replaced by 'init(new:)'}}{{documentation-file=deprecated-declaration}} expected-note {{use 'init(new:)' instead}} {{29-32=new}}
let _: DeprecatedInitSub2 = .init(old: 0) // expected-warning {{'init(old:)' is deprecated: replaced by 'init(new:)'}}{{documentation-file=deprecated-declaration}} expected-note {{use 'init(new:)' instead}} {{35-38=new}}

_ = DeprecatedInitBase(multipleEqualAvailabilityAttributes: 0) // expected-warning {{'init(multipleEqualAvailabilityAttributes:)' is deprecated: replaced by 'init(new:)'}}{{documentation-file=deprecated-declaration}} expected-note {{use 'init(new:)' instead}} {{24-59=new}}
_ = DeprecatedInitBase.init(multipleEqualAvailabilityAttributes: 0) // expected-warning {{'init(multipleEqualAvailabilityAttributes:)' is deprecated: replaced by 'init(new:)'}}{{documentation-file=deprecated-declaration}} expected-note {{use 'init(new:)' instead}} {{29-64=new}}
let _: DeprecatedInitBase = .init(multipleEqualAvailabilityAttributes: 0) // expected-warning {{'init(multipleEqualAvailabilityAttributes:)' is deprecated: replaced by 'init(new:)'}}{{documentation-file=deprecated-declaration}} expected-note {{use 'init(new:)' instead}} {{35-70=new}}
_ = DeprecatedInitSub2(multipleEqualAvailabilityAttributes: 0) // expected-warning {{'init(multipleEqualAvailabilityAttributes:)' is deprecated: replaced by 'init(new:)'}}{{documentation-file=deprecated-declaration}} expected-note {{use 'init(new:)' instead}} {{24-59=new}}
_ = DeprecatedInitSub2.init(multipleEqualAvailabilityAttributes: 0) // expected-warning {{'init(multipleEqualAvailabilityAttributes:)' is deprecated: replaced by 'init(new:)'}}{{documentation-file=deprecated-declaration}} expected-note {{use 'init(new:)' instead}} {{29-64=new}}
let _: DeprecatedInitSub2 = .init(multipleEqualAvailabilityAttributes: 0) // expected-warning {{'init(multipleEqualAvailabilityAttributes:)' is deprecated: replaced by 'init(new:)'}}{{documentation-file=deprecated-declaration}} expected-note {{use 'init(new:)' instead}} {{35-70=new}}

_ = DeprecatedInitBase(multipleUnequalAvailabilityAttributes: 0) // expected-warning {{'init(multipleUnequalAvailabilityAttributes:)' is deprecated: replaced by 'init(new:)'}}{{documentation-file=deprecated-declaration}} expected-note {{use 'init(new:)' instead}} {{24-61=new}}
_ = DeprecatedInitBase.init(multipleUnequalAvailabilityAttributes: 0) // expected-warning {{'init(multipleUnequalAvailabilityAttributes:)' is deprecated: replaced by 'init(new:)'}}{{documentation-file=deprecated-declaration}} expected-note {{use 'init(new:)' instead}} {{29-66=new}}
let _: DeprecatedInitBase = .init(multipleUnequalAvailabilityAttributes: 0) // expected-warning {{'init(multipleUnequalAvailabilityAttributes:)' is deprecated: replaced by 'init(new:)'}}{{documentation-file=deprecated-declaration}} expected-note {{use 'init(new:)' instead}} {{35-72=new}}
_ = DeprecatedInitSub2(multipleUnequalAvailabilityAttributes: 0) // expected-warning {{'init(multipleUnequalAvailabilityAttributes:)' is deprecated: replaced by 'init(new:)'}}{{documentation-file=deprecated-declaration}} expected-note {{use 'init(new:)' instead}} {{24-61=new}}
_ = DeprecatedInitSub2.init(multipleUnequalAvailabilityAttributes: 0) // expected-warning {{'init(multipleUnequalAvailabilityAttributes:)' is deprecated: replaced by 'init(new:)'}}{{documentation-file=deprecated-declaration}} expected-note {{use 'init(new:)' instead}} {{29-66=new}}
let _: DeprecatedInitSub2 = .init(multipleUnequalAvailabilityAttributes: 0) // expected-warning {{'init(multipleUnequalAvailabilityAttributes:)' is deprecated: replaced by 'init(new:)'}}{{documentation-file=deprecated-declaration}} expected-note {{use 'init(new:)' instead}} {{35-72=new}}


class Base {
  @available(*, unavailable)
  func bad() {} // expected-note {{here}}
  @available(*, unavailable, message: "it was smelly")
  func smelly() {} // expected-note {{here}}
  @available(*, unavailable, renamed: "new")
  func old() {} // expected-note {{here}}
  @available(*, unavailable, renamed: "new", message: "it was smelly")
  func oldAndSmelly() {} // expected-note {{here}}
  @available(*, unavailable)
  func expendable() {}

  @available(*, unavailable)
  var badProp: Int { return 0 } // expected-note {{here}}
  @available(*, unavailable, message: "it was smelly")
  var smellyProp: Int { return 0 } // expected-note {{here}}
  @available(*, unavailable, renamed: "new")
  var oldProp: Int { return 0 } // expected-note {{here}}
  @available(*, unavailable, renamed: "new", message: "it was smelly")
  var oldAndSmellyProp: Int { return 0 } // expected-note {{here}}
  @available(*, unavailable)
  var expendableProp: Int { return 0 }

  @available(*, unavailable, renamed: "init")
  func nowAnInitializer() {} // expected-note {{here}}
  @available(*, unavailable, renamed: "init()")
  func nowAnInitializer2() {} // expected-note {{here}}

  @available(*, unavailable, renamed: "foo")
  init(nowAFunction: Int) {} // expected-note {{here}}
  @available(*, unavailable, renamed: "foo(_:)")
  init(nowAFunction2: Int) {} // expected-note {{here}}

  @available(*, unavailable, renamed: "shinyLabeledArguments(example:)")
  func unavailableArgNames(a: Int) {} // expected-note {{here}}
  @available(*, unavailable, renamed: "shinyLabeledArguments(example:)")
  func unavailableArgRenamed(a: Int) {} // expected-note {{here}}
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
  @available(*, unavailable, renamed: "shinyLabeledArguments(a:b:)")
  func unavailableMultiSame(a: Int, b: Int) {} // expected-note {{here}}
  @available(*, unavailable, renamed: "shinyLabeledArguments(example:another:)")
  func unavailableMultiUnnamed(_ a: Int, _ b: Int) {} // expected-note {{here}}
  @available(*, unavailable, renamed: "shinyLabeledArguments(_:_:)")
  func unavailableMultiUnnamedSame(_ a: Int, _ b: Int) {} // expected-note {{here}}
  @available(*, unavailable, renamed: "shinyLabeledArguments(_:_:)")
  func unavailableMultiNewlyUnnamed(a: Int, b: Int) {} // expected-note {{here}}

  @available(*, unavailable, renamed: "init(shinyNewName:)")
  init(unavailableArgNames: Int) {} // expected-note{{here}}
  @available(*, unavailable, renamed: "init(a:)")
  init(_ unavailableUnnamed: Int) {} // expected-note{{here}}
  @available(*, unavailable, renamed: "init(_:)")
  init(unavailableNewlyUnnamed: Int) {} // expected-note{{here}}
  @available(*, unavailable, renamed: "init(a:b:)")
  init(_ unavailableMultiUnnamed: Int, _ b: Int) {} // expected-note{{here}}
  @available(*, unavailable, renamed: "init(_:_:)")
  init(unavailableMultiNewlyUnnamed a: Int, b: Int) {} // expected-note{{here}}

  @available(*, unavailable, renamed: "shinyLabeledArguments(x:)")
  func unavailableTooFew(a: Int, b: Int) {} // expected-note {{here}}
  @available(*, unavailable, renamed: "shinyLabeledArguments(x:b:)")
  func unavailableTooMany(a: Int) {} // expected-note {{here}}
  @available(*, unavailable, renamed: "shinyLabeledArguments(x:)")
  func unavailableNoArgsTooMany() {} // expected-note {{here}}

  @available(*, unavailable, renamed: "Base.shinyLabeledArguments()")
  func unavailableHasType() {} // expected-note {{here}}
}

class Sub : Base {
  override func bad() {} // expected-error {{cannot override 'bad' which has been marked unavailable}} {{none}} expected-note {{remove 'override' modifier to declare a new 'bad'}} {{3-12=}}
  override func smelly() {} // expected-error {{cannot override 'smelly' which has been marked unavailable: it was smelly}} {{none}} expected-note {{remove 'override' modifier to declare a new 'smelly'}} {{3-12=}}
  override func old() {} // expected-error {{'old()' has been renamed to 'new'}} {{17-20=new}} expected-note {{remove 'override' modifier to declare a new 'old'}} {{3-12=}}
  override func oldAndSmelly() {} // expected-error {{'oldAndSmelly()' has been renamed to 'new': it was smelly}} {{17-29=new}} expected-note {{remove 'override' modifier to declare a new 'oldAndSmelly'}} {{3-12=}}
  func expendable() {} // no-error

  override var badProp: Int { return 0 } // expected-error {{cannot override 'badProp' which has been marked unavailable}} {{none}} expected-note {{remove 'override' modifier to declare a new 'badProp'}} {{3-12=}}
  override var smellyProp: Int { return 0 } // expected-error {{cannot override 'smellyProp' which has been marked unavailable: it was smelly}} {{none}} expected-note {{remove 'override' modifier to declare a new 'smellyProp'}} {{3-12=}}
  override var oldProp: Int { return 0 } // expected-error {{'oldProp' has been renamed to 'new'}} {{16-23=new}} expected-note {{remove 'override' modifier to declare a new 'oldProp'}} {{3-12=}}
  override var oldAndSmellyProp: Int { return 0 } // expected-error {{'oldAndSmellyProp' has been renamed to 'new': it was smelly}} {{16-32=new}} expected-note {{remove 'override' modifier to declare a new 'oldAndSmellyProp'}} {{3-12=}}
  var expendableProp: Int { return 0 } // no-error

  override func nowAnInitializer() {} // expected-error {{'nowAnInitializer()' has been replaced by 'init'}} {{none}} expected-note {{remove 'override' modifier to declare a new 'nowAnInitializer'}} {{3-12=}}
  override func nowAnInitializer2() {} // expected-error {{'nowAnInitializer2()' has been replaced by 'init()'}} {{none}} expected-note {{remove 'override' modifier to declare a new 'nowAnInitializer2'}} {{3-12=}}
  override init(nowAFunction: Int) {} // expected-error {{'init(nowAFunction:)' has been renamed to 'foo'}} {{none}} expected-note {{remove 'override' modifier to declare a new 'init'}} {{3-12=}}
  override init(nowAFunction2: Int) {} // expected-error {{'init(nowAFunction2:)' has been renamed to 'foo(_:)'}} {{none}} expected-note {{remove 'override' modifier to declare a new 'init'}} {{3-12=}}

  override func unavailableArgNames(a: Int) {} // expected-error {{'unavailableArgNames(a:)' has been renamed to 'shinyLabeledArguments(example:)'}} {{17-36=shinyLabeledArguments}} {{37-37=example }} expected-note {{remove 'override' modifier to declare a new 'unavailableArgNames'}} {{3-12=}}
  override func unavailableArgRenamed(a param: Int) {} // expected-error {{'unavailableArgRenamed(a:)' has been renamed to 'shinyLabeledArguments(example:)'}} {{17-38=shinyLabeledArguments}} {{39-40=example}} expected-note {{remove 'override' modifier to declare a new 'unavailableArgRenamed'}} {{3-12=}}
  override func unavailableNoArgs() {} // expected-error {{'unavailableNoArgs()' has been renamed to 'shinyLabeledArguments()'}} {{17-34=shinyLabeledArguments}} expected-note {{remove 'override' modifier to declare a new 'unavailableNoArgs'}} {{3-12=}}
  override func unavailableSame(a: Int) {} // expected-error {{'unavailableSame(a:)' has been renamed to 'shinyLabeledArguments(a:)'}} {{17-32=shinyLabeledArguments}} expected-note {{remove 'override' modifier to declare a new 'unavailableSame'}} {{3-12=}}
  override func unavailableUnnamed(_ a: Int) {} // expected-error {{'unavailableUnnamed' has been renamed to 'shinyLabeledArguments(example:)'}} {{17-35=shinyLabeledArguments}} {{36-37=example}} expected-note {{remove 'override' modifier to declare a new 'unavailableUnnamed'}} {{3-12=}}
  override func unavailableUnnamedSame(_ a: Int) {} // expected-error {{'unavailableUnnamedSame' has been renamed to 'shinyLabeledArguments(_:)'}} {{17-39=shinyLabeledArguments}} expected-note {{remove 'override' modifier to declare a new 'unavailableUnnamedSame'}} {{3-12=}}
  override func unavailableNewlyUnnamed(a: Int) {} // expected-error {{'unavailableNewlyUnnamed(a:)' has been renamed to 'shinyLabeledArguments(_:)'}} {{17-40=shinyLabeledArguments}} {{41-41=_ }} expected-note {{remove 'override' modifier to declare a new 'unavailableNewlyUnnamed'}} {{3-12=}}
  override func unavailableMultiSame(a: Int, b: Int) {} // expected-error {{'unavailableMultiSame(a:b:)' has been renamed to 'shinyLabeledArguments(a:b:)'}} {{17-37=shinyLabeledArguments}} expected-note {{remove 'override' modifier to declare a new 'unavailableMultiSame'}} {{3-12=}}
  override func unavailableMultiUnnamed(_ a: Int, _ b: Int) {} // expected-error {{'unavailableMultiUnnamed' has been renamed to 'shinyLabeledArguments(example:another:)'}} {{17-40=shinyLabeledArguments}} {{41-42=example}} {{51-52=another}} expected-note {{remove 'override' modifier to declare a new 'unavailableMultiUnnamed'}} {{3-12=}}
  override func unavailableMultiUnnamedSame(_ a: Int, _ b: Int) {} // expected-error {{'unavailableMultiUnnamedSame' has been renamed to 'shinyLabeledArguments(_:_:)'}} {{17-44=shinyLabeledArguments}} expected-note {{remove 'override' modifier to declare a new 'unavailableMultiUnnamedSame'}} {{3-12=}}
  override func unavailableMultiNewlyUnnamed(a: Int, b: Int) {} // expected-error {{'unavailableMultiNewlyUnnamed(a:b:)' has been renamed to 'shinyLabeledArguments(_:_:)'}} {{17-45=shinyLabeledArguments}} {{46-46=_ }} {{54-54=_ }} expected-note {{remove 'override' modifier to declare a new 'unavailableMultiNewlyUnnamed'}} {{3-12=}}

  override init(unavailableArgNames: Int) {} // expected-error {{'init(unavailableArgNames:)' has been renamed to 'init(shinyNewName:)'}} {{17-17=shinyNewName }} expected-note {{remove 'override' modifier to declare a new 'init'}} {{3-12=}}
  override init(_ unavailableUnnamed: Int) {} // expected-error {{'init(_:)' has been renamed to 'init(a:)'}} {{17-18=a}} expected-note {{remove 'override' modifier to declare a new 'init'}} {{3-12=}}
  override init(unavailableNewlyUnnamed: Int) {} // expected-error {{'init(unavailableNewlyUnnamed:)' has been renamed to 'init(_:)'}} {{17-17=_ }} expected-note {{remove 'override' modifier to declare a new 'init'}} {{3-12=}}
  override init(_ unavailableMultiUnnamed: Int, _ b: Int) {} // expected-error {{'init(_:_:)' has been renamed to 'init(a:b:)'}} {{17-18=a}} {{49-51=}} expected-note {{remove 'override' modifier to declare a new 'init'}} {{3-12=}}
  override init(unavailableMultiNewlyUnnamed a: Int, b: Int) {} // expected-error {{'init(unavailableMultiNewlyUnnamed:b:)' has been renamed to 'init(_:_:)'}} {{17-45=_}} {{54-54=_ }} expected-note {{remove 'override' modifier to declare a new 'init'}} {{3-12=}}

  override func unavailableTooFew(a: Int, b: Int) {} // expected-error {{'unavailableTooFew(a:b:)' has been renamed to 'shinyLabeledArguments(x:)'}} {{none}} expected-note {{remove 'override' modifier to declare a new 'unavailableTooFew'}} {{3-12=}}
  override func unavailableTooMany(a: Int) {} // expected-error {{'unavailableTooMany(a:)' has been renamed to 'shinyLabeledArguments(x:b:)'}} {{none}} expected-note {{remove 'override' modifier to declare a new 'unavailableTooMany'}} {{3-12=}}
  override func unavailableNoArgsTooMany() {} // expected-error {{'unavailableNoArgsTooMany()' has been renamed to 'shinyLabeledArguments(x:)'}} {{none}} expected-note {{remove 'override' modifier to declare a new 'unavailableNoArgsTooMany'}} {{3-12=}}
  override func unavailableHasType() {} // expected-error {{'unavailableHasType()' has been replaced by 'Base.shinyLabeledArguments()'}} {{none}} expected-note {{remove 'override' modifier to declare a new 'unavailableHasType'}} {{3-12=}}
}

// U: Unnamed, L: Labeled
@available(*, unavailable, renamed: "after(fn:)")
func closure_U_L(_ x: () -> Int) {} // expected-note 3 {{here}}
@available(*, unavailable, renamed: "after(fn:)")
func closure_L_L(x: () -> Int) {} // expected-note 3 {{here}}
@available(*, unavailable, renamed: "after(_:)")
func closure_L_U(x: () -> Int) {} // expected-note 3 {{here}}

@available(*, unavailable, renamed: "after(arg:fn:)")
func closure_UU_LL(_ x: Int, _ y: () -> Int) {} // expected-note 2 {{here}}
@available(*, unavailable, renamed: "after(arg:fn:)")
func closure_LU_LL(x: Int, _ y: () -> Int) {} // expected-note 2 {{here}}
@available(*, unavailable, renamed: "after(arg:fn:)")
func closure_LL_LL(x: Int, y: () -> Int) {} // expected-note 2 {{here}}
@available(*, unavailable, renamed: "after(arg:fn:)")
func closure_UU_LL_ne(_ x: Int, _ y: () -> Int) {} // expected-note 2 {{here}}

@available(*, unavailable, renamed: "after(arg:_:)")
func closure_UU_LU(_ x: Int, _ closure: () -> Int) {} // expected-note 2 {{here}}
@available(*, unavailable, renamed: "after(arg:_:)")
func closure_LU_LU(x: Int, _ closure: () -> Int) {} // expected-note 2 {{here}}
@available(*, unavailable, renamed: "after(arg:_:)")
func closure_LL_LU(x: Int, y: () -> Int) {} // expected-note 2 {{here}}
@available(*, unavailable, renamed: "after(arg:_:)")
func closure_UU_LU_ne(_ x: Int, _ y: () -> Int) {} // expected-note 2 {{here}}

func testTrailingClosure() {
  closure_U_L { 0 } // expected-error {{'closure_U_L' has been renamed to 'after(fn:)'}} {{3-14=after}} {{none}}
  closure_U_L() { 0 } // expected-error {{'closure_U_L' has been renamed to 'after(fn:)'}} {{3-14=after}} {{none}}
  closure_U_L({ 0 }) // expected-error {{'closure_U_L' has been renamed to 'after(fn:)'}} {{3-14=after}} {{15-15=fn: }} {{none}}

  closure_L_L { 0 } // expected-error {{'closure_L_L(x:)' has been renamed to 'after(fn:)'}} {{3-14=after}} {{none}}
  closure_L_L() { 0 } // expected-error {{'closure_L_L(x:)' has been renamed to 'after(fn:)'}} {{3-14=after}} {{none}}
  closure_L_L(x: { 0 }) // expected-error {{'closure_L_L(x:)' has been renamed to 'after(fn:)'}} {{3-14=after}} {{15-16=fn}} {{none}}

  closure_L_U { 0 } // expected-error {{'closure_L_U(x:)' has been renamed to 'after(_:)'}} {{3-14=after}} {{none}}
  closure_L_U() { 0 } // expected-error {{'closure_L_U(x:)' has been renamed to 'after(_:)'}} {{3-14=after}} {{none}}
  closure_L_U(x: { 0 }) // expected-error {{'closure_L_U(x:)' has been renamed to 'after(_:)'}} {{3-14=after}} {{15-18=}} {{none}}

  closure_UU_LL(0) { 0 } // expected-error {{'closure_UU_LL' has been renamed to 'after(arg:fn:)'}} {{3-16=after}} {{17-17=arg: }} {{none}}
  closure_UU_LL(0, { 0 }) // expected-error {{'closure_UU_LL' has been renamed to 'after(arg:fn:)'}} {{3-16=after}} {{17-17=arg: }} {{20-20=fn: }} {{none}}

  closure_LU_LL(x: 0) { 0 } // expected-error {{'closure_LU_LL(x:_:)' has been renamed to 'after(arg:fn:)'}} {{3-16=after}} {{17-18=arg}} {{none}}
  closure_LU_LL(x: 0, { 0 }) // expected-error {{'closure_LU_LL(x:_:)' has been renamed to 'after(arg:fn:)'}} {{3-16=after}} {{17-18=arg}} {{23-23=fn: }} {{none}}

  closure_LL_LL(x: 1) { 1 } // expected-error {{'closure_LL_LL(x:y:)' has been renamed to 'after(arg:fn:)'}} {{3-16=after}} {{17-18=arg}} {{none}}
  closure_LL_LL(x: 1, y: { 0 }) // expected-error {{'closure_LL_LL(x:y:)' has been renamed to 'after(arg:fn:)'}} {{3-16=after}} {{17-18=arg}} {{23-24=fn}} {{none}}

  closure_UU_LL_ne(1) { 1 } // expected-error {{'closure_UU_LL_ne' has been renamed to 'after(arg:fn:)'}} {{3-19=after}} {{20-20=arg: }} {{none}}
  closure_UU_LL_ne(1, { 0 }) // expected-error {{'closure_UU_LL_ne' has been renamed to 'after(arg:fn:)'}} {{3-19=after}} {{20-20=arg: }} {{23-23=fn: }} {{none}}

  closure_UU_LU(0) { 0 } // expected-error {{'closure_UU_LU' has been renamed to 'after(arg:_:)'}} {{3-16=after}} {{17-17=arg: }} {{none}}
  closure_UU_LU(0, { 0 }) // expected-error {{'closure_UU_LU' has been renamed to 'after(arg:_:)'}} {{3-16=after}} {{17-17=arg: }} {{none}}

  closure_LU_LU(x: 0) { 0 } // expected-error {{'closure_LU_LU(x:_:)' has been renamed to 'after(arg:_:)'}} {{3-16=after}} {{17-18=arg}} {{none}}
  closure_LU_LU(x: 0, { 0 }) // expected-error {{'closure_LU_LU(x:_:)' has been renamed to 'after(arg:_:)'}} {{3-16=after}} {{17-18=arg}} {{none}}

  closure_LL_LU(x: 1) { 1 } // expected-error {{'closure_LL_LU(x:y:)' has been renamed to 'after(arg:_:)'}} {{3-16=after}} {{17-18=arg}} {{none}}
  closure_LL_LU(x: 1, y: { 0 }) // expected-error {{'closure_LL_LU(x:y:)' has been renamed to 'after(arg:_:)'}} {{3-16=after}} {{17-18=arg}} {{23-26=}} {{none}}

  closure_UU_LU_ne(1) { 1 } // expected-error {{'closure_UU_LU_ne' has been renamed to 'after(arg:_:)'}} {{3-19=after}} {{20-20=arg: }} {{none}}
  closure_UU_LU_ne(1, { 0 }) // expected-error {{'closure_UU_LU_ne' has been renamed to 'after(arg:_:)'}} {{3-19=after}} {{20-20=arg: }} {{none}}
}

@available(*, unavailable, renamed: "after(x:)")
func defaultUnnamed(_ a: Int = 1) {} // expected-note 2 {{here}}
@available(*, unavailable, renamed: "after(x:y:)")
func defaultBeforeRequired(a: Int = 1, b: Int) {} // expected-note {{here}}
@available(*, unavailable, renamed: "after(x:y:z:)")
func defaultPlusTrailingClosure(a: Int = 1, b: Int = 2, c: () -> Void) {} // expected-note 3 {{here}}

func testDefaults() {
  defaultUnnamed() // expected-error {{'defaultUnnamed' has been renamed to 'after(x:)'}} {{3-17=after}} {{none}}
  defaultUnnamed(1) // expected-error {{'defaultUnnamed' has been renamed to 'after(x:)'}} {{3-17=after}} {{18-18=x: }} {{none}}
  defaultBeforeRequired(b: 5) // expected-error {{'defaultBeforeRequired(a:b:)' has been renamed to 'after(x:y:)'}} {{3-24=after}} {{25-26=y}} {{none}}
  defaultPlusTrailingClosure {} // expected-error {{'defaultPlusTrailingClosure(a:b:c:)' has been renamed to 'after(x:y:z:)'}} {{3-29=after}} {{none}}
  defaultPlusTrailingClosure(c: {}) // expected-error {{'defaultPlusTrailingClosure(a:b:c:)' has been renamed to 'after(x:y:z:)'}} {{3-29=after}} {{30-31=z}} {{none}}
  defaultPlusTrailingClosure(a: 1) {} // expected-error {{'defaultPlusTrailingClosure(a:b:c:)' has been renamed to 'after(x:y:z:)'}} {{3-29=after}} {{30-31=x}} {{none}}
}

@available(*, unavailable, renamed: "after(x:y:)")
func variadic1(a: Int ..., b: Int = 0) {} // expected-note 2 {{here}}
@available(*, unavailable, renamed: "after(x:y:)")
func variadic2(a: Int, _ b: Int ...) {} // expected-note {{here}}
@available(*, unavailable, renamed: "after(x:_:y:z:)")
func variadic3(_ a: Int, b: Int ..., c: String = "", d: String) {} // expected-note 2 {{here}}

func testVariadic() {
  variadic1(a: 1, 2) // expected-error {{'variadic1(a:b:)' has been renamed to 'after(x:y:)'}} {{3-12=after}} {{13-14=x}} {{none}}
  variadic1(a: 1, 2, b: 3) // expected-error {{'variadic1(a:b:)' has been renamed to 'after(x:y:)'}} {{3-12=after}} {{13-14=x}} {{22-23=y}} {{none}}
  variadic2(a: 1, 2, 3) // expected-error {{'variadic2(a:_:)' has been renamed to 'after(x:y:)'}} {{3-12=after}} {{13-14=x}} {{19-19=y: }} {{none}}
  variadic3(1, b: 2, 3, d: "test") // expected-error {{'variadic3(_:b:c:d:)' has been renamed to 'after(x:_:y:z:)'}} {{3-12=after}} {{13-13=x: }} {{16-19=}} {{25-26=z}} {{none}}
  variadic3(1, d:"test") // expected-error {{'variadic3(_:b:c:d:)' has been renamed to 'after(x:_:y:z:)'}} {{3-12=after}} {{13-13=x: }} {{16-17=z}} {{none}}
}

enum E_32526620 {
  case foo
  case bar

  func set() {}
}

@available(*, unavailable, renamed: "E_32526620.set(self:)")
func rdar32526620_1(a: E_32526620) {} // expected-note {{here}}
rdar32526620_1(a: .foo)
// expected-error@-1 {{'rdar32526620_1(a:)' has been replaced by instance method 'E_32526620.set()'}} {{1-15=E_32526620.foo.set}} {{16-23=}}

@available(*, unavailable, renamed: "E_32526620.set(a:self:)")
func rdar32526620_2(a: Int, b: E_32526620) {} // expected-note {{here}}
rdar32526620_2(a: 42, b: .bar)
// expected-error@-1 {{'rdar32526620_2(a:b:)' has been replaced by instance method 'E_32526620.set(a:)'}} {{1-15=E_32526620.bar.set}} {{21-30=}}

@available(*, unavailable, renamed: "E_32526620.set(a:self:c:)")
func rdar32526620_3(a: Int, b: E_32526620, c: String) {} // expected-note {{here}}
rdar32526620_3(a: 42, b: .bar, c: "question")
// expected-error@-1 {{'rdar32526620_3(a:b:c:)' has been replaced by instance method 'E_32526620.set(a:c:)'}} {{1-15=E_32526620.bar.set}} {{23-32=}}


var deprecatedGetter: Int {
  @available(*, deprecated) get { return 0 }
  set {}
}
var deprecatedGetterOnly: Int {
  @available(*, deprecated) get { return 0 }
}
var deprecatedSetter: Int {
  get { return 0 }
  @available(*, deprecated) set {}
}
var deprecatedBoth: Int {
  @available(*, deprecated) get { return 0 }
  @available(*, deprecated) set {}
}
var deprecatedMessage: Int {
  @available(*, deprecated, message: "bad getter") get { return 0 }
  @available(*, deprecated, message: "bad setter") set {}
}
var deprecatedRename: Int {
  @available(*, deprecated, renamed: "betterThing()") get { return 0 }
  @available(*, deprecated, renamed: "setBetterThing(_:)") set {}
}
@available(*, deprecated, message: "bad variable")
var deprecatedProperty: Int {
  @available(*, deprecated, message: "bad getter") get { return 0 }
  @available(*, deprecated, message: "bad setter") set {}
}

_ = deprecatedGetter // expected-warning {{getter for 'deprecatedGetter' is deprecated}}{{documentation-file=deprecated-declaration}} {{none}}
deprecatedGetter = 0
deprecatedGetter += 1 // expected-warning {{getter for 'deprecatedGetter' is deprecated}}{{documentation-file=deprecated-declaration}} {{none}}

_ = deprecatedGetterOnly // expected-warning {{getter for 'deprecatedGetterOnly' is deprecated}}{{documentation-file=deprecated-declaration}} {{none}}

_ = deprecatedSetter
deprecatedSetter = 0 // expected-warning {{setter for 'deprecatedSetter' is deprecated}}{{documentation-file=deprecated-declaration}} {{none}}
deprecatedSetter += 1 // expected-warning {{setter for 'deprecatedSetter' is deprecated}}{{documentation-file=deprecated-declaration}} {{none}}

_ = deprecatedBoth // expected-warning {{getter for 'deprecatedBoth' is deprecated}}{{documentation-file=deprecated-declaration}} {{none}}
deprecatedBoth = 0 // expected-warning {{setter for 'deprecatedBoth' is deprecated}}{{documentation-file=deprecated-declaration}} {{none}}
deprecatedBoth += 1 // expected-warning {{getter for 'deprecatedBoth' is deprecated}}{{documentation-file=deprecated-declaration}} {{none}} expected-warning {{setter for 'deprecatedBoth' is deprecated}}{{documentation-file=deprecated-declaration}} {{none}}

_ = deprecatedMessage // expected-warning {{getter for 'deprecatedMessage' is deprecated: bad getter}}{{documentation-file=deprecated-declaration}} {{none}}
deprecatedMessage = 0 // expected-warning {{setter for 'deprecatedMessage' is deprecated: bad setter}}{{documentation-file=deprecated-declaration}} {{none}}
deprecatedMessage += 1 // expected-warning {{getter for 'deprecatedMessage' is deprecated: bad getter}}{{documentation-file=deprecated-declaration}} {{none}} expected-warning {{setter for 'deprecatedMessage' is deprecated: bad setter}}{{documentation-file=deprecated-declaration}} {{none}}

_ = deprecatedRename // expected-warning {{getter for 'deprecatedRename' is deprecated: renamed to 'betterThing()'}}{{documentation-file=deprecated-declaration}} {{none}}
deprecatedRename = 0  // expected-warning {{setter for 'deprecatedRename' is deprecated: renamed to 'setBetterThing(_:)'}}{{documentation-file=deprecated-declaration}} {{none}}
deprecatedRename += 1 // expected-warning {{getter for 'deprecatedRename' is deprecated: renamed to 'betterThing()'}}{{documentation-file=deprecated-declaration}} {{none}} expected-warning {{setter for 'deprecatedRename' is deprecated: renamed to 'setBetterThing(_:)'}}{{documentation-file=deprecated-declaration}} {{none}}

_ = deprecatedProperty // expected-warning {{'deprecatedProperty' is deprecated: bad variable}}{{documentation-file=deprecated-declaration}} {{none}}
deprecatedProperty = 0 // expected-warning {{'deprecatedProperty' is deprecated: bad variable}}{{documentation-file=deprecated-declaration}} {{none}}
deprecatedProperty += 1 // expected-warning {{'deprecatedProperty' is deprecated: bad variable}}{{documentation-file=deprecated-declaration}} {{none}}

var unavailableGetter: Int {
  @available(*, unavailable) get { return 0 } // expected-note * {{here}}
  set {}
}
var unavailableGetterOnly: Int {
  @available(*, unavailable) get { return 0 } // expected-note * {{here}}
}
var unavailableSetter: Int {
  get { return 0 }
  @available(*, unavailable) set {} // expected-note * {{here}}
}
var unavailableBoth: Int {
  @available(*, unavailable) get { return 0 } // expected-note * {{here}}
  @available(*, unavailable) set {} // expected-note * {{here}}
}
var unavailableMessage: Int {
  @available(*, unavailable, message: "bad getter") get { return 0 } // expected-note * {{here}}
  @available(*, unavailable, message: "bad setter") set {} // expected-note * {{here}}
}
var unavailableRename: Int {
  @available(*, unavailable, renamed: "betterThing()") get { return 0 } // expected-note * {{here}}
  @available(*, unavailable, renamed: "setBetterThing(_:)") set {} // expected-note * {{here}}
}
@available(*, unavailable, message: "bad variable")
var unavailableProperty: Int { // expected-note * {{here}}
  @available(*, unavailable, message: "bad getter") get { return 0 }
  @available(*, unavailable, message: "bad setter") set {}
}

_ = unavailableGetter // expected-error {{getter for 'unavailableGetter' is unavailable}} {{none}}
unavailableGetter = 0
unavailableGetter += 1 // expected-error {{getter for 'unavailableGetter' is unavailable}} {{none}}

_ = unavailableGetterOnly // expected-error {{getter for 'unavailableGetterOnly' is unavailable}} {{none}}

_ = unavailableSetter
unavailableSetter = 0 // expected-error {{setter for 'unavailableSetter' is unavailable}} {{none}}
unavailableSetter += 1 // expected-error {{setter for 'unavailableSetter' is unavailable}} {{none}}

_ = unavailableBoth // expected-error {{getter for 'unavailableBoth' is unavailable}} {{none}}
unavailableBoth = 0 // expected-error {{setter for 'unavailableBoth' is unavailable}} {{none}}
unavailableBoth += 1 // expected-error {{getter for 'unavailableBoth' is unavailable}} {{none}} expected-error {{setter for 'unavailableBoth' is unavailable}} {{none}}

_ = unavailableMessage // expected-error {{getter for 'unavailableMessage' is unavailable: bad getter}} {{none}}
unavailableMessage = 0 // expected-error {{setter for 'unavailableMessage' is unavailable: bad setter}} {{none}}
unavailableMessage += 1 // expected-error {{getter for 'unavailableMessage' is unavailable: bad getter}} {{none}} expected-error {{setter for 'unavailableMessage' is unavailable: bad setter}} {{none}}

_ = unavailableRename // expected-error {{getter for 'unavailableRename' has been renamed to 'betterThing()'}} {{none}}
unavailableRename = 0  // expected-error {{setter for 'unavailableRename' has been renamed to 'setBetterThing(_:)'}} {{none}}
unavailableRename += 1 // expected-error {{getter for 'unavailableRename' has been renamed to 'betterThing()'}} {{none}} expected-error {{setter for 'unavailableRename' has been renamed to 'setBetterThing(_:)'}} {{none}}

_ = unavailableProperty // expected-error {{'unavailableProperty' is unavailable: bad variable}} {{none}}
unavailableProperty = 0 // expected-error {{'unavailableProperty' is unavailable: bad variable}} {{none}}
unavailableProperty += 1 // expected-error {{'unavailableProperty' is unavailable: bad variable}} {{none}}

struct DeprecatedAccessors {
  var deprecatedMessage: Int {
    @available(*, deprecated, message: "bad getter") get { return 0 }
    @available(*, deprecated, message: "bad setter") set {}
  }

  static var staticDeprecated: Int {
    @available(*, deprecated, message: "bad getter") get { return 0 }
    @available(*, deprecated, message: "bad setter") set {}
  }

  @available(*, deprecated, message: "bad property")
  var deprecatedProperty: Int {
    @available(*, deprecated, message: "bad getter") get { return 0 }
    @available(*, deprecated, message: "bad setter") set {}
  }

  subscript(_: Int) -> Int {
    @available(*, deprecated, message: "bad subscript getter") get { return 0 }
    @available(*, deprecated, message: "bad subscript setter") set {}
  }

  @available(*, deprecated, message: "bad subscript!")
  subscript(alsoDeprecated _: Int) -> Int {
    @available(*, deprecated, message: "bad subscript getter") get { return 0 }
    @available(*, deprecated, message: "bad subscript setter") set {}
  }

  mutating func testAccessors(other: inout DeprecatedAccessors) {
    _ = deprecatedMessage // expected-warning {{getter for 'deprecatedMessage' is deprecated: bad getter}}{{documentation-file=deprecated-declaration}} {{none}}
    deprecatedMessage = 0 // expected-warning {{setter for 'deprecatedMessage' is deprecated: bad setter}}{{documentation-file=deprecated-declaration}} {{none}}
    deprecatedMessage += 1 // expected-warning {{getter for 'deprecatedMessage' is deprecated: bad getter}}{{documentation-file=deprecated-declaration}} {{none}} expected-warning {{setter for 'deprecatedMessage' is deprecated: bad setter}}{{documentation-file=deprecated-declaration}} {{none}}

    _ = other.deprecatedMessage // expected-warning {{getter for 'deprecatedMessage' is deprecated: bad getter}}{{documentation-file=deprecated-declaration}} {{none}}
    other.deprecatedMessage = 0 // expected-warning {{setter for 'deprecatedMessage' is deprecated: bad setter}}{{documentation-file=deprecated-declaration}} {{none}}
    other.deprecatedMessage += 1 // expected-warning {{getter for 'deprecatedMessage' is deprecated: bad getter}}{{documentation-file=deprecated-declaration}} {{none}} expected-warning {{setter for 'deprecatedMessage' is deprecated: bad setter}}{{documentation-file=deprecated-declaration}} {{none}}

    _ = other.deprecatedProperty // expected-warning {{'deprecatedProperty' is deprecated: bad property}}{{documentation-file=deprecated-declaration}} {{none}}
    other.deprecatedProperty = 0 // expected-warning {{'deprecatedProperty' is deprecated: bad property}}{{documentation-file=deprecated-declaration}} {{none}}
    other.deprecatedProperty += 1 // expected-warning {{'deprecatedProperty' is deprecated: bad property}}{{documentation-file=deprecated-declaration}} {{none}}

    _ = DeprecatedAccessors.staticDeprecated // expected-warning {{getter for 'staticDeprecated' is deprecated: bad getter}}{{documentation-file=deprecated-declaration}} {{none}}
    DeprecatedAccessors.staticDeprecated = 0 // expected-warning {{setter for 'staticDeprecated' is deprecated: bad setter}}{{documentation-file=deprecated-declaration}} {{none}}
    DeprecatedAccessors.staticDeprecated += 1 // expected-warning {{getter for 'staticDeprecated' is deprecated: bad getter}}{{documentation-file=deprecated-declaration}} {{none}} expected-warning {{setter for 'staticDeprecated' is deprecated: bad setter}}{{documentation-file=deprecated-declaration}} {{none}}

    _ = other[0] // expected-warning {{getter for 'subscript(_:)' is deprecated: bad subscript getter}}{{documentation-file=deprecated-declaration}} {{none}}
    other[0] = 0 // expected-warning {{setter for 'subscript(_:)' is deprecated: bad subscript setter}}{{documentation-file=deprecated-declaration}} {{none}}
    other[0] += 1 // expected-warning {{getter for 'subscript(_:)' is deprecated: bad subscript getter}}{{documentation-file=deprecated-declaration}} {{none}} expected-warning {{setter for 'subscript(_:)' is deprecated: bad subscript setter}}{{documentation-file=deprecated-declaration}} {{none}}

    _ = other[alsoDeprecated: 0] // expected-warning {{'subscript(alsoDeprecated:)' is deprecated: bad subscript!}}{{documentation-file=deprecated-declaration}} {{none}}
    other[alsoDeprecated: 0] = 0 // expected-warning {{'subscript(alsoDeprecated:)' is deprecated: bad subscript!}}{{documentation-file=deprecated-declaration}} {{none}}
    other[alsoDeprecated: 0] += 1 // expected-warning {{'subscript(alsoDeprecated:)' is deprecated: bad subscript!}}{{documentation-file=deprecated-declaration}} {{none}}
  }
}

struct UnavailableAccessors {
  var unavailableMessage: Int {
    @available(*, unavailable, message: "bad getter") get { return 0 } // expected-note * {{here}}
    @available(*, unavailable, message: "bad setter") set {} // expected-note * {{here}}
  }

  static var staticUnavailable: Int {
    @available(*, unavailable, message: "bad getter") get { return 0 } // expected-note * {{here}}
    @available(*, unavailable, message: "bad setter") set {} // expected-note * {{here}}
  }

  @available(*, unavailable, message: "bad property")
  var unavailableProperty: Int { // expected-note * {{here}}
    @available(*, unavailable, message: "bad getter") get { return 0 }
    @available(*, unavailable, message: "bad setter") set {}
  }

  subscript(_: Int) -> Int {
    @available(*, unavailable, message: "bad subscript getter") get { return 0 } // expected-note * {{here}}
    @available(*, unavailable, message: "bad subscript setter") set {} // expected-note * {{here}}
  }

  @available(*, unavailable, message: "bad subscript!")
  subscript(alsoUnavailable _: Int) -> Int { // expected-note * {{here}}
    @available(*, unavailable, message: "bad subscript getter") get { return 0 }
    @available(*, unavailable, message: "bad subscript setter") set {}
  }

  mutating func testAccessors(other: inout UnavailableAccessors) {
    _ = unavailableMessage // expected-error {{getter for 'unavailableMessage' is unavailable: bad getter}} {{none}}
    unavailableMessage = 0 // expected-error {{setter for 'unavailableMessage' is unavailable: bad setter}} {{none}}
    unavailableMessage += 1 // expected-error {{getter for 'unavailableMessage' is unavailable: bad getter}} {{none}} expected-error {{setter for 'unavailableMessage' is unavailable: bad setter}} {{none}}

    _ = other.unavailableMessage // expected-error {{getter for 'unavailableMessage' is unavailable: bad getter}} {{none}}
    other.unavailableMessage = 0 // expected-error {{setter for 'unavailableMessage' is unavailable: bad setter}} {{none}}
    other.unavailableMessage += 1 // expected-error {{getter for 'unavailableMessage' is unavailable: bad getter}} {{none}} expected-error {{setter for 'unavailableMessage' is unavailable: bad setter}} {{none}}

    _ = other.unavailableProperty // expected-error {{'unavailableProperty' is unavailable: bad property}} {{none}}
    other.unavailableProperty = 0 // expected-error {{'unavailableProperty' is unavailable: bad property}} {{none}}
    other.unavailableProperty += 1 // expected-error {{'unavailableProperty' is unavailable: bad property}} {{none}}

    _ = UnavailableAccessors.staticUnavailable // expected-error {{getter for 'staticUnavailable' is unavailable: bad getter}} {{none}}
    UnavailableAccessors.staticUnavailable = 0 // expected-error {{setter for 'staticUnavailable' is unavailable: bad setter}} {{none}}
    UnavailableAccessors.staticUnavailable += 1 // expected-error {{getter for 'staticUnavailable' is unavailable: bad getter}} {{none}} expected-error {{setter for 'staticUnavailable' is unavailable: bad setter}} {{none}}

    _ = other[0] // expected-error {{getter for 'subscript(_:)' is unavailable: bad subscript getter}} {{none}}
    other[0] = 0 // expected-error {{setter for 'subscript(_:)' is unavailable: bad subscript setter}} {{none}}
    other[0] += 1 // expected-error {{getter for 'subscript(_:)' is unavailable: bad subscript getter}} {{none}} expected-error {{setter for 'subscript(_:)' is unavailable: bad subscript setter}} {{none}}

    _ = other[alsoUnavailable: 0] // expected-error {{'subscript(alsoUnavailable:)' is unavailable: bad subscript!}} {{none}}
    other[alsoUnavailable: 0] = 0 // expected-error {{'subscript(alsoUnavailable:)' is unavailable: bad subscript!}} {{none}}
    other[alsoUnavailable: 0] += 1 // expected-error {{'subscript(alsoUnavailable:)' is unavailable: bad subscript!}} {{none}}
  }
}

// https://github.com/apple/swift/issues/51149
// Should produce no warnings.

enum Enum_51149: Int {
  case a
  @available(*, deprecated, message: "I must not be raised in synthesized code")
  case b
  case c
}

struct Struct_51149: Equatable {
  @available(*, deprecated, message: "I must not be raised in synthesized code", renamed: "x")
  let a: Int
}

@available(*, deprecated, message: "This is a message", message: "This is another message")
// expected-warning@-1 {{'message' argument has already been specified}}
func rdar46348825_message() {}

@available(*, deprecated, renamed: "rdar46348825_message", renamed: "unavailable_func_with_message")
// expected-warning@-1 {{'renamed' argument has already been specified}}
func rdar46348825_renamed() {}

@available(swift, introduced: 4.0, introduced: 4.0)
// expected-warning@-1 {{'introduced' argument has already been specified}}
func rdar46348825_introduced() {}

@available(swift, deprecated: 4.0, deprecated: 4.0)
// expected-warning@-1 {{'deprecated' argument has already been specified}}
func rdar46348825_deprecated() {}

@available(swift, obsoleted: 4.0, obsoleted: 4.0)
// expected-warning@-1 {{'obsoleted' argument has already been specified}}
func rdar46348825_obsoleted() {}

// Referencing unavailable types in signatures of unavailable functions should be accepted
@available(*, unavailable)
protocol UnavailableProto {
}

@available(*, unavailable)
func unavailableFunc(_ arg: UnavailableProto) -> UnavailableProto {}

@available(*, unavailable)
struct S {
  var a: UnavailableProto
}

// Bad rename.
struct BadRename {
  @available(*, deprecated, renamed: "init(range:step:)")
  init(from: Int, to: Int, step: Int = 1) { }

  init(range: Range<Int>, step: Int) { }
}

func testBadRename() {
  _ = BadRename(from: 5, to: 17) // expected-warning{{'init(from:to:step:)' is deprecated: replaced by 'init(range:step:)'}}{{documentation-file=deprecated-declaration}}
  // expected-note@-1{{use 'init(range:step:)' instead}}
}

struct AvailableGenericParam<@available(*, deprecated) T> {}
// expected-error@-1 {{'@available' attribute cannot be applied to this declaration}}

struct TypeWithTrailingClosures {
  func twoTrailingClosures(a: () -> Void, b: () -> Void) {}
  func threeTrailingClosures(a: () -> Void, b: () -> Void, c: () -> Void) {}
  func threeUnlabeledTrailingClosures(_ a: () -> Void, _ b: () -> Void, _ c: () -> Void) {}
  func variadicTrailingClosures(a: (() -> Void)..., b: Int = 0, c: Int = 0) {}
}

@available(*, deprecated, renamed: "TypeWithTrailingClosures.twoTrailingClosures(self:a:b:)")
func twoTrailingClosures(_ x: TypeWithTrailingClosures, a: () -> Void, b: () -> Void) {}

@available(*, deprecated, renamed: "TypeWithTrailingClosures.twoTrailingClosures(self:a:b:)")
func twoTrailingClosuresWithDefaults(x: TypeWithTrailingClosures, y: Int = 0, z: Int = 0, a: () -> Void, b: () -> Void) {}

@available(*, deprecated, renamed: "TypeWithTrailingClosures.threeTrailingClosures(self:a:b:c:)")
func threeTrailingClosures(_ x: TypeWithTrailingClosures, a: () -> Void, b: () -> Void, c: () -> Void) {}

@available(*, deprecated, renamed: "TypeWithTrailingClosures.threeTrailingClosures(self:a:b:c:)")
func threeTrailingClosuresDiffLabels(_: TypeWithTrailingClosures, x: () -> Void, y: () -> Void, z: () -> Void) {}

@available(*, deprecated, renamed: "TypeWithTrailingClosures.threeUnlabeledTrailingClosures(self:_:_:_:)")
func threeTrailingClosuresRemoveLabels(_ x: TypeWithTrailingClosures, a: () -> Void, b: () -> Void, c: () -> Void) {}

@available(*, deprecated, renamed: "TypeWithTrailingClosures.variadicTrailingClosures(self:a:b:c:)")
func variadicTrailingClosures(_ x: TypeWithTrailingClosures, a: (() -> Void)...) {}

func testMultipleTrailingClosures(_ x: TypeWithTrailingClosures) {
  twoTrailingClosures(x) {} b: {} // expected-warning {{'twoTrailingClosures(_:a:b:)' is deprecated: replaced by instance method 'TypeWithTrailingClosures.twoTrailingClosures(a:b:)'}}{{documentation-file=deprecated-declaration}}
  // expected-note@-1 {{use 'TypeWithTrailingClosures.twoTrailingClosures(a:b:)' instead}} {{3-22=x.twoTrailingClosures}} {{23-24=}} {{none}}
  x.twoTrailingClosures() {} b: {}

  twoTrailingClosuresWithDefaults(x: x) {} b: {} // expected-warning {{'twoTrailingClosuresWithDefaults(x:y:z:a:b:)' is deprecated: replaced by instance method 'TypeWithTrailingClosures.twoTrailingClosures(a:b:)'}}{{documentation-file=deprecated-declaration}}
  // expected-note@-1 {{use 'TypeWithTrailingClosures.twoTrailingClosures(a:b:)' instead}} {{3-34=x.twoTrailingClosures}} {{35-39=}} {{none}}
  x.twoTrailingClosures() {} b: {}

  threeTrailingClosures(x, a: {}) {} c: {} // expected-warning {{'threeTrailingClosures(_:a:b:c:)' is deprecated: replaced by instance method 'TypeWithTrailingClosures.threeTrailingClosures(a:b:c:)'}}{{documentation-file=deprecated-declaration}}
  // expected-note@-1 {{use 'TypeWithTrailingClosures.threeTrailingClosures(a:b:c:)' instead}} {{3-24=x.threeTrailingClosures}} {{25-28=}} {{none}}
  x.threeTrailingClosures(a: {}) {} c: {}

  threeTrailingClosuresDiffLabels(x, x: {}) {} z: {} // expected-warning {{'threeTrailingClosuresDiffLabels(_:x:y:z:)' is deprecated: replaced by instance method 'TypeWithTrailingClosures.threeTrailingClosures(a:b:c:)'}}{{documentation-file=deprecated-declaration}}
  // expected-note@-1 {{use 'TypeWithTrailingClosures.threeTrailingClosures(a:b:c:)' instead}} {{3-34=x.threeTrailingClosures}} {{35-38=}} {{38-39=a}} {{48-49=c}} {{none}}
  x.threeTrailingClosures(a: {}) {} c: {}

  threeTrailingClosuresRemoveLabels(x, a: {}) {} c: {} // expected-warning {{'threeTrailingClosuresRemoveLabels(_:a:b:c:)' is deprecated: replaced by instance method 'TypeWithTrailingClosures.threeUnlabeledTrailingClosures(_:_:_:)'}}{{documentation-file=deprecated-declaration}}
  // expected-note@-1 {{use 'TypeWithTrailingClosures.threeUnlabeledTrailingClosures(_:_:_:)' instead}} {{3-36=x.threeUnlabeledTrailingClosures}} {{37-40=}} {{40-43=}} {{50-51=_}} {{none}}
  x.threeUnlabeledTrailingClosures({}) {} _: {}

  variadicTrailingClosures(x) {} _: {} _: {} // expected-warning {{'variadicTrailingClosures(_:a:)' is deprecated: replaced by instance method 'TypeWithTrailingClosures.variadicTrailingClosures(a:b:c:)'}}{{documentation-file=deprecated-declaration}}
  // expected-note@-1 {{use 'TypeWithTrailingClosures.variadicTrailingClosures(a:b:c:)' instead}} {{3-27=x.variadicTrailingClosures}} {{28-29=}} {{none}}
  x.variadicTrailingClosures() {} _: {} _: {}
}

struct UnavailableSubscripts { 
  @available(*, unavailable, renamed: "subscript(new:)")
  subscript(old index: Int) -> Int { 3 } // expected-note * {{'subscript(old:)' has been explicitly marked unavailable here}}
  @available(*, unavailable, renamed: "subscript(new:)")
  func getValue(old: Int) -> Int { 3 } // expected-note * {{'getValue(old:)' has been explicitly marked unavailable here}}

  subscript(new index: Int) -> Int { 3 }

  @available(*, unavailable, renamed: "getAValue(new:)")
  subscript(getAValue index: Int) -> Int { 3 } // expected-note * {{'subscript(getAValue:)' has been explicitly marked unavailable here}}
  func getAValue(new: Int) -> Int { 3 }

  @available(*, unavailable, renamed: "subscript(arg1:arg2:arg3:)")
  subscript(_ argg1: Int, _ argg2: Int, _ argg3: Int) -> Int { 3 } // expected-note * {{'subscript(_:_:_:)' has been explicitly marked unavailable here}}

  @available(*, deprecated, renamed: "subscript(arg1:arg2:arg3:)")
  subscript(argg1 argg1: Int, argg2 argg2: Int, argg3 argg3: Int) -> Int { 3 }

  @available(*, deprecated, renamed: "subscript(arg1:arg2:arg3:)")
  subscript(only1 only1: Int, only2 only2: Int) -> Int { 3 }

  subscript(arg1 arg1: Int, arg2 arg2: Int, arg3 arg3: Int) -> Int { 3 }

  @available(*, deprecated, renamed: "subscriptTo(_:)")
  subscript(to to: Int) -> Int { 3 }
  func subscriptTo(_ index: Int) -> Int { 3 }

  func testUnavailableSubscripts(_ x: UnavailableSubscripts) { 
    _ = self[old: 3] // expected-error {{'subscript(old:)' has been renamed to 'subscript(new:)'}} {{14-17=new}}
    _ = x[old: 3] // expected-error {{'subscript(old:)' has been renamed to 'subscript(new:)'}} {{11-14=new}}

    _ = self.getValue(old: 3) // expected-error {{'getValue(old:)' has been renamed to 'subscript(new:)'}} {{14-22=[}} {{29-30=]}} {{23-26=new}}
    _ = getValue(old: 3) // expected-error {{'getValue(old:)' has been renamed to 'subscript(new:)'}} {{9-9=self}} {{9-17=[}} {{24-25=]}} {{18-21=new}}
    _ = x.getValue(old: 3) // expected-error {{'getValue(old:)' has been renamed to 'subscript(new:)'}} {{11-19=[}} {{26-27=]}} {{20-23=new}}

    _ = self[getAValue: 3] // expected-error {{'subscript(getAValue:)' has been renamed to 'getAValue(new:)'}} {{13-14=.getAValue(}} {{26-27=)}} {{14-23=new}}
    _ = x[getAValue: 3] // expected-error {{'subscript(getAValue:)' has been renamed to 'getAValue(new:)'}} {{10-11=.getAValue(}} {{23-24=)}} {{11-20=new}}

    _ = self[argg1: 3, argg2: 3, argg3: 3] // expected-warning {{'subscript(argg1:argg2:argg3:)' is deprecated: renamed to 'subscript(arg1:arg2:arg3:)'}}{{documentation-file=deprecated-declaration}} // expected-note {{use 'subscript(arg1:arg2:arg3:)' instead}} {{14-19=arg1}} {{24-29=arg2}} {{34-39=arg3}}
    _ = x[argg1: 3, argg2: 3, argg3: 3] // expected-warning {{'subscript(argg1:argg2:argg3:)' is deprecated: renamed to 'subscript(arg1:arg2:arg3:)'}}{{documentation-file=deprecated-declaration}} // expected-note {{use 'subscript(arg1:arg2:arg3:)' instead}} {{11-16=arg1}} {{21-26=arg2}} {{31-36=arg3}}

    // Different number of parameters emit no fixit
    _ = self[only1: 3, only2: 3] // expected-warning {{'subscript(only1:only2:)' is deprecated: renamed to 'subscript(arg1:arg2:arg3:)'}}{{documentation-file=deprecated-declaration}} // expected-note {{use 'subscript(arg1:arg2:arg3:)' instead}} {{none}}

    _ = self[3, 3, 3] // expected-error {{'subscript(_:_:_:)' has been renamed to 'subscript(arg1:arg2:arg3:)'}} {{14-14=arg1: }} {{17-17=arg2: }} {{20-20=arg3: }}
    _ = x[3, 3, 3] // expected-error {{'subscript(_:_:_:)' has been renamed to 'subscript(arg1:arg2:arg3:)'}} {{11-11=arg1: }} {{14-14=arg2: }} {{17-17=arg3: }}

    _ = self[to: 3] // expected-warning {{'subscript(to:)' is deprecated: renamed to 'subscriptTo(_:)'}}{{documentation-file=deprecated-declaration}} // expected-note {{use 'subscriptTo(_:)' instead}} {{13-14=.subscriptTo(}} {{19-20=)}} {{14-18=}}
    _ = x[to: 3] // expected-warning {{'subscript(to:)' is deprecated: renamed to 'subscriptTo(_:)'}}{{documentation-file=deprecated-declaration}} // expected-note {{use 'subscriptTo(_:)' instead}} {{10-11=.subscriptTo(}} {{16-17=)}} {{11-15=}}
  }
}
