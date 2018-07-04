// RUN: %swift -typecheck -verify -parse-stdlib -module-name Swift -target x86_64-apple-macosx10.10 %s

// Fake declarations of some standard library features for -parse-stdlib.
precedencegroup AssignmentPrecedence {}
enum Optional<T> {
  case none
  case some(T)
}


@available(OSX, introduced: 10.5, deprecated: 10.8, obsoleted: 10.9,
              message: "you don't want to do that anyway")
func doSomething() { }
// expected-note @-1{{'doSomething()' was obsoleted in OS X 10.9}}

doSomething() // expected-error{{'doSomething()' is unavailable: you don't want to do that anyway}}


// Preservation of major.minor.micro
@available(OSX, introduced: 10.5, deprecated: 10.8, obsoleted: 10.9.1)
func doSomethingElse() { }
// expected-note @-1{{'doSomethingElse()' was obsoleted in OS X 10.9.1}}

doSomethingElse() // expected-error{{'doSomethingElse()' is unavailable}}

// Preservation of minor-only version
@available(OSX, introduced: 8.0, deprecated: 8.5, obsoleted: 10)
func doSomethingReallyOld() { }
// expected-note @-1{{'doSomethingReallyOld()' was obsoleted in OS X 10}}

doSomethingReallyOld() // expected-error{{'doSomethingReallyOld()' is unavailable}}

// Test deprecations in 10.10 and later

@available(OSX, introduced: 10.5, deprecated: 10.10,
              message: "Use another function")
func deprecatedFunctionWithMessage() { }

deprecatedFunctionWithMessage() // expected-warning{{'deprecatedFunctionWithMessage()' was deprecated in OS X 10.10: Use another function}}


@available(OSX, introduced: 10.5, deprecated: 10.10)
func deprecatedFunctionWithoutMessage() { }

deprecatedFunctionWithoutMessage() // expected-warning{{'deprecatedFunctionWithoutMessage()' was deprecated in OS X 10.10}}

@available(OSX, introduced: 10.5, deprecated: 10.10,
              message: "Use BetterClass instead")
class DeprecatedClass { }

func functionWithDeprecatedParameter(p: DeprecatedClass) { } // expected-warning{{'DeprecatedClass' was deprecated in OS X 10.10: Use BetterClass instead}}

@available(OSX, introduced: 10.5, deprecated: 10.11,
              message: "Use BetterClass instead")
class DeprecatedClassIn10_11 { }

// Elements deprecated later than the minimum deployment target (which is 10.10, in this case) should not generate warnings
func functionWithDeprecatedLaterParameter(p: DeprecatedClassIn10_11) { }

// Unconditional platform unavailability
@available(OSX, unavailable)
func doSomethingNotOnOSX() { }
// expected-note @-1{{'doSomethingNotOnOSX()' has been explicitly marked unavailable here}}

doSomethingNotOnOSX() // expected-error{{'doSomethingNotOnOSX()' is unavailable}}

@available(iOS, unavailable)
func doSomethingNotOniOS() { }

doSomethingNotOniOS() // okay

// Unconditional platform deprecation
@available(OSX, deprecated)
func doSomethingDeprecatedOnOSX() { }

doSomethingDeprecatedOnOSX() // expected-warning{{'doSomethingDeprecatedOnOSX()' is deprecated on OS X}}

@available(iOS, deprecated)
func doSomethingDeprecatedOniOS() { }

doSomethingDeprecatedOniOS() // okay


struct TestStruct {}

@available(macOS 10.10, *)
extension TestStruct { // expected-note {{enclosing scope here}}
  @available(swift 400)
  func doTheThing() {} // expected-note {{'doTheThing()' was introduced in Swift 400}}

  @available(macOS 10.9, *) // expected-error {{declaration cannot be more available than enclosing scope}}
  @available(swift 400)
  func doAnotherThing() {} // expected-note {{'doAnotherThing()' was introduced in Swift 400}}

  @available(macOS 10.12, *)
  @available(swift 400)
  func doThirdThing() {} // expected-note {{'doThirdThing()' was introduced in Swift 400}}

  @available(macOS 10.12, *)
  @available(swift 1)
  func doFourthThing() {}

  @available(*, deprecated)
  func doDeprecatedThing() {}
}

@available(macOS 10.11, *)
func testMemberAvailability() {
  TestStruct().doTheThing() // expected-error {{'doTheThing()' is unavailable}}
  TestStruct().doAnotherThing() // expected-error {{'doAnotherThing()' is unavailable}}
  TestStruct().doThirdThing() // expected-error {{'doThirdThing()' is unavailable}}
  TestStruct().doFourthThing() // expected-error {{'doFourthThing()' is only available on OS X 10.12 or newer}} expected-note {{'if #available'}}
  TestStruct().doDeprecatedThing() // expected-warning {{'doDeprecatedThing()' is deprecated}}
}

extension TestStruct {
  struct Data {
    mutating func mutate() {}
  }

  var unavailableGetter: Data {
    @available(macOS, unavailable, message: "bad getter")
    get { return Data() } // expected-note 2 {{here}}
    set {}
  }

  var unavailableSetter: Data {
    get { return Data() }
    @available(macOS, obsoleted: 10.5, message: "bad setter")
    set {} // expected-note 2 {{setter for 'unavailableSetter' was obsoleted in OS X 10.5}}
  }
}

func testAccessors() {
  var t = TestStruct()
  _ = t.unavailableGetter // expected-error {{getter for 'unavailableGetter' is unavailable}}
  t.unavailableGetter = .init()
  t.unavailableGetter.mutate() // expected-error {{getter for 'unavailableGetter' is unavailable}}

  _ = t.unavailableSetter
  t.unavailableSetter = .init() // expected-error {{setter for 'unavailableSetter' is unavailable: bad setter}}
  t.unavailableSetter.mutate() // expected-error {{setter for 'unavailableSetter' is unavailable: bad setter}}
}
