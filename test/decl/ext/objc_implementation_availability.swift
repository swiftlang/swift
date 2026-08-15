// RUN: %target-typecheck-verify-swift -verify-ignore-unrelated -import-objc-header %S/Inputs/objc_implementation_availability.h -target %target-stable-abi-triple -Xcc -Wno-nullability-completeness
// REQUIRES: OS=macosx

@objc @implementation extension MacOS99Class1 {
  // expected-error@-1 {{'MacOS99Class1' is only available in macOS 99.0 or newer}}
  // expected-note@-2 {{add '@available' attribute to enclosing extension}}{{1-1=@available(macOS 99.0, *)\n}}
}

@available(macOS 99.0, *)
@objc @implementation extension MacOS99Class2 { }

@available(macOS 100.0, *)
@objc @implementation extension MacOS99Class3 {
  // expected-warning@-1 {{'@objc @implementation' extension cannot implement class 'MacOS99Class3' because it is only available in macOS 100.0 or newer}}
}

@objc @implementation extension MacOSUnavailableClass1 {
  // expected-error@-1 {{'MacOSUnavailableClass1' is unavailable in macOS}}
}

@available(macOS, unavailable)
@objc @implementation extension MacOSUnavailableClass2 { }

@available(*, unavailable)
@objc @implementation extension MacOSUnavailableClass3 {
  // expected-error@-1 {{'@objc @implementation' extension cannot implement class 'MacOSUnavailableClass3' because it is unavailable}} {{none}}
  // expected-note@-3 {{extension of 'MacOSUnavailableClass3' has been explicitly marked unavailable here}} {{none}}
}

// The class extension in the header does not have availability of its own, so
// matching the availability of the class is sufficient.
@available(macOS 99.0, *)
@objc @implementation extension MacOS99Class4 {
  func macOS99ClassExtensionMethod() { }
}

@objc @implementation extension AlwaysAvailableClass {
  func macOS99Method1() { }
  // expected-warning@-1 {{instance method 'macOS99Method1()' does not match the declaration in the header because it must be only available in macOS 99.0 or newer}} {{3-3=@available(macOS 99.0, *)\n  }}

  @available(macOS 99.0, *)
  func macOS99Method2() { }

  @available(macOS 100.0, *)
  func macOS99Method3() { }
  // expected-warning@-1 {{instance method 'macOS99Method3()' does not match the declaration in the header because it is only available in macOS 100.0 or newer}}

  @objc(macOS99Method4)
  func macOS99Method4() { }
  // expected-warning@-1 {{instance method 'macOS99Method4()' does not match the declaration in the header because it must be only available in macOS 99.0 or newer}} {{-1:3-3=@available(macOS 99.0, *)\n  }}

  // No fix-it is offered because a stored property cannot be marked
  // potentially unavailable.
  var macOS99Property1: CInt
  // expected-warning@-1 {{property 'macOS99Property1' does not match the declaration in the header because it must be only available in macOS 99.0 or newer}}

  @available(macOS 99.0, *) // expected-error {{stored properties cannot be marked potentially unavailable with '@available'}}
  var macOS99Property2: CInt

  @available(macOS 100.0, *) // expected-error {{stored properties cannot be marked potentially unavailable with '@available'}}
  var macOS99Property3: CInt
  // expected-warning@-1 {{property 'macOS99Property3' does not match the declaration in the header because it is only available in macOS 100.0 or newer}}

  func macOSUnavailableMethod1() { }
  // expected-warning@-1 {{instance method 'macOSUnavailableMethod1()' does not match the declaration in the header because it must be unavailable in macOS}} {{3-3=@available(macOS, unavailable)\n  }}

  @available(macOS, unavailable)
  func macOSUnavailableMethod2() { }

  @available(*, unavailable)
  func macOSUnavailableMethod3() { }
  // expected-error@-1 {{instance method 'macOSUnavailableMethod3()' does not match the declaration in the header because it is unavailable}}
  // expected-note@-3 {{'macOSUnavailableMethod3()' has been explicitly marked unavailable here}}
}

@objc @implementation extension MacOSDeprecated10_10Class1 { }
// expected-warning@-1 {{'MacOSDeprecated10_10Class1' was deprecated in macOS 10.10}}

@available(macOS, deprecated: 10.10)
@objc @implementation extension MacOSDeprecated10_10Class2 { }

@objc @implementation extension MacOSDeprecated99Class1 { }

@objc @implementation extension DeprecatedMembersClass {
  func macOSDeprecated10_10Method1() { }

  // But it may be.
  @available(macOS, deprecated: 10.10)
  func macOSDeprecated10_10Method2() { }

  func macOSDeprecated99Method1() { }

  var macOSDeprecated10_10Property1: CInt

  @available(macOS, deprecated: 10.10)
  func alwaysAvailableMethod1() { }
}

@objc @implementation extension AsyncMembersClass {
  @available(macOS 99.0, *)
  func macOS99Method1() async { }

  @available(macOS 10.15, *)
  func macOS99Method2() async { }
  // expected-warning@-1 {{instance method 'macOS99Method2()' does not match the declaration in the header because it must be only available in macOS 99.0 or newer}} {{-1:3-3=@available(macOS 99.0, *)\n  }}

  @available(macOS 100.0, *)
  func macOS99Method3() async { }
  // expected-warning@-1 {{instance method 'macOS99Method3()' does not match the declaration in the header because it is only available in macOS 100.0 or newer}}

  func macOS99Method4(completionHandler: @escaping () -> Void) { }
  // expected-warning@-1 {{instance method 'macOS99Method4(completionHandler:)' does not match the declaration in the header because it must be only available in macOS 99.0 or newer}} {{3-3=@available(macOS 99.0, *)\n  }}

  @available(macOS 10.15, *)
  func macOSUnavailableMethod1() async { }
  // expected-warning@-1 {{instance method 'macOSUnavailableMethod1()' does not match the declaration in the header because it must be unavailable in macOS}} {{-1:3-3=@available(macOS, unavailable)\n  }}

  @available(macOS 99.0, *)
  func alwaysAvailableMethod1() async { }
  // expected-warning@-1 {{instance method 'alwaysAvailableMethod1()' does not match the declaration in the header because it is only available in macOS 99.0 or newer}}

  // An 'async' implementation may additionally require the availability of the
  // back deployed Swift concurrency runtime, since it cannot be called without
  // it and the header has no way to express that requirement.
  @available(macOS 10.15, *)
  func alwaysAvailableMethod2() async { }
}

@objc @implementation extension AccessorMembersClass {
  @available(macOS 99.0, *)
  var macOS99Property1: CInt {
    get { 0 }
    set { }
  }

  @available(macOS 99.0, *)
  var macOS99Property2: CInt {
    @available(macOS 100.0, *)
    get { 0 }
    // expected-warning@-1 {{getter for property 'macOS99Property2' does not match the declaration in the header because it is only available in macOS 100.0 or newer}}
    set { }
  }

  @available(macOS 99.0, *)
  var macOS99Property3: CInt {
    get { 0 }
    @available(macOS, unavailable)
    // expected-note@-1 {{setter for 'macOS99Property3' has been explicitly marked unavailable here}}
    set { }
    // expected-warning@-1 {{setter for property 'macOS99Property3' does not match the declaration in the header because it is unavailable in macOS}}
  }

  // OK, the implicit getter of a read-only property inherits its availability.
  @available(macOS 99.0, *)
  var macOS99Property4: CInt { 0 }

  var alwaysAvailableProperty1: CInt {
    @available(macOS 99.0, *)
    get { 0 }
    // expected-warning@-1 {{getter for property 'alwaysAvailableProperty1' does not match the declaration in the header because it is only available in macOS 99.0 or newer}}
    set { }
  }

  @available(macOS 99.0, *)
  var alwaysAvailableProperty2: CInt {
    // expected-warning@-1 {{property 'alwaysAvailableProperty2' does not match the declaration in the header because it is only available in macOS 99.0 or newer}}
    get { 0 }
    set { }
  }
}

@implementation @_cdecl("macOS99CDeclFunc1")
func macOS99CDeclFunc1(_: Int32) { }
// expected-warning@-1 {{global function 'macOS99CDeclFunc1' does not match the declaration in the header because it must be only available in macOS 99.0 or newer}}

@available(macOS 99.0, *)
@implementation @_cdecl("macOS99CDeclFunc2")
func macOS99CDeclFunc2(_: Int32) { }

@available(macOS 100.0, *)
@implementation @_cdecl("macOS99CDeclFunc3")
func macOS99CDeclFunc3(_: Int32) { }
// expected-warning@-1 {{global function 'macOS99CDeclFunc3' does not match the declaration in the header because it is only available in macOS 100.0 or newer}}

@implementation @_cdecl("macOSUnavailableCDeclFunc1")
func macOSUnavailableCDeclFunc1(_: Int32) { }
// expected-warning@-1 {{global function 'macOSUnavailableCDeclFunc1' does not match the declaration in the header because it must be unavailable in macOS}}

@available(macOS 99.0, *)
@implementation @_cdecl("alwaysAvailableCDeclFunc1")
func alwaysAvailableCDeclFunc1(_: Int32) { }
// expected-warning@-1 {{global function 'alwaysAvailableCDeclFunc1' does not match the declaration in the header because it is only available in macOS 99.0 or newer}}

@implementation @c
func macOS99CFunc1(_: Int32) { }
// expected-warning@-1 {{global function 'macOS99CFunc1' does not match the declaration in the header because it must be only available in macOS 99.0 or newer}}

@available(macOS 99.0, *)
@implementation @c
func macOS99CFunc2(_: Int32) { }

@available(macOS 100.0, *)
@implementation @c
func macOS99CFunc3(_: Int32) { }
// expected-warning@-1 {{global function 'macOS99CFunc3' does not match the declaration in the header because it is only available in macOS 100.0 or newer}}

@implementation @c
func macOSUnavailableCFunc1(_: Int32) { }
// expected-warning@-1 {{global function 'macOSUnavailableCFunc1' does not match the declaration in the header because it must be unavailable in macOS}}

@available(macOS 99.0, *)
@implementation @c
func alwaysAvailableCFunc1(_: Int32) { }
// expected-warning@-1 {{global function 'alwaysAvailableCFunc1' does not match the declaration in the header because it is only available in macOS 99.0 or newer}}
