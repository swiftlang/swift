// RUN: %target-typecheck-verify-swift -module-name MyModule

// REQUIRES: OS=macosx

@available(*, unavailable)
func unavailable_foo() {} // expected-note {{'unavailable_foo()' has been explicitly marked unavailable here}}

func test() {
  unavailable_foo() // expected-error {{'unavailable_foo()' is unavailable}}
}

@available(*,unavailable,message: "use 'Int' instead")
struct NSUInteger {} // expected-note 3 {{explicitly marked unavailable here}}

struct Outer {
  @available(*,unavailable,message: "use 'UInt' instead")
  struct NSUInteger {} // expected-note 2 {{explicitly marked unavailable here}}
}

func foo(x : NSUInteger) { // expected-error {{'NSUInteger' is unavailable: use 'Int' instead}}
     let y : NSUInteger = 42 // expected-error {{'NSUInteger' is unavailable: use 'Int' instead}}

  let z : MyModule.NSUInteger = 42 // expected-error {{'NSUInteger' is unavailable: use 'Int' instead}}

  let z2 : Outer.NSUInteger = 42 // expected-error {{'NSUInteger' is unavailable: use 'UInt' instead}}  

  let z3 : MyModule.Outer.NSUInteger = 42 // expected-error {{'NSUInteger' is unavailable: use 'UInt' instead}}  
}

// Test preventing overrides of unavailable methods.
class ClassWithUnavailable {
  @available(*, unavailable)
  func doNotOverride() {} // expected-note {{'doNotOverride()' has been explicitly marked unavailable here}}

  // FIXME: extraneous diagnostic here
  @available(*, unavailable)
  init(int _: Int) {} // expected-note 2 {{'init(int:)' has been explicitly marked unavailable here}}

  convenience init(otherInt: Int) {
    self.init(int: otherInt) // expected-error {{'init(int:)' is unavailable}}
  }

  @available(*, unavailable)
  subscript (i: Int) -> Int { // expected-note{{'subscript(_:)' has been explicitly marked unavailable here}}
    return i
  }
}

class ClassWithOverride : ClassWithUnavailable {
  override func doNotOverride() {} // expected-error {{cannot override 'doNotOverride' which has been marked unavailable}}
}

func testInit() {
  ClassWithUnavailable(int: 0) // expected-error {{'init(int:)' is unavailable}} // expected-warning{{unused}}
}

func testSubscript(cwu: ClassWithUnavailable) {
  _ = cwu[5] // expected-error{{'subscript(_:)' is unavailable}}
}

/* FIXME 'nil == a' fails to type-check with a bogus error message
 * <rdar://problem/17540796>
func markUsed<T>(t: T) {}
func testString() {
  let a : String = "Hey"
  if a == nil {
    markUsed("nil")
  } else if nil == a {
    markUsed("nil")
  }
  else {
    markUsed("not nil")
  }
}
 */

// Test preventing protocol witnesses for unavailable requirements
@objc
protocol ProtocolWithRenamedRequirement {
  @available(*, unavailable, renamed: "new(bar:)")
  @objc optional func old(foo: Int) // expected-note{{'old(foo:)' has been explicitly marked unavailable here}}
  func new(bar: Int)
}

class ClassWithGoodWitness : ProtocolWithRenamedRequirement {
  @objc func new(bar: Int) {}
}

class ClassWithBadWitness : ProtocolWithRenamedRequirement {
  @objc func old(foo: Int) {} // expected-error{{'old(foo:)' has been renamed to 'new(bar:)'}}
  @objc func new(bar: Int) {}
}

@available(OSX, unavailable)
let unavailableOnOSX: Int = 0 // expected-note{{explicitly marked unavailable here}}
@available(iOS, unavailable)
let unavailableOniOS: Int = 0
@available(iOS, unavailable) @available(OSX, unavailable)
let unavailableOnBothA: Int = 0 // expected-note{{explicitly marked unavailable here}}
@available(OSX, unavailable) @available(iOS, unavailable)
let unavailableOnBothB: Int = 0 // expected-note{{explicitly marked unavailable here}}

@available(OSX, unavailable)
typealias UnavailableOnOSX = Int // expected-note{{explicitly marked unavailable here}}
@available(iOS, unavailable)
typealias UnavailableOniOS = Int
@available(iOS, unavailable) @available(OSX, unavailable)
typealias UnavailableOnBothA = Int // expected-note{{explicitly marked unavailable here}}
@available(OSX, unavailable) @available(iOS, unavailable)
typealias UnavailableOnBothB = Int // expected-note{{explicitly marked unavailable here}}

@available(macOS, unavailable)
let unavailableOnMacOS: Int = 0 // expected-note{{explicitly marked unavailable here}}
@available(macOS, unavailable)
typealias UnavailableOnMacOS = Int // expected-note{{explicitly marked unavailable here}}

@available(OSXApplicationExtension, unavailable)
let unavailableOnOSXAppExt: Int = 0
@available(macOSApplicationExtension, unavailable)
let unavailableOnMacOSAppExt: Int = 0

@available(OSXApplicationExtension, unavailable)
typealias UnavailableOnOSXAppExt = Int
@available(macOSApplicationExtension, unavailable)
typealias UnavailableOnMacOSAppExt = Int

func testPlatforms() {
  _ = unavailableOnOSX // expected-error{{unavailable}}
  _ = unavailableOniOS
  _ = unavailableOnBothA // expected-error{{unavailable}}
  _ = unavailableOnBothB // expected-error{{unavailable}}
  _ = unavailableOnMacOS // expected-error{{unavailable}}
  _ = unavailableOnOSXAppExt
  _ = unavailableOnMacOSAppExt

  let _: UnavailableOnOSX = 0 // expected-error{{unavailable}}
  let _: UnavailableOniOS = 0
  let _: UnavailableOnBothA = 0 // expected-error{{unavailable}}
  let _: UnavailableOnBothB = 0 // expected-error{{unavailable}}
  let _: UnavailableOnMacOS = 0 // expected-error{{unavailable}}
  let _: UnavailableOnOSXAppExt = 0
  let _: UnavailableOnMacOSAppExt = 0
}

struct VarToFunc {
  @available(*, unavailable, renamed: "function()")
  var variable: Int // expected-note 2 {{explicitly marked unavailable here}}

  @available(*, unavailable, renamed: "function()")
  func oldFunction() -> Int { return 42 } // expected-note 2 {{explicitly marked unavailable here}}

  func function() -> Int {
    _ = variable // expected-error{{'variable' has been renamed to 'function()'}}{{9-17=function()}}
    _ = oldFunction() //expected-error{{'oldFunction()' has been renamed to 'function()'}}{{9-20=function}}
    _ = oldFunction // expected-error{{'oldFunction()' has been renamed to 'function()'}} {{9-20=function}}

    return 42
  }

  mutating func testAssignment() {
    // This is nonsense, but someone shouldn't be using 'renamed' for this
    // anyway. Just make sure we don't crash or anything.
    variable = 2 // expected-error {{'variable' has been renamed to 'function()'}} {{5-13=function()}}
  }
}

