// RUN: %target-parse-verify-swift

// REQUIRES: OS=macosx

@availability(*, unavailable)
func unavailable_foo() {} // expected-note {{'unavailable_foo()' has been explicitly marked unavailable here}}

func test() {
  unavailable_foo() // expected-error {{'unavailable_foo()' is unavailable}}
}

@availability(*,unavailable,message="use 'Int' instead")
struct NSUInteger {} // expected-note 2 {{explicitly marked unavailable here}}

func foo(x : NSUInteger) { // expected-error {{'NSUInteger' is unavailable: use 'Int' instead}}
     let y : NSUInteger = 42 // expected-error {{'NSUInteger' is unavailable: use 'Int' instead}}
}

// Test preventing overrides of unavailable methods.
class ClassWithUnavailable {
  @availability(*, unavailable)
  func doNotOverride() {}

  // FIXME: extraneous diagnostic here
  @availability(*, unavailable)
  init(int _: Int) {} // expected-note 2 {{'init(int:)' has been explicitly marked unavailable here}}

  convenience init(otherInt: Int) {
    self.init(int: otherInt) // expected-error {{'init(int:)' is unavailable}}
  }

  @availability(*, unavailable)
  subscript (i: Int) -> Int { // expected-note{{'subscript' has been explicitly marked unavailable here}}
    return i
  }
}

class ClassWithOverride : ClassWithUnavailable {
  override func doNotOverride() {} // expected-error {{cannot override 'doNotOverride' which has been marked unavailable}}
}

func testInit() {
  ClassWithUnavailable(int: 0) // expected-error {{'init(int:)' is unavailable}}
}

func testSuvscript(cwu: ClassWithUnavailable) {
  let _ = cwu[5] // expected-error{{'subscript' is unavailable}}
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

@availability(OSX, unavailable)
let unavailableOnOSX: Int = 0 // expected-note{{explicitly marked unavailable here}}
@availability(iOS, unavailable)
let unavailableOniOS: Int = 0
@availability(iOS, unavailable) @availability(OSX, unavailable)
let unavailableOnBothA: Int = 0 // expected-note{{explicitly marked unavailable here}}
@availability(OSX, unavailable)
let unavailableOnBothB: Int = 0 // expected-note{{explicitly marked unavailable here}}

@availability(OSX, unavailable)
typealias UnavailableOnOSX = Int // expected-note{{explicitly marked unavailable here}}
@availability(iOS, unavailable)
typealias UnavailableOniOS = Int
@availability(iOS, unavailable) @availability(OSX, unavailable)
typealias UnavailableOnBothA = Int // expected-note{{explicitly marked unavailable here}}
@availability(OSX, unavailable) @availability(iOS, unavailable)
typealias UnavailableOnBothB = Int // expected-note{{explicitly marked unavailable here}}

func testPlatforms() {
  _ = unavailableOnOSX // expected-error{{unavailable}}
  _ = unavailableOniOS
  _ = unavailableOnBothA // expected-error{{unavailable}}
  _ = unavailableOnBothB // expected-error{{unavailable}}

  let _: UnavailableOnOSX = 0 // expected-error{{unavailable}}
  let _: UnavailableOniOS = 0
  let _: UnavailableOnBothA = 0 // expected-error{{unavailable}}
  let _: UnavailableOnBothB = 0 // expected-error{{unavailable}}
}
