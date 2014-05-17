// RUN: %swift %s -verify

@availability(*, unavailable)
func unavailable_foo() {} // expected-note {{'unavailable_foo' has been explicitly marked unavailable here}}

func test() {
  unavailable_foo() // expected-error {{'unavailable_foo' is unavailable}}
}

// FIXME: This will be moved to the Foundation overlay
// once @availability is properly serialized.
@availability(*,unavailable,message="use 'Int' instead")
struct NSUInteger {}

func foo(x : NSUInteger) { // expected-error {{'NSUInteger' is unavailable: use 'Int' instead}}
     let y : NSUInteger = 42 // expected-error {{'NSUInteger' is unavailable: use 'Int' instead}}
}

// Test preventing overrides of unavailable methods.
class ClassWithUnavailable {
  @availability(*, unavailable)
  func doNotOverride() {}

  // FIXME: extraneous diagnostic here
  @availability(*, unavailable)
  init(int _: Int) {} // expected-note 3 {{'init' has been explicitly marked unavailable here}}

  convenience init(otherInt: Int) {
    self.init(int: otherInt) // expected-error {{'init' is unavailable}}
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
  ClassWithUnavailable(int: 0) // expected-error {{'init' is unavailable}}
}

func testSuvscript(cwu: ClassWithUnavailable) {
  let x = cwu[5] // expected-error{{'subscript' is unavailable}}
}

func testString() {
  let a : String = "Hey"
  if a == nil { // expected-error {{Cannot compare a String to nil}}
    println("nil")
  } else if nil == a {  // expected-error {{Cannot compare a String to nil}}
    println("nil")
  }
  else {
    println("not nil")
  }
}
