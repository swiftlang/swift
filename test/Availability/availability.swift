// RUN: %target-typecheck-verify-swift -parse-as-library -module-name MyModule

@available(*, unavailable)
func unavailable_foo() {} // expected-note {{'unavailable_foo()' has been explicitly marked unavailable here}}

@_unavailableInEmbedded // no-op without -enable-experimental-feature Embedded
public func unavailable_in_embedded() { }

func test() {
  unavailable_foo() // expected-error {{'unavailable_foo()' is unavailable}}
  unavailable_in_embedded() // ok
}

@available(*,unavailable,message: "use 'Int' instead")
struct NSUInteger {} // expected-note 3 {{explicitly marked unavailable here}}

struct Outer {
  @available(*,unavailable,message: "use 'UInt' instead")
  struct NSUInteger {} // expected-note 2 {{explicitly marked unavailable here}}
}

func foo(x : NSUInteger) { // expected-error {{'NSUInteger' is unavailable: use 'Int' instead}}
     let y : NSUInteger = 42 // expected-error {{'NSUInteger' is unavailable: use 'Int' instead}}
     // expected-error@-1 {{cannot convert value of type 'Int' to specified type 'NSUInteger'}}

  let z : MyModule.NSUInteger = 42 // expected-error {{'NSUInteger' is unavailable: use 'Int' instead}}
  // expected-error@-1 {{cannot convert value of type 'Int' to specified type 'NSUInteger'}}

  let z2 : Outer.NSUInteger = 42 // expected-error {{'NSUInteger' is unavailable: use 'UInt' instead}}
  // expected-error@-1 {{cannot convert value of type 'Int' to specified type 'Outer.NSUInteger'}}

  let z3 : MyModule.Outer.NSUInteger = 42 // expected-error {{'NSUInteger' is unavailable: use 'UInt' instead}}
  // expected-error@-1 {{cannot convert value of type 'Int' to specified type 'Outer.NSUInteger'}}
}

struct VarToFunc {
  @available(*, unavailable, renamed: "function()")
  var variable: Int { // expected-note 2 {{explicitly marked unavailable here}}
    get { 0 }
    set {}
  }

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

struct DeferBody {
  func foo() {
    enum No: Error {
      case no
    }

    defer {
      do {
        throw No.no
      } catch No.no {
      } catch {
      }
    }
    _ = ()
  }

  func bar() {
    @available(*, unavailable)
    enum No: Error { // expected-note 2 {{'No' has been explicitly marked unavailable here}}
      case no
    }
    do {
      throw No.no
      // expected-error@-1 {{'No' is unavailable}}
    } catch No.no {} catch _ {}
    // expected-error@-1 {{'No' is unavailable}}
  }
}

// Ensure that we do not select the unavailable failable init as the
// only solution and then fail to typecheck.
protocol P {}

class C : P {
  @available(swift, obsoleted: 4)
  public init?(_ c: C) {
  }

  public init<T : P>(_ c: T) {}
}

func f(c: C) {
  let _: C? = C(c)
}

// rdar://problem/60047439 - unable to disambiguate expression based on availability
func test_contextual_member_with_availability() {
  struct A {
    static var foo: A = A()
  }

  struct B {
    @available(*, unavailable, renamed: "bar")
    static var foo: B = B()
  }

  struct Test {
    init(_: A) {}
    init(_: B) {}
  }

  _ = Test(.foo) // Ok
}

@available(*, unavailable)
func unavailableFunction(_ x: Int) -> Bool { true } // expected-note {{'unavailableFunction' has been explicitly marked unavailable here}}

/// https://github.com/apple/swift/issues/55700
/// Availability checking not working in the `where` clause of a `for` loop
func f_55700(_ arr: [Int]) {
  for x in arr where unavailableFunction(x) {} // expected-error {{'unavailableFunction' is unavailable}}
}

// rdar://101814209
public struct Box<T> {
  @usableFromInline
  let value: T
}

@available(macOS, unavailable)
extension Box where T == Int {
  @usableFromInline
  init(value: T) {
    self.value = value
  }
}

@available(macOS, unavailable)
@_alwaysEmitIntoClient public func aeicFunction() {
  // Select the unavailable @usableFromInline init over the synthesized internal
  // memberwise initializer.
  _ = Box(value: 42)
}

// rdar://87403752 - ambiguity with member declared in unavailable extension
struct HasUnavailableExtesion {
}

@available(*, unavailable)
extension HasUnavailableExtesion {
  static var foo: Self = HasUnavailableExtesion()
}

func test_contextual_member_with_unavailable_extension() {
  struct A {
    static var foo: A = A()
  }

  struct Test {
    init(_: A) {}
    init(_: HasUnavailableExtesion) {}
  }

  _ = Test(.foo) // Ok `A.foo` since `foo` from `HasUnavailableExtesion` is unavailable
}
