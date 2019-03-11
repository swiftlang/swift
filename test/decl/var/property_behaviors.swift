// RUN: %target-typecheck-verify-swift

struct Wrapper<T> {
  var value: T
}

// ---------------------------------------------------------------------------
// Parsing
// ---------------------------------------------------------------------------

func testParsing() {
  let wrapped1: Int by Wrapper
  let wrapped2: Int by Wrapper = Wrapper(value: 5)
  let wrapped3 by Wrapper = Wrapper(value: 5)

  _ = wrapped1
  _ = wrapped2
  _ = wrapped3
}

func testParseError() {
  let (a, b): (Int, Int) by Wrapper // expected-error{{property behavior can only by written on a single-variable pattern}}
  let (c, d): (Int, Int) by // expected-error{{expected behavior type after 'by'}}

  _ = a
  _ = b
  _ = c
  _ = d
}

// ---------------------------------------------------------------------------
// Type formation
// ---------------------------------------------------------------------------
func testExplicitWrapperType() {
  var wrapped1: Int by Wrapper

  wrapped1 = "Hello" // expected-error{{cannot assign value of type 'String' to type 'Int'}}
}

func testImplicitWrapperType() {
  var wrapped1 by Wrapper = .init(value: 5)

  wrapped1 = "Hello" // expected-error{{cannot assign value of type 'String' to type 'Int'}}
}

struct NonGenericWrapper { }
struct TwoParameterWrapper<T, U> { } // expected-note{{generic struct 'TwoParameterWrapper' declared here}}

func testBadWrapperTypes() {
  let wrapped1: Int by NonGenericWrapper // expected-error{{property behavior must name a generic type without any generic arguments}}
  let wrapped2: Int by TwoParameterWrapper // expected-error{{property behavior must refer to a single-parameter generic type}}
  let wrapped3: Int by TwoParameterWrapper<Int, Int> // expected-error{{property behavior must name a generic type without any generic arguments}}
  let wrapped4: Int by (Int) // expected-error{{property behavior must name a generic type without any generic arguments}}

  _ = wrapped1
  _ = wrapped2
  _ = wrapped3
  _ = wrapped4
}

// ---------------------------------------------------------------------------
// Initialization via initial value
// ---------------------------------------------------------------------------
struct WrapperWithInitialValue<T> {
  var value: T
  init(initialValue: T) { value = initialValue }
}

func testExplicitInitialValue() {
  var wrapped1: Int by WrapperWithInitialValue = 5

  wrapped1 = "Hello" // expected-error{{cannot assign value of type 'String' to type 'Int'}}
}

func testImplicitInitialValue() {
  var wrapped1 by WrapperWithInitialValue = 5

  wrapped1 = "Hello" // expected-error{{cannot assign value of type 'String' to type 'Int'}}
}

// ---------------------------------------------------------------------------
// Unwrap suppression
// ---------------------------------------------------------------------------
func testSuppressUnwrap() {
  var wrapped1 by WrapperWithInitialValue = 5

  ^wrapped1 = "Hello" // expected-error{{cannot assign value of type 'String' to type 'WrapperWithInitialValue<Int>'}}
  let z  = ^wrapped1
  let _: Double = z // expected-error{{cannot convert value of type 'WrapperWithInitialValue<Int>' to specified type 'Double'}}

  // FIXME wrong type for ^wrapped1
  let _: Double = ^wrapped1 // expected-error{{cannot convert value of type 'Int' to specified type 'Double'}}
}

// ---------------------------------------------------------------------------
// Behaviors as members of types
// ---------------------------------------------------------------------------
struct HasWrapper {
  var wrapped by WrapperWithInitialValue = 5

  func testReading() -> Int {
    return wrapped + self.wrapped
  }

  mutating func testMutating(i: Int) {
    wrapped += i
    self.wrapped += i
  }

  mutating func testSuppression(i: Int) {
    ^wrapped = WrapperWithInitialValue(initialValue: 17)
    ^(self.wrapped) = WrapperWithInitialValue(initialValue: 17)
  }
}

struct HasStaticWrapper {
  static var wrapped by WrapperWithInitialValue = 5

  static func testReading() -> Int {
    return wrapped + HasStaticWrapper.wrapped
  }

  static func testMutating(i: Int) {
    wrapped += i
    HasStaticWrapper.wrapped += i
  }

  static func testSuppression(i: Int) {
    ^wrapped = WrapperWithInitialValue(initialValue: 17)
    ^(HasStaticWrapper.wrapped) = WrapperWithInitialValue(initialValue: 17)
  }
}
