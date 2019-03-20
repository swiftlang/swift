// RUN: %target-typecheck-verify-swift -debugger-support

@propertyBehavior
struct Wrapper<T> {
  var value: T
}

// ---------------------------------------------------------------------------
// Parsing
// ---------------------------------------------------------------------------

func testParsing() {
  var wrapped1: Int by Wrapper
  var wrapped2: Int by Wrapper = 5
  var wrapped3 by Wrapper = 5

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

@propertyBehavior
struct NonGenericWrapper { }
// expected-error@-1{{property behavior type must have a single generic type parameter}}

@propertyBehavior
struct TwoParameterWrapper<T, U> { }
// expected-error@-1{{property behavior type must have a single generic type parameter}}

@propertyBehavior
struct MissingValue<T> { }
// expected-error@-1{{property behavior type 'MissingValue' does not contain a non-static property named 'value'}}

// expected-note@+1{{type 'NotABehavior<T>' must have the attribute '@propertyBehavior' to be used as a property behavior}}{{1-1=@propertyBehavior}}
struct NotABehavior<T> {
  var value: T
}

// expected-error@+1{{'@propertyBehavior' attribute cannot be applied to this declaration}}
@propertyBehavior
protocol CannotBeABehavior {
  associatedtype Value
  var value: Value { get set }
}

func testBadWrapperTypes() {
  var wrapped1: Int by NonGenericWrapper // expected-error{{property behavior type 'NonGenericWrapper' must not provide generic arguments}}
  var wrapped2: Int by TwoParameterWrapper
  var wrapped3: Int by TwoParameterWrapper<Int, Int> // expected-error{{property behavior type 'TwoParameterWrapper<Int, Int>' must not provide generic arguments}}
  var wrapped4: Int by (Int) // expected-error{{use of non-behavior type 'Int' as a property behavior}}
  var wrapped5: Int by Wrapper<Int> // expected-error{{property behavior type 'Wrapper<Int>' must not provide generic arguments}}
  var wrapped6: Int by NotABehavior // expected-error{{use of non-behavior type 'NotABehavior' as a property behavior}}

  wrapped1 = 0
  wrapped2 = 0
  wrapped3 = 0
  wrapped4 = 0
  wrapped5 = 0
  wrapped6 = 0
  _ = wrapped1
  _ = wrapped2
  _ = wrapped3
  _ = wrapped4
  _ = wrapped5
  _ = wrapped6
}

// ---------------------------------------------------------------------------
// Property behaviors as members
// ---------------------------------------------------------------------------
struct HasBehavior {
  var wrapped1: Int by Wrapper
}
