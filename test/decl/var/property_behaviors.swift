// RUN: %target-typecheck-verify-swift -debugger-support

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

struct NonGenericWrapper { }
// expected-error@-1{{property behavior type must have a single generic type parameter}}

struct TwoParameterWrapper<T, U> { }
// expected-error@-1{{property behavior type must have a single generic type parameter}}

func testBadWrapperTypes() {
  var wrapped1: Int by NonGenericWrapper
  var wrapped2: Int by TwoParameterWrapper
  var wrapped3: Int by TwoParameterWrapper<Int, Int>
  var wrapped4: Int by (Int) // FIXME: error about Int not being a behavior type
  var wrapped5: Int by Wrapper<Int> // FIXME: diagnose this consistently

  wrapped1 = 0
  wrapped2 = 0
  wrapped3 = 0
  wrapped4 = 0
  wrapped5 = 0
  _ = wrapped1
  _ = wrapped2
  _ = wrapped3
  _ = wrapped4
  _ = wrapped5
}

// ---------------------------------------------------------------------------
// Property behaviors as members
// ---------------------------------------------------------------------------
struct HasBehavior {
  var wrapped1: Int by Wrapper
}
