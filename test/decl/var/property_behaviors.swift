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

  wrapped1 = "Hello" // expected-error{{cannot assign value of type 'String' to type 'Wrapper<Int>'}}
}

func testImplicitWrapperType() {
  var wrapped1 by Wrapper = .init(value: 5)

  wrapped1 = "Hello" // expected-error{{cannot assign value of type 'String' to type 'Wrapper<Int>'}}
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
