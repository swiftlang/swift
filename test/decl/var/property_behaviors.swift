// RUN: %target-typecheck-verify-swift

struct Wrapper<T> {
  var value: T

  init(initialValue: T) {
    self.value = initialValue
  }
}

// ---------------------------------------------------------------------------
// Parsing
// ---------------------------------------------------------------------------

func testParsing() {
  let wrapped1: Int by Wrapper
  let wrapped2: Int by Wrapper = 5
  let wrapped3 by Wrapper = 5

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
