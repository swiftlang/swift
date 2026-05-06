// RUN: %empty-directory(%t)
// RUN: %target-run-simple-swift

// REQUIRES: executable_test

import StdlibUnittest

struct Foo: ~Copyable {
  func idk() -> String {
    "idk"
  }

  deinit {
    print("foo")
  }
}

let suite = TestSuite("UniqueBox")

if #available(SwiftStdlib 6.4, *) {
suite.test("Basic") {
  var intBox = UniqueBox(123)

  expectEqual(intBox.value, 123)

  intBox.value += 321

  expectEqual(intBox.value, 444)
}
}

if #available(SwiftStdlib 6.4, *) {
suite.test("Span") {
  let fooBox = UniqueBox(Foo())

  let fooSpan = fooBox.span

  expectEqual(fooSpan.count, 1)
  expectEqual(fooSpan[0].idk(), "idk")
}
}

if #available(SwiftStdlib 6.4, *) {
suite.test("MutableSpan") {
  var intBox = UniqueBox(123)

  let intSpan = intBox.mutableSpan

  expectEqual(intSpan.count, 1)
  expectEqual(intSpan[0], 123)
}
}

if #available(SwiftStdlib 6.4, *) {
suite.test("Clone") {
  var intBox = UniqueBox(8)
  var cloneIntBox = intBox.clone()

  intBox.value += 8
  cloneIntBox.value -= 8

  expectEqual(intBox.value, 16)
  expectEqual(cloneIntBox.value, 0)
}
}

runAllTests()
