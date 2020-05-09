// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest

private class Foo {}
class Bar {}

var StringDescribingTestSuite = TestSuite("StringDescribing")

StringDescribingTestSuite.test("String(describing:) shouldn't include extra stuff if the class is private") {
  expectEqual(String(describing: Foo.self), "Foo")
  expectEqual(String(describing: Bar.self), "Bar")
}

StringDescribingTestSuite.test("String(reflecting:) should include extra stuff if the class is private") {
  expectEqual(String(reflecting: Bar.self), "main.Bar")

  let privateName = String(reflecting: Foo.self)

  expectEqual(privateName.prefix(6), "main.(")
  expectEqual(privateName.suffix(5), ").Foo")
}

runAllTests()
