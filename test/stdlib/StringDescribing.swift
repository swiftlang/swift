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
  expectEqual(String(reflecting: Foo.self), "main.(Foo in _AE29BC3E71CF180B9604AA0071CCE6E8)")
}

runAllTests()
