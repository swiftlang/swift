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

StringDescribingTestSuite.test("String(describing:) shouldn't include local discriminator") {
  class LocalType {}
  let t = LocalType()
  expectEqual(String(describing: type(of: t)), "LocalType")
}

StringDescribingTestSuite.test("String(reflecting:) should uniquely qualify type name") {
  class LocalType {}
  let t = LocalType()
  let representation = String(reflecting: type(of: t))

  expectEqual(representation.prefix(6), "main.(")
  expectEqual(representation.suffix(11), ").LocalType")
}

runAllTests()
