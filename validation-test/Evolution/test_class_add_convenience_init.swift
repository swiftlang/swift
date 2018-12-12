// RUN: %target-resilience-test
// REQUIRES: executable_test

import StdlibUnittest
import class_add_convenience_init

var AddConvenienceInitTest = TestSuite("AddConvenienceInit")

class Hobbit : AddConvenienceInit {
  public let name: String = "Bilbo"
}

AddConvenienceInitTest.test("AddConvenienceInit") {
  let t = Hobbit(z: 4)

  expectEqual("Bilbo", t.name)
  expectEqual(18, t.age)
}

runAllTests()
