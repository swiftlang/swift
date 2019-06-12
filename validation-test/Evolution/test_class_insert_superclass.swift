// RUN: %target-resilience-test
// REQUIRES: executable_test

import StdlibUnittest
import class_insert_superclass


var ClassInsertSuperclassTest = TestSuite("ClassInsertSuperclass")

class FirstDerived : FirstMiddle {
  func get2() -> String {
    return "\(get()) \(get())"
  }
}

ClassInsertSuperclassTest.test("First") {
  let t = FirstDerived(x: "foo")

  expectEqual("foo", t.get())
  expectEqual("foo foo", t.get2())
}

class SecondDerived : SecondMiddle {
  func get2() -> String {
    return "\(get()) \(get())"
  }
}

ClassInsertSuperclassTest.test("Second") {
  let t = SecondDerived(x: "foo")

  expectEqual("foo", t.get())
  expectEqual("foo foo", t.get2())
}

class ThirdDerived : GenericMiddle<String> {
  func get2() -> String {
    return "\(get()) \(get())"
  }
}

ClassInsertSuperclassTest.test("Third") {
  let t = ThirdDerived(x: "foo")

  expectEqual("foo", t.get())
  expectEqual("foo foo", t.get2())
}

runAllTests()
