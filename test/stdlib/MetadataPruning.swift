// RUN: %target-run-simple-swift
// RUN: %target-run-simple-swift(-Xfrontend -disable-reflection-metadata)
// RUN: %target-run-simple-swift(-Xfrontend -reflection-metadata-for-debugger-only)
// RUN: %target-run-simple-swift(-Xfrontend -disable-reflection-names)
//
// REQUIRES: executable_test

import StdlibUnittest

var tests = TestSuite("MetadataPruning")

struct TestStruct {
  var int = 0
  var double = 0.0
  var bool = false
}

struct GenericStruct<T> {
  var int = 0
  var first: T
  var second: T
}

enum TestEnum {
  case one
  case two
  case three(TestStruct)
}

class BaseClass {
  var superInt = 0
  init() {}
}

class TestClass: BaseClass {
  var int = 0
  var double = 0.0
  var bool = false
  override init() {}
}

class TestSubclass: TestClass {
  var strings: [String] = []
  override init() {}
}

class GenericClass<T, U>: BaseClass {
  var first: T
  var second: U

  init(_ t: T, _ u: U) {
    self.first = t
    self.second = u
  }
}

class GenericSubclass<V, W>: GenericClass<V, Bool> {
  var third: W

  init(_ v: V, _ w: W) {
    self.third = w
    super.init(v, false)
  }
}

class OwnershipTestClass: BaseClass {
  weak var test1: TestClass?
  unowned var test2: TestClass
  unowned(unsafe) var test3: TestClass

  init(_ t: TestClass) {
    self.test1 = t
    self.test2 = t
    self.test3 = t
  }
}

struct ContainsObject {
  var obj: TestClass
}

#if _runtime(_ObjC)
import Foundation

class NSObjectSubclass: NSObject {
  var point: (Double, Double)

  init(x: Double, y: Double) {
    self.point = (x, y)
  }
}

class EmptyNSObject: NSObject {}
#endif


func printAddress(_ obj: AnyObject) {
  print("\(obj) address: \(Unmanaged.passUnretained(obj).toOpaque())")
}

tests.test("Allocate types without metadata") {
  let testStruct = TestStruct()
  let genericStruct = GenericStruct<Double>(first: 1.3, second: 3.7)
  let testEnum = TestEnum.three(testStruct)
  let baseClass = BaseClass()
  let testClass = TestClass()
  let testSubclass = TestSubclass()
  let genericClass = GenericClass<Int, String>(5, "bla")
  let genericSubclass = GenericSubclass<Double, TestClass>(1.1, testClass)
  let ownershipTestClass = OwnershipTestClass(testClass)
  let containsObject = ContainsObject(obj: testClass)

  print("\(testStruct)")
  print("\(genericStruct)")
  print("\(testEnum)")
  printAddress(baseClass)
  printAddress(testClass)
  printAddress(testSubclass)
  printAddress(genericClass)
  printAddress(genericSubclass)
  printAddress(ownershipTestClass)
  print("\(containsObject)")

#if _runtime(_ObjC)
  let nsObjectSubclass = NSObjectSubclass(x: 1.2, y: 3.4)
  let emptyNSObject = EmptyNSObject()

  printAddress(nsObjectSubclass)
  printAddress(emptyNSObject)
#endif
}

runAllTests()
