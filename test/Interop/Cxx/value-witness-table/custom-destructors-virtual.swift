// RUN: %target-run-simple-swift(-I %S/Inputs -cxx-interoperability-mode=default)
// RUN: %target-run-simple-swift(-I %S/Inputs -cxx-interoperability-mode=default -O)
//
// REQUIRES: executable_test

import CustomDestructor
import StdlibUnittest

var CXXDestructorTestSuite = TestSuite("CXXDestructor")

func createTypeWithVirtualBaseAndDestructor(
  _ ptr: UnsafeMutablePointer<Int32>
) {
  _ = HasVirtualBaseAndDestructor(ptr)
}

func createTypeWithBaseWithVirtualDestructor(
  _ ptr: UnsafeMutablePointer<Int32>
) {
  _ = HasBaseWithVirtualDestructor(ptr)
}

func createTypeWithVirtualBaseWithVirtualDestructor(
  _ ptr: UnsafeMutablePointer<Int32>
) {
  _ = HasVirtualBaseWithVirtualDestructor(ptr)
}

CXXDestructorTestSuite.test("Virtual base and destructor") {
  var value: Int32 = 0
  createTypeWithVirtualBaseAndDestructor(&value)
  expectEqual(value, 42)
}

CXXDestructorTestSuite.test("Base with virtual destructor") {
  var value: Int32 = 0
  createTypeWithBaseWithVirtualDestructor(&value)
  expectEqual(value, 42)
}

CXXDestructorTestSuite.test("Virtual base with virtual destructor") {
  var value: Int32 = 0
  createTypeWithVirtualBaseWithVirtualDestructor(&value)
  expectEqual(value, 42)
}

CXXDestructorTestSuite.test("Type with virtual defaulted destructor") {
  _ = HasVirtualDefaultedDestructor()
}

runAllTests()
