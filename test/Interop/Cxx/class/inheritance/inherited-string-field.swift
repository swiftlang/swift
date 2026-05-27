// RUN: %target-run-simple-swift(-I %S/Inputs/ -Xfrontend -cxx-interoperability-mode=default)
//
// REQUIRES: executable_test

import StdlibUnittest
import CxxStdlib
import InheritedStringField

var InheritedStringFieldTestSuite = TestSuite("InheritedStringField")

InheritedStringFieldTestSuite.test("Access inherited std::string fields with long strings") {
  let obj = makeDerivedWithLongStrings()
  expectEqual(String(obj.type), "this has 22 characters")
  expectEqual(String(obj.data), "this has 23 characters.")
  expectEqual(obj.id, 42)
  expectEqual(String(obj.sauce), "this has 23 characters!")
}

InheritedStringFieldTestSuite.test("Access inherited std::string fields with short strings") {
  let obj = makeDerivedWithShortStrings()
  expectEqual(String(obj.type), "s")
  expectEqual(String(obj.data), "short")
  expectEqual(obj.id, 42)
  expectEqual(String(obj.sauce), "short!")
}

runAllTests()
