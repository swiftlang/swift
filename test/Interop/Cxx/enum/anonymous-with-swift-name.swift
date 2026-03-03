// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -enable-experimental-cxx-interop)

// REQUIRES: executable_test

import AnonymousWithSwiftName
import StdlibUnittest

var AnonymousEnumsTestSuite = TestSuite("Anonymous Enums With Swift Name")

AnonymousEnumsTestSuite.test("CF_OPTIONS") {
  let red: CFColorMask = .red
  let green = CFColorMask.green
  let blue = .blue as CFColorMask
  let all: CFColorMask = .all

  expectEqual(red.rawValue, 2)
  expectEqual(green.rawValue, 4)
  expectEqual(blue.rawValue, 8)
  expectEqual(all.rawValue, ~CUnsignedInt(0))
}

AnonymousEnumsTestSuite.test("Parameter types") {
  let red: CFColorMask = .red
  let green = CFColorMask.green

  let blue = useCFColorMask(.blue)
  let all = useCFColorMask(.all)

  expectEqual(red, useCFColorMask(.red))
  expectEqual(green, useCFColorMask(.green))
  expectEqual(blue, .blue)
  expectEqual(all, .all)
}

AnonymousEnumsTestSuite.test("Computed properties") {
  let parent = ParentStruct()

  expectEqual(parent.colorProp, useCFColorMask(.red))
  parent.colorProp = .green
  expectEqual(parent.colorProp, useCFColorMask(.red))
}

runAllTests()
