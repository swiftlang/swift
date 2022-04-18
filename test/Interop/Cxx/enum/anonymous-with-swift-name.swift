// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -enable-experimental-cxx-interop)

// REQUIRES: executable_test

import AnonymousWithSwiftName
import StdlibUnittest

var AnonymousEnumsTestSuite = TestSuite("Anonymous Enums With Swift Name")

AnonymousEnumsTestSuite.test("SOME_OPTIONS") {
  let red: SOColorMask = .red
  let green = SOColorMask.green
  let blue = .blue as SOColorMask
  let all: SOColorMask = .all

  expectEqual(red.rawValue, 2)
  expectEqual(green.rawValue, 4)
  expectEqual(blue.rawValue, 8)
   expectEqual(all.rawValue, ~CUnsignedInt(0))
}

AnonymousEnumsTestSuite.test("CF_OPTIONS") {
  let red: SOColorMask = .red
  let green = SOColorMask.green
  let blue = .blue as SOColorMask
  let all: SOColorMask = .all

  expectEqual(red.rawValue, 2)
  expectEqual(green.rawValue, 4)
  expectEqual(blue.rawValue, 8)
  expectEqual(all.rawValue, ~CUnsignedInt(0))
}

runAllTests()
