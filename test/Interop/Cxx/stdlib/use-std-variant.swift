// RUN: %target-run-simple-swift(-I %S/Inputs -cxx-interoperability-mode=upcoming-swift -Xcc -std=c++17)

// REQUIRES: executable_test

import StdlibUnittest
import StdVariant
import CxxStdlib

var StdVariantTestSuite = TestSuite("StdVariant")

StdVariantTestSuite.test("index()") {
  let variantInt = getStdVariantInt()
  expectEqual(variantInt.index(), 0)

  let variantString = getStdVariantString()
  expectEqual(variantString.index(), 1)
  
  let variantNonCopyable = getStdVariantNonCopyable()
  expectEqual(variantNonCopyable.index(), 0)
}

runAllTests()
