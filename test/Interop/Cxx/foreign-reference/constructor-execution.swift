// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -enable-experimental-cxx-interop -Xfrontend -disable-availability-checking)
// REQUIRES: executable_test

import Constructor
import StdlibUnittest

var ForeignRefCtorSuite = TestSuite("ForeignRef Ctor")

ForeignRefCtorSuite.test("ImportStaticFactoryAsInitializer") {
  let x = ImportWithCtor()
  expectEqual(x.param1, 0)
  expectEqual(x.param2, 0)
  let y = x
  let z = ImportWithCtor(1)
  expectEqual(z.param1, 1)
  expectEqual(z.param2, 0)
  let z2 = ImportWithCtor(2, 3)
  expectEqual(z2.param1, 2)
  expectEqual(z2.param2, 3)
  let z3 = ImportWithCtor(2, 3, 4)
  expectEqual(z3.param1, 2)
  expectEqual(z3.param2, 3)
}

runAllTests()
