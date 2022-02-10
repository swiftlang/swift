// RUN: %target-run-simple-swift(-I %S/Inputs/ -Xfrontend -enable-cxx-interop)
//
// REQUIRES: executable_test
//

import StdlibUnittest
import AmbiguousMethods

var CxxAmbiguousMethodTestSuite = TestSuite("CxxAmbiguousMethods")

CxxAmbiguousMethodTestSuite.test("numberOfMutableMethodsCalled: () -> Int") {
  var instance = HasAmbiguousMethods()

  // Sanity check. Make sure we start at 0
  expectEqual(0, instance.numberOfMutableMethodsCalled())

  // Make sure calling numberOfMutableMethodsCalled above didn't
  // change the count
  expectEqual(0, instance.numberOfMutableMethodsCalled())

  // Check that mutable version _does_ change the mutable call count
  expectEqual(1, instance.numberOfMutableMethodsCalledMutating())

  expectEqual(1, instance.numberOfMutableMethodsCalled())
}

CxxAmbiguousMethodTestSuite.test("Basic Increment: (Int) -> Int") {
  var instance = HasAmbiguousMethods()
  var a: Int32 = 0

  // Sanity check. Make sure we start at 0
  expectEqual(0, instance.numberOfMutableMethodsCalled())

  // Non mutable version should NOT change count
  a = instance.increment(a);
  expectEqual(1, a)
  expectEqual(0, instance.numberOfMutableMethodsCalled())

  a = instance.incrementMutating(a);
  expectEqual(2, a)
  expectEqual(1, instance.numberOfMutableMethodsCalled())
}

CxxAmbiguousMethodTestSuite.test("Out Param Increment: (Int, Int, inout Int) -> Void") {
  var instance = HasAmbiguousMethods()
  var out: Int32 = 0

  // Sanity check. Make sure we start at 0
  expectEqual(0, instance.numberOfMutableMethodsCalled())

  // Non mutable version should NOT change count
  instance.increment(0, 1, &out);
  expectEqual(1, out)
  expectEqual(0, instance.numberOfMutableMethodsCalled())

  instance.incrementMutating(5, 2, &out);
  expectEqual(7, out)
  expectEqual(1, instance.numberOfMutableMethodsCalled())
}

CxxAmbiguousMethodTestSuite.test("Inout Param Increment: (inout Int, Int) -> Void") {
  var instance = HasAmbiguousMethods()
  var inoutVal: Int32 = 0

  // Sanity check. Make sure we start at 0
  expectEqual(0, instance.numberOfMutableMethodsCalled())

  // Non mutable version should NOT change count
  instance.increment(&inoutVal, 1);
  expectEqual(1, inoutVal)
  expectEqual(0, instance.numberOfMutableMethodsCalled())

  instance.incrementMutating(&inoutVal, 2);
  expectEqual(3, inoutVal)
  expectEqual(1, instance.numberOfMutableMethodsCalled())
}

runAllTests()
