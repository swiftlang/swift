// RUN: %target-run-simple-swift(-I %S/Inputs/ -Xfrontend -enable-experimental-cxx-interop)
//
// REQUIRES: executable_test
//

import StdlibUnittest
import AmbiguousMethods

var CxxAmbiguousMethodTestSuite = TestSuite("CxxAmbiguousMethods")

// It's important to check that both calling the const version first
// and the mutable version first pass. This helps confirm that the lookup
// table is being properly seeded.
CxxAmbiguousMethodTestSuite.test("[Const First] numberOfMutableMethodsCalled: () -> Int") {
  var instance = HasAmbiguousMethods()

  // Soundness check. Make sure we start at 0
  // and that calling numberOfMutableMethodsCalled doesn't change
  // the count
  expectEqual(0, instance.numberOfMutableMethodsCalled())
  expectEqual(0, instance.numberOfMutableMethodsCalled())

  // Check that mutable version _does_ change the mutable call count
  expectEqual(0, instance.numberOfMutableMethodsCalled())
  expectEqual(1, instance.numberOfMutableMethodsCalledMutating())
}

CxxAmbiguousMethodTestSuite.test("[Mutable First] numberOfMutableMethodsCalled: () -> Int") {
  var instance = HasAmbiguousMethods()

  // Call mutable first
  expectEqual(1, instance.numberOfMutableMethodsCalledMutating())
  expectEqual(1, instance.numberOfMutableMethodsCalled())

}

CxxAmbiguousMethodTestSuite.test("Basic Increment: (Int) -> Int") {
  var instance = HasAmbiguousMethods()
  var a: Int32 = 0

  // Soundness check. Make sure we start at 0
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

  // Soundness check. Make sure we start at 0
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

  // Soundness check. Make sure we start at 0
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
