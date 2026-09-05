// RUN: %target-run-simple-swift( \
// RUN:   -I %S/Inputs \
// RUN:   -cxx-interoperability-mode=default \
// RUN:   -Xfrontend -disable-availability-checking \
// RUN:   -Onone)
// RUN: %target-run-simple-swift( \
// RUN:   -I %S/Inputs \
// RUN:   -cxx-interoperability-mode=default \
// RUN:   -Xfrontend -disable-availability-checking \
// RUN:   -O)
//
// REQUIRES: executable_test

// A @convention(c) function value returning a foreign reference type returns
// it unretained (+0), whether it is called from Swift or from C++.

import StdlibUnittest
import ReferenceCounted

var ConventionCReturnsFRTTestSuite =
    TestSuite("@convention(c) functions returning foreign reference types")

@inline(never)
func blackHole<T>(_ _: T) {}

func passThrough(_ x: GlobalCount) -> GlobalCount { return x }

@inline(never)
func roundTrips(_ x: GlobalCount) {
  let closure: @convention(c) (GlobalCount) -> GlobalCount = { $0 }
  blackHole(closure(x))

  let functionReference: @convention(c) (GlobalCount) -> GlobalCount =
      passThrough
  blackHole(functionReference(x))

  let optional: @convention(c) (GlobalCount?) -> GlobalCount? = { $0 }
  blackHole(optional(x))
  blackHole(optional(nil))

  blackHole(callThroughGlobalCount(closure, x))
}

ConventionCReturnsFRTTestSuite.test("round trips are balanced") {
  expectEqual(globalCount, 0)
  roundTrips(GlobalCount.create())
  expectEqual(globalCount, 0)
}

runAllTests()
