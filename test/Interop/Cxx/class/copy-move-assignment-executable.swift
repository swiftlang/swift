// RUN: %target-run-simple-swift(-I %S/Inputs/ -Xfrontend -enable-experimental-cxx-interop)
//
// Re-test with optimizations:
// RUN: %target-run-simple-swift(-I %S/Inputs/ -Xfrontend -enable-experimental-cxx-interop -O)
//
// REQUIRES: executable_test

import StdlibUnittest
import CopyMoveAssignment

var CxxCopyMoveAssignTestSuite = TestSuite("CxxCopyMoveAssignTestSuite")

@inline(never)
func takeValue<T>(_ x: T) {
    let _ = x
}

CxxCopyMoveAssignTestSuite.test("NonTrivialCopyAssign") {
    do {
        var instance = NonTrivialCopyAssign()
        expectEqual(0, instance.copyAssignCounter)
        let instance2 = NonTrivialCopyAssign()
        instance = instance2
        // `operator=` isn't called.
        expectEqual(0, instance.copyAssignCounter)
        takeValue(instance2)
    }
    // The number of constructors and destructors called for `NonTrivialCopyAssign` must be balanced.
    expectEqual(0, InstanceBalanceCounter.getCounterValue())
}

CxxCopyMoveAssignTestSuite.test("NonTrivialMoveAssign") {
    do {
        var instance = NonTrivialMoveAssign()
        expectEqual(0, instance.moveAssignCounter)
        instance = NonTrivialMoveAssign()
        // `operator=` isn't called.
        expectEqual(0, instance.moveAssignCounter)
    }
    // The number of constructors and destructors called for `NonTrivialCopyAssign` must be balanced.
    expectEqual(0, InstanceBalanceCounter.getCounterValue())
}

CxxCopyMoveAssignTestSuite.test("NonTrivialCopyAndCopyMoveAssign") {
    do {
        var instance = NonTrivialCopyAndCopyMoveAssign()
        expectEqual(0, instance.assignCounter)
        let instance2 = NonTrivialCopyAndCopyMoveAssign()
        instance = instance2
        // `operator=` isn't called.
        expectEqual(0, instance.assignCounter)
        takeValue(instance2)
    }
    // The number of constructors and destructors called for `NonTrivialCopyAndCopyMoveAssign` must be balanced.
    expectEqual(0, InstanceBalanceCounter.getCounterValue())
}

runAllTests()
