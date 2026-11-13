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
        // `operator=` is called.
        expectEqual(1, instance.copyAssignCounter)
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
        // `operator=` is called.
        expectEqual(1, instance.moveAssignCounter)
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
        // `operator=` is called.
        expectEqual(1, instance.assignCounter)
        takeValue(instance2)
    }
    // The number of constructors and destructors called for `NonTrivialCopyAndCopyMoveAssign` must be balanced.
    expectEqual(0, InstanceBalanceCounter.getCounterValue())
}

CxxCopyMoveAssignTestSuite.test("move assignment falls back to copy assignment") {
    do {
        var instance = NonTrivialCopyAssign()
        expectEqual(0, instance.copyAssignCounter)
        instance = NonTrivialCopyAssign()
        // The value is moved, but `NonTrivialCopyAssign` has no move `operator=`,
        // so the copy `operator=` is called.
        expectEqual(1, instance.copyAssignCounter)
    }
    // The number of constructors and destructors called for `NonTrivialCopyAssign` must be balanced.
    expectEqual(0, InstanceBalanceCounter.getCounterValue())
}

CxxCopyMoveAssignTestSuite.test("Optional<NonTrivialCopyAssign>") {
    do {
        var instance: NonTrivialCopyAssign? = NonTrivialCopyAssign()
        expectEqual(0, instance!.copyAssignCounter)
        let instance2: NonTrivialCopyAssign? = NonTrivialCopyAssign()
        instance = instance2
        // `operator=` is called.
        expectEqual(1, instance!.copyAssignCounter)
        takeValue(instance2)
    }
    // The number of constructors and destructors called for `NonTrivialCopyAssign` must be balanced.
    expectEqual(0, InstanceBalanceCounter.getCounterValue())
}

struct HoldsNonTrivialCopyAssign {
    var value = NonTrivialCopyAssign()
}

CxxCopyMoveAssignTestSuite.test("struct holding NonTrivialCopyAssign") {
    do {
        var instance = HoldsNonTrivialCopyAssign()
        expectEqual(0, instance.value.copyAssignCounter)
        let instance2 = HoldsNonTrivialCopyAssign()
        instance = instance2
        // `operator=` is called.
        expectEqual(1, instance.value.copyAssignCounter)
        takeValue(instance2)
    }
    // The number of constructors and destructors called for `NonTrivialCopyAssign` must be balanced.
    expectEqual(0, InstanceBalanceCounter.getCounterValue())
}

runAllTests()
