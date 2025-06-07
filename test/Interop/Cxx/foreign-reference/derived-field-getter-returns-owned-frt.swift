// RUN: %target-run-simple-swift(-I %S/Inputs/ -Xfrontend -enable-experimental-cxx-interop -Xcc -fignore-exceptions -Xfrontend -disable-availability-checking -O)
// RUN: %target-run-simple-swift(-I %S/Inputs/ -Xfrontend -enable-experimental-cxx-interop -Xcc -fignore-exceptions -Xfrontend -disable-availability-checking)
//
// REQUIRES: executable_test

import StdlibUnittest
import DerivedFieldGetterReturnsOwnedFRT

var FunctionsTestSuite = TestSuite("Calling functions in base foreign reference classes")

FunctionsTestSuite.test("base member FRT field accessing shared reference FRT") {
    let refScope = {
        let frt: RefCounted
        do {
            let base = BaseFieldFRT()
            frt = base.value
        }
        expectEqual(getLiveRefCountedCounter().pointee, 1)
        return frt.testVal
    }
    let p = refScope()
    expectEqual(p, 1)
    expectEqual(getLiveRefCountedCounter().pointee, 0)
}

FunctionsTestSuite.test("derived-to-base member FRT field accessing shared reference FRT") {
    let refScope = {
        let frt: RefCounted
        do {
            let derived = DerivedFieldFRT()
            frt = derived.value
        }
        expectEqual(getLiveRefCountedCounter().pointee, 1)
        return frt.testVal
    }
    let p = refScope()
    expectEqual(p, 1)
    expectEqual(getLiveRefCountedCounter().pointee, 0)
}

FunctionsTestSuite.test("derivedDerived-to-derived-to-base member FRT field accessing shared reference FRT") {
    let refScope = {
        let frt: RefCounted
        do {
            let derived = DerivedDerivedFieldFRT()
            frt = derived.value
        }
        expectEqual(getLiveRefCountedCounter().pointee, 1)
        return frt.testVal
    }
    let p = refScope()
    expectEqual(p, 1)
    expectEqual(getLiveRefCountedCounter().pointee, 0)
}

FunctionsTestSuite.test("base member FRT field setting shared reference FRT") {
    let frt = createRefCounted()
    var base = BaseFieldFRT()
    base.value = frt
    expectEqual(getLiveRefCountedCounter().pointee, 1)
}

FunctionsTestSuite.test("derived member FRT field setting shared reference FRT") {
    let frt = createRefCounted()
    var base = DerivedFieldFRT()
    base.value = frt
    expectEqual(getLiveRefCountedCounter().pointee, 1)
}

runAllTests()
