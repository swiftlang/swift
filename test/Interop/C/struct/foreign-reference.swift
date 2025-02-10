// RUN: %target-run-simple-swift(-I %S/Inputs/ -Xfrontend -validate-tbd-against-ir=none -Xfrontend -experimental-c-foreign-reference-types -Onone -D NO_OPTIMIZATIONS)
// RUN: %target-run-simple-swift(-I %S/Inputs/ -Xfrontend -validate-tbd-against-ir=none -Xfrontend -experimental-c-foreign-reference-types -O)
//
// REQUIRES: executable_test
// TODO: This should work without ObjC interop in the future rdar://97497120
// REQUIRES: objc_interop

import StdlibUnittest
import ForeignReference

var ReferenceCountedTestSuite = TestSuite("Foreign reference types that have reference counts")

@inline(never)
public func blackHole<T>(_ _: T) {  }

ReferenceCountedTestSuite.test("Local") {
    var x = createLocalCount()
#if NO_OPTIMIZATIONS
    expectEqual(x.value, 2)
#endif

    let t = (x, x, x)
#if NO_OPTIMIZATIONS
    expectEqual(x.value, 5)
#endif
}

@inline(never)
func globalTest1() {
    var x = createGlobalCount()
    let t = (x, x, x)
#if NO_OPTIMIZATIONS
    expectEqual(globalCount, 4)
#endif
    blackHole(t)
}

@inline(never)
func globalTest2() {
    var x = createGlobalCount()
#if NO_OPTIMIZATIONS
    expectEqual(globalCount, 1)
#endif
}

ReferenceCountedTestSuite.test("Global") {
    expectEqual(globalCount, 0)
    globalTest1()
    globalTest2()
    expectEqual(globalCount, 0)
}

runAllTests()
