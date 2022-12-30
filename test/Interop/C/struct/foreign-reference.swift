// RUN: %target-run-simple-swift(-I %S/Inputs/ -Xfrontend -validate-tbd-against-ir=none -Xfrontend -experimental-c-foreign-reference-types)
//
// REQUIRES: executable_test
// TODO: This should work without ObjC interop in the future rdar://97497120
// REQUIRES: objc_interop
// REQUIRES: rdar101790203

import StdlibUnittest
import ForeignReference

var ReferenceCountedTestSuite = TestSuite("Foreign reference types that have reference counts")

@inline(never)
public func blackHole<T>(_ _: T) {  }

ReferenceCountedTestSuite.test("Local") {
    var x = createLocalCount()
    expectEqual(x.value, 6) // This is 6 because of "var x" "x.value" * 2 and "(x, x, x)".

    let t = (x, x, x)
    expectEqual(x.value, 5)
}

@inline(never)
func globalTest1() {
    var x = createGlobalCount()
    let t = (x, x, x)
    expectEqual(globalCount, 4)
    blackHole(t)
}

@inline(never)
func globalTest2() {
    var x = createGlobalCount()
    expectEqual(globalCount, 1)
}

ReferenceCountedTestSuite.test("Global") {
    expectEqual(globalCount, 0)
    globalTest1()
    globalTest2()
    expectEqual(globalCount, 0)
}

runAllTests()
