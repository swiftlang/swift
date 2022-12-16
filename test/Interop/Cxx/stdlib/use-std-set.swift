// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -enable-experimental-cxx-interop -Xfrontend -validate-tbd-against-ir=none)
//
// REQUIRES: executable_test
//
// Enable this everywhere once we have a solution for modularizing other C++ stdlibs: rdar://87654514
// REQUIRES: OS=macosx || OS=linux-gnu

import StdlibUnittest
import StdSet
import CxxStdlib
import Cxx

var StdSetTestSuite = TestSuite("StdSet")

extension SetOfCInt.const_iterator : UnsafeCxxInputIterator { }
extension SetOfCInt : CxxSequence { }

StdSetTestSuite.test("iterate") {
    let s = initSetOfCInt()
    var result = [CInt]()
    for x in s {
        result.append(x)
    }
    expectEqual(result[0], 1)
    expectEqual(result[1], 3)
    expectEqual(result[2], 5)
}

runAllTests()
