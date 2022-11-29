// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -enable-experimental-cxx-interop)
//
// REQUIRES: executable_test
//
// Enable this everywhere once we have a solution for modularizing libstdc++: rdar://87654514
// REQUIRES: OS=macosx

import StdlibUnittest
import StdVector
import CxxStdlib.vector

var StdVectorTestSuite = TestSuite("StdVector")

extension Vector : RandomAccessCollection {
  public var startIndex: Int { 0 }
  public var endIndex: Int { size() }
}

StdVectorTestSuite.test("init") {
    let v = Vector()
    expectEqual(v.size(), 0)
    expectTrue(v.empty())
}

StdVectorTestSuite.test("push back") {
    var v = Vector()
    var _42: CInt = 42
    v.push_back(&_42)
    expectEqual(v.size(), 1)
    expectFalse(v.empty())
    expectEqual(v[0], 42)
}

func fill(vector v: inout Vector) {
    var _1: CInt = 1, _2: CInt = 2, _3: CInt = 3
    v.push_back(&_1)
    v.push_back(&_2)
    v.push_back(&_3)
}

// TODO: in some configurations the stdlib emits a "initializeWithCopy" where the arguments
// have incorrect indirection: rdar://87728422 and rdar://87805795
// StdVectorTestSuite.test("for loop") {
//     var v = Vector()
//     fill(vector: &v)
//
//     var count: CInt = 1
//     for e in v {
//         expectEqual(e, count)
//         count += 1
//     }
//     expectEqual(count, 4)
// }

StdVectorTestSuite.test("map") {
    var v = Vector()
    fill(vector: &v)

    let a = v.map { $0 + 5 }
    expectEqual(a, [6, 7, 8])
}

runAllTests()
