// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -enable-cxx-interop)
//
// REQUIRES: executable_test
//
// Enable this everywhere once we have a solution for modularizing libstdc++: rdar://87654514
// REQUIRES: OS=macosx

import StdlibUnittest
import StdIterator
import std.iterator
import StdVector
import std.vector

var StdIteratorTestSuite = TestSuite("StdIterator")

StdIteratorTestSuite.test("init") {
    var vector = Vector()
    var _1: CInt = 1
    vector.push_back(&_1)
    //ideally we should call vector.begin(), however we need to prevent a copy of self before vector.begin() is invoked
    //current workaround is to use beginMutating()
    let it = vector.beginMutating()
    expectEqual(it[0], 1)
}

StdIteratorTestSuite.test("next") {
    var vector = Vector()
    var _1: CInt = 1, _2: CInt = 2, _3: CInt = 3
    vector.push_back(&_1)
    vector.push_back(&_2)
    vector.push_back(&_3)
    let it = vector.beginMutating()
    let n1 = next(it, 2)
    expectEqual(n1[0], 3)
}

StdIteratorTestSuite.test("prev") {
    var vector = Vector()
    var _1: CInt = 1, _2: CInt = 2, _3: CInt = 3
    vector.push_back(&_1)
    vector.push_back(&_2)
    vector.push_back(&_3)
    let e = vector.endMutating()
    let n1 = prev(e, 2)
    expectEqual(n1[0], 2)
}

runAllTests()