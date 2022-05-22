// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -enable-experimental-cxx-interop)
//
// REQUIRES: executable_test
//
// Enable this everywhere once we have a solution for modularizing libstdc++: rdar://87654514
// REQUIRES: OS=macosx

import StdlibUnittest
import StdVector
import std.vector

var StdIteratorTestSuite = TestSuite("StdIterator")

StdIteratorTestSuite.test("init") {
    var vector = Vector()
    var _1: CInt = 1
    //There seems to be an issue when importing this method, where the const_ref is mapped with 
    //the correct typealias to be able to pass immutable values. related to: https://github.com/apple/swift/pull/41611
    vector.push_back(&_1)
    //ideally we should call vector.begin(), however we need to prevent a copy of self before vector.begin() is invoked
    //current workaround is to use beginMutating()
    let it = vector.beginMutating()
    expectEqual(it[0], 1)
}

StdIteratorTestSuite.test("advance") {
    var vector = Vector()
    var _1: CInt = 1, _2: CInt = 2, _3: CInt = 3
    vector.push_back(&_1)
    vector.push_back(&_2)
    vector.push_back(&_3)
    var it = vector.beginMutating()
    std.__1.advance(&it, 2)
    expectEqual(it[0], 3)
}

runAllTests()