// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -enable-cxx-interop)
//
// REQUIRES: executable_test
//
// Enable this everywhere once we have a solution for modularizing libstdc++: rdar://87654514
// REQUIRES: OS=macosx

import StdlibUnittest
import StdUnorderedSet
import std.unordered_set

var StdUnorderedSetTestSuite = TestSuite("StdUnorderedSet")

StdUnorderedSetTestSuite.test("init") {
    let u = UnorderedSet()
    expectEqual(u.size(), 0)
    expectTrue(u.empty())
}

// TODO: No use of u.insert or u.begin because of constness ambiguity.
//       Also getValueFromInterator is used because we don't yet implement
//       operator OO_ArrowStar/BO_PtrMemI.

StdUnorderedSetTestSuite.test("insert") {
    var u = UnorderedSet()
    let _42: CInt = 42
    unambiguousInsert(&u, _42)
    expectEqual(u.size(), 1)
    expectFalse(u.empty())
    expectEqual(getValueFromInterator(u.cbegin()), 42)
}

StdUnorderedSetTestSuite.test("insertUnique") {
    let _1: CInt = 1, _2: CInt = 2, _3: CInt = 3
    var u = UnorderedSet()
    unambiguousInsert(&u, _1)
    unambiguousInsert(&u, _2)
    unambiguousInsert(&u, _3)

    expectEqual(u.size(), 3)
    expectFalse(u.empty())

    unambiguousInsert(&u, _3)
    expectEqual(u.size(), 3)
}

runAllTests()
