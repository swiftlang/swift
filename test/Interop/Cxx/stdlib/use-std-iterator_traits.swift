// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -enable-cxx-interop)
//
// REQUIRES: executable_test
//
// Enable this everywhere once we have a solution for modularizing libstdc++: rdar://87654514
// REQUIRES: OS=macosx

import StdlibUnittest
import StdIteratorTraits
import std.iterator_traits

var StdIteratorTraitsTestSuite = TestSuite("StdIteratorTraits")

extension IteratorTraits : RandomAccessIteractor {
  public var inputIt: Int { 0 }
  public var numberToAdvance: Int { 1 }
}

StdIteratorTraitsTestSuite.test("init") {
    let a = [12, 13, 14]
    let iterator = IteratorTraits()
    iterator.inputIt = a[0]
    iterator.numberToAdvance = 1
    expectEqual(iterator, 13)
}

runAllTests()
