// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -enable-cxx-interop)
//
// REQUIRES: executable_test
//
// Enable this everywhere once we have a solution for modularizing libstdc++: rdar://87654514
// REQUIRES: OS=macosx

import StdlibUnittest
import StdIteratorTraits
import std.iterator

var StdIteratorTraitsTestSuite = TestSuite("StdIteratorTraits")

extension IteratorTraits {
  public var inputIt: Int { 0 }
  public var numberToAdvance: Int { 0 }
}

StdIteratorTraitsTestSuite.test("init") {
    let iterator = IteratorTraits()
    expectTrue(iterator != nil)
}

runAllTests()
