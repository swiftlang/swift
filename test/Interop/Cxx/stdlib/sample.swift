// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -enable-experimental-cxx-interop)
//
// REQUIRES: executable_test
//
// Enable this everywhere once we have a solution for modularizing libstdc++: rdar://87654514
// REQUIRES: OS=macosx

import StdlibUnittest
import std.map
import Sample

var SampleTestSuite = TestSuite("Sample")

actor X {
    var counter: Counter

    init(counter: Counter) {
        self.counter = counter
    }

    func test() {
        self.counter = self.counter.successor() // this is ++
    }
}

SampleTestSuite.test("test") {
}

runAllTests()
