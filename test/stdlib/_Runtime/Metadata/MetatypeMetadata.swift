// RUN: %target-run-simple-swift
// REQUIRES: executable_test

// UNSUPPORTED: freestanding

import StdlibUnittest
import _Runtime

let suite = TestSuite("MetatypeMetadata")

if #available(SwiftStdlib 5.9, *) {
  suite.test("Basic") {
    let metatype = Metadata(Int.Type.self).metatype
    let int = Metadata(Int.self)
    
    expectEqual(metatype.instanceMetadata, int)
  }
}

runAllTests()
