// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest

// Also import modules which are used by StdlibUnittest internally. This
// workaround is needed to link all required libraries in case we compile
// StdlibUnittest with -sil-serialize-all.
#if _runtime(_ObjC)
import ObjectiveC
#endif

let PrintTests = TestSuite("PrintSet")
PrintTests.test("Printable") {
  expectPrinted("[]", Set<Int>())
  expectDebugPrinted("Set([])", Set<Int>())
  
  let s0 = Set<Int>([11, 22])
  expectPrinted(expectedOneOf: [ "[11, 22]", "[22, 11]" ], s0)
  expectDebugPrinted(expectedOneOf: [ "Set([11, 22])",
    "Set([22, 11])" ], s0)
  
  let s1 = Set<String>(["Hello", "world"])
  expectPrinted(expectedOneOf: [ "[\"Hello\", \"world\"]",
    "[\"world\", \"Hello\"]" ], s1)
  expectDebugPrinted(expectedOneOf: [ "Set([\"Hello\", \"world\"])",
    "Set([\"world\", \"Hello\"])" ], s1)
}

runAllTests()
