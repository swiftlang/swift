// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest


let PrintTests = TestSuite("PrintSet")
PrintTests.test("Printable") {
  expectPrinted("[]", Set<Int>())
  expectDebugPrinted("Set([])", Set<Int>())
  
  let s0 = Set<Int>([11, 22])
  expectPrinted(expectedOneOf: ["[11, 22]", "[22, 11]"], s0)
  expectDebugPrinted(expectedOneOf: ["Set([11, 22])",
    "Set([22, 11])"], s0)
  
  let s1 = Set<String>(["Hello", "world"])
  expectPrinted(expectedOneOf: ["[\"Hello\", \"world\"]",
    "[\"world\", \"Hello\"]"], s1)
  expectDebugPrinted(expectedOneOf: ["Set([\"Hello\", \"world\"])",
    "Set([\"world\", \"Hello\"])"], s1)
}

runAllTests()
