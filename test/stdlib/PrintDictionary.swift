// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest


let PrintTests = TestSuite("PrintDictionary")

PrintTests.test("Printable") {
  expectPrinted("[:]", [String: Int]())
  expectDebugPrinted("[:]", [String: Int]())
  
  expectPrinted("[\"aaa\": 1]", ["aaa": 1])
  expectDebugPrinted("[\"aaa\": 1]", ["aaa": 1])
  
  let d0 = ["aaa": 1, "bbb": 2]
  expectPrinted(expectedOneOf: ["[\"aaa\": 1, \"bbb\": 2]",
    "[\"bbb\": 2, \"aaa\": 1]"], d0)
  expectDebugPrinted(expectedOneOf: ["[\"aaa\": 1, \"bbb\": 2]",
    "[\"bbb\": 2, \"aaa\": 1]"], d0)
  
  let d1 = ["aaa": "bbb"]
  expectPrinted("[\"aaa\": \"bbb\"]", d1)
  expectDebugPrinted("[\"aaa\": \"bbb\"]", d1)
}

runAllTests()
