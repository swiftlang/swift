// RUN: %target-run-simple-swift(-O)
// REQUIRES: executable_test
// REQUIRES: reflection

import StdlibUnittest

let testSuite = TestSuite("ArraySlice")

@inline(never)
func getArray() -> [String] {
  return [_opaqueIdentity("a very long string which is is not inlined")]
}

@inline(never)
func testSliceLiverange() -> String {
  let a = getArray()
  let s = a[0..<1]
  return s[0]
}

testSuite.test("liverange") {
  expectEqual(testSliceLiverange(), "a very long string which is is not inlined")
}

runAllTests()
