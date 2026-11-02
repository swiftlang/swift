// RUN: %target-run-simple-swift(-I %S/Inputs -cxx-interoperability-mode=default)

// REQUIRES: executable_test
// REQUIRES: objc_interop

import CoreFoundation
import CFReturnsRetained
import StdlibUnittest

var CFReturnsRetainedTestSuite = TestSuite("CFReturnsRetained")

func makeString() -> CFString {
  return CFStringCreateMutable(nil, 0)
}

CFReturnsRetainedTestSuite.test("instance method") {
  let s = makeString()
  let h = CFHolder(str: Unmanaged.passUnretained(s))
  let start = CFGetRetainCount(s)
  for _ in 0..<10 { _ = h.copyString() }
  expectEqual(start, CFGetRetainCount(s))
  withExtendedLifetime(h) {}
}

CFReturnsRetainedTestSuite.test("static method") {
  let s = makeString()
  let start = CFGetRetainCount(s)
  for _ in 0..<10 { _ = CFHolder.staticCopyString(s) }
  expectEqual(start, CFGetRetainCount(s))
}

CFReturnsRetainedTestSuite.test("free function") {
  let s = makeString()
  let start = CFGetRetainCount(s)
  for _ in 0..<10 { _ = freeCopyString(s) }
  expectEqual(start, CFGetRetainCount(s))
}

runAllTests()
