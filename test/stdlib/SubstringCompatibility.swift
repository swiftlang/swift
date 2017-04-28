// RUN: rm -rf %t ; mkdir -p %t
// RUN: %target-build-swift %s -o %t/a.out3 -swift-version 3 && %target-run %t/a.out3
// RUN: %target-build-swift %s -o %t/a.out4 -swift-version 4 && %target-run %t/a.out4

import StdlibUnittest

#if swift(>=4)

public typealias ExpectedSubstring = Substring

#else

public typealias ExpectedSubstring = String

#endif

var Tests = TestSuite("SubstringCompatibility")

Tests.test("Range/Subscript/ExpectedType") {
  var s = "hello world"
  var sub = s[s.startIndex ..< s.endIndex]
  var subsub = sub[s.startIndex ..< s.endIndex]

  expectType(String.self, &s)
  expectType(ExpectedSubstring.self, &sub)
  expectType(ExpectedSubstring.self, &subsub)
}

Tests.test("ClosedRange/Subsript/ExpectedType") {
  var s = "hello world"
  let lastIndex = s.index(before:s.endIndex)
  var sub = s[s.startIndex ... lastIndex]
  var subsub = sub[s.startIndex ... lastIndex]

  expectType(String.self, &s)
  expectType(ExpectedSubstring.self, &sub)
  expectType(ExpectedSubstring.self, &subsub)
}


runAllTests()
