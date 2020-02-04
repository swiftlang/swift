// RUN: %target-run-simple-swiftgyb
// REQUIRES: executable_test

// This test requires that the standard library calls ICU
// directly.  It is not specific to Linux, it is just that on
// Apple platforms we are using the NSString bridge right now.

// REQUIRES: OS=linux-gnu || OS=linux-android || OS=linux-androideabi

import StdlibUnittest

var StringTests = TestSuite("StringTests")

StringTests.test("smoketest") {
  expectEqual("i\u{0307}", "\u{0130}".lowercased())
  expectEqual("SS", "\u{00df}".uppercased())
}

runAllTests()

