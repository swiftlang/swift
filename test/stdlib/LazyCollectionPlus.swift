// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -o %t/a.out3 -swift-version 3 && %target-run %t/a.out3
// REQUIRES: executable_test

import StdlibUnittest

var tests = TestSuite("LazyCollectionPlus")

tests.test("NoAmbiguity") {
  let d: [String: Int] = ["" : 0]
  let xs = d.values.map { $0 }
  let ys = d.values.map { $0 + 42 }
  let actual = xs + ys // this line should compile without ambiguity
  expectEqualSequence([0, 42], actual)
}

runAllTests()
