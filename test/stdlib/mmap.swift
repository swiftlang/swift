// RUN: %target-run-simple-swift %t
// REQUIRES: executable_test

import StdlibUnittest
#if os(Linux) || os(Android)
  import Glibc
#else
  import Darwin
#endif

var MMapTests = TestSuite("MMaptests")

MMapTests.test("MAP_FAILED") {
  expectEqual(mmap(nil, 0, 0, 0, 0, 0), MAP_FAILED)
}

runAllTests()
