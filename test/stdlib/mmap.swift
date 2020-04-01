// RUN: %target-run-simple-swift %t
// REQUIRES: executable_test
// UNSUPPORTED: OS=windows-msvc

import StdlibUnittest
#if os(macOS) || os(iOS) || os(tvOS) || os(watchOS)
  import Darwin
#elseif os(Linux) || os(FreeBSD) || os(OpenBSD) || os(PS4) || os(Android) || os(Cygwin) || os(Haiku)
  import Glibc
#else
#error("Unsupported platform")
#endif

var MMapTests = TestSuite("MMaptests")

MMapTests.test("MAP_FAILED") {
  expectEqual(mmap(nil, 0, 0, 0, 0, 0), MAP_FAILED)
}

runAllTests()
