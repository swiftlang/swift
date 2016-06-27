// RUN: %target-run-stdlib-swift
// REQUIRES: executable_test

// REQUIRES: OS=linux-gnu

import Swift
import StdlibUnittest


import Glibc

var GlibcTestSuite = TestSuite("Glibc")

GlibcTestSuite.test("errno") {
  errno = 0
  expectEqual(0, errno)
  close(-1)
  expectEqual(EBADF, errno)
}

var GlibcIoctlConstants = TestSuite("GlibcIoctlConstants")

GlibcIoctlConstants.test("tty ioctl constants availability") {
  let aConstant = TIOCGWINSZ
}

runAllTests()
