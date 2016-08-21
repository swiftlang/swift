// RUN: %target-run-simple-swift
// REQUIRES: executable_test
//
// UNSUPPORTED: OS=macosx
// UNSUPPORTED: OS=ios
// UNSUPPORTED: OS=tvos
// UNSUPPORTED: OS=watchos
// UNSUPPORTED: OS=linux-androideabi

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

GlibcTestSuite.test("sendfile") {
  // Check that `sendfile` is available.  Don't actually call it, because doing that is non-trivial.
  var sendfile_ = sendfile
  expectEqual(((Int32, Int32, off_t, size_t) -> ssize_t).self, &sendfile_)
}

var GlibcIoctlConstants = TestSuite("GlibcIoctlConstants")

GlibcIoctlConstants.test("tty ioctl constants availability") {
  let aConstant = TIOCSTI
}

runAllTests()
