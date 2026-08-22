// RUN: %target-run-simple-swift
// REQUIRES: executable_test
//
// UNSUPPORTED: OS=macosx
// UNSUPPORTED: OS=ios
// UNSUPPORTED: OS=tvos
// UNSUPPORTED: OS=watchos

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
  _ = sendfile
}

var GlibcIoctlConstants = TestSuite("GlibcIoctlConstants")

GlibcIoctlConstants.test("tty ioctl constants availability") {
  let aConstant = TIOCSTI
}


// Test spellings introduced when '_GNU_SOURCE' is defined.
var GlibcGNUSource = TestSuite("GlibcGNUSource")

GlibcGNUSource.test("accept4 address parameter") {
  let accept4Signature: (
    CInt, UnsafeMutablePointer<sockaddr>?, UnsafeMutablePointer<socklen_t>?, CInt
  ) -> CInt
  accept4Signature = accept4
}

GlibcGNUSource.test("getrlimit accepts CInt") {
  var rl = rlimit()

  expectEqual(0, getrlimit(__rlimit_resource_t(RLIMIT_CORE.rawValue), &rl))
  // use the compatibility overload:
  expectEqual(0, getrlimit(CInt(RLIMIT_CORE.rawValue), &rl))
}

GlibcGNUSource.test("getrusage accepts a negative CInt") {
  var ru = rusage()

  expectEqual(0, getrusage(__rusage_who_t(RUSAGE_CHILDREN.rawValue), &ru))
  // use the compatibility overload:
  expectEqual(0, getrusage(CInt(RUSAGE_CHILDREN.rawValue), &ru))
}

GlibcGNUSource.test("getpriority accepts CInt") {
  errno = 0
  _ = getpriority(__priority_which_t(PRIO_PROCESS.rawValue), 0)
  expectEqual(0, errno)

  // use the compatibility overload:
  errno = 0
  _ = getpriority(CInt(PRIO_PROCESS.rawValue), 0)
  expectEqual(0, errno)
}

GlibcGNUSource.test("getitimer accepts CInt") {
  var itv = itimerval()

  expectEqual(0, getitimer(__itimer_which_t(ITIMER_REAL.rawValue), &itv))
  // use the compatibility overload:
  expectEqual(0, getitimer(CInt(ITIMER_REAL.rawValue), &itv))
}

GlibcGNUSource.test("fd_set __fds_bits") {
  var set = SwiftGlibc.fd_set()
  // .fds_bits is the property name when _GNU_SOURCE is defined
  set.fds_bits.3 = 42

  // .__fds_bits is the property name without _GNU_SOURCE,
  // made available with an extension in the Glibc overlay.
  expectTrue(type(of: set.__fds_bits) == type(of: set.fds_bits))
  expectEqual(42, set.__fds_bits.3)

  set.__fds_bits.7 = 99
  expectEqual(99, set.fds_bits.7)
}

runAllTests()
