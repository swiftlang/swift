// RUN: %target-run-stdlib-swift
// REQUIRES: executable_test

import Swift
import StdlibUnittest

#if os(Linux) || os(FreeBSD)
import Glibc
#endif

var GlibcTestSuite = TestSuite("Glibc")

GlibcTestSuite.test("errno")
  .skip(.custom({
    !( linuxAny(reason: "").evaluate() || freeBSDAny(reason: "").evaluate() ) 
  }, reason: "Only Linux and FreeBSD are supported")).code {
    errno = 0
    expectEqual(0, errno)
    close(-1)
    expectEqual(EBADF, errno)
}


var GlibcIoctlConstants = TestSuite("GlibcIoctlConstants")

GlibcIoctlConstants.test("tty ioctl constants availability") 
  .skip(.custom({
    !( linuxAny(reason: "").evaluate() || freeBSDAny(reason: "").evaluate() ) 
  }, reason: "Only Linux and FreeBSD are supported")).code {
    let aConstant = TIOCGWINSZ
}


runAllTests()
