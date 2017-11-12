// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest

struct Buffer32 {
  var x0: UInt64 = 0
  var x1: UInt64 = 0
  var x2: UInt64 = 0
  var x3: UInt64 = 0
}

func foo() -> UInt64 {
  var buffer = Buffer32()
  var v0: UInt64 = 1
  var v1: UInt64 = 2
  var b: Bool = true
  return withUnsafeMutablePointer(to: &buffer) { bufferPtr in
    bufferPtr.pointee.x0 = 5
    bufferPtr.pointee.x1 = v0
    bufferPtr.pointee.x2 = v1
    bufferPtr.pointee.x3 = b ? v0 : v1
    return bufferPtr.pointee.x3
  }
}

TestSuite("AllocRounding").test("Basic") {
  expectEqual(1, foo())
}

runAllTests()
