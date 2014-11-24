// RUN: %target-run-simple-swift
// XFAIL: interpret, linux

import StdlibUnittest

var UnmanagedTests = TestSuite("Unmanaged")

UnmanagedTests.test("fromOpaque()/trap") {
  let null = getPointer(COpaquePointer())
  expectCrashLater()
  Unmanaged<AnyObject>.fromOpaque(null)
}

runAllTests()

