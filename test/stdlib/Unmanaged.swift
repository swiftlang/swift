// RUN: %target-run-simple-swift
// XFAIL: interpret

import StdlibUnittest

var UnmanagedTests = TestSuite("Unmanaged")

UnmanagedTests.test("fromOpaque()/trap") {
  let null = getPtr(COpaquePointer())
  expectCrashLater()
  Unmanaged<AnyObject>.fromOpaque(null)
}

runAllTests()

