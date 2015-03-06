// RUN: %target-run-simple-swift

// XFAIL: interpret

import StdlibUnittest

var UnmanagedTests = TestSuite("Unmanaged")

UnmanagedTests.test("fromOpaque()/trap") {
  let null = getPointer(COpaquePointer())
  expectCrashLater()
  Unmanaged<AnyObject>.fromOpaque(null)
}

class FooClass {}

UnmanagedTests.test("unsafeBitCast(Unmanaged, Word)") {
  let ref = FooClass()
  expectNotEqual(
    0,
    unsafeBitCast(
      Unmanaged.passUnretained(ref) as Unmanaged<AnyObject>,
      Word.self))
  _fixLifetime(ref)
}

runAllTests()

