// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest


// Check that the generic parameter is called 'Instance'.
protocol TestProtocol1 {}

extension Unmanaged where Instance : TestProtocol1 {
  var _instanceIsTestProtocol1: Bool {
    fatalError("not implemented")
  }
}

var UnmanagedTests = TestSuite("Unmanaged")

class FooClass {}

UnmanagedTests.test("unsafeBitCast(Unmanaged, Int)") {
  let ref = FooClass()
  expectNotEqual(
    0,
    unsafeBitCast(
      Unmanaged.passUnretained(ref) as Unmanaged<AnyObject>,
      to: Int.self))
  _fixLifetime(ref)
}

class Foobar {
  func foo() -> Int { return 1 }
}

UnmanagedTests.test("_withUnsafeGuaranteedRef") {
  var ref = Foobar()
  var unmanaged = Unmanaged.passUnretained(ref)
  withExtendedLifetime(ref) {
    unmanaged._withUnsafeGuaranteedRef {
      expectTrue(ref === $0)
    }
    unmanaged._withUnsafeGuaranteedRef {
      expectEqual(1, $0.foo())
    }
  }
}

UnmanagedTests.test("_withUnsafeGuaranteedRef/return") {
  var ref = Foobar()
  var unmanaged = Unmanaged.passUnretained(ref)
  withExtendedLifetime(ref) {
    expectEqual(1, unmanaged._withUnsafeGuaranteedRef {
      return $0.foo()
    })
  }
}

UnmanagedTests.test("Opaque") {
  var ref = Foobar()
  let opaquePtr = Unmanaged.passUnretained(ref).toOpaque()
  
  let unknownPtr = Int(bitPattern: opaquePtr)
  let voidPtr = UnsafeRawPointer(bitPattern: unknownPtr)
  expectNotEmpty(voidPtr, "toOpaque must not return null pointer")
  
  let unmanaged = Unmanaged<Foobar>.fromOpaque(voidPtr!)
  expectEqual(
    ref === unmanaged.takeUnretainedValue(),
    true,
    "fromOpaque must return the same reference")
  
  _fixLifetime(ref)
}

runAllTests()
