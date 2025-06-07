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
  expectNotNil(voidPtr, "toOpaque must not return null pointer")
  
  let unmanaged = Unmanaged<Foobar>.fromOpaque(voidPtr!)
  expectEqual(
    ref === unmanaged.takeUnretainedValue(),
    true,
    "fromOpaque must return the same reference")
  
  _fixLifetime(ref)
}

UnmanagedTests.test("Opaque avoid retain/release") {
  // The purpose of this test is to ensure that the fromOpaque/toOpaque calls do
  // NOT attempt to retain/release the class instance at the passed pointer.
  // Here we're simulating a dangling pointer referencing a class who has
  // already been released (the allocated pointer points at nothing) and
  // attempting to create an Unmanaged instance from it and get back the
  // pointer. This test's expectEqual is kind of bogus, we're just checking that
  // it doesn't crash.

  // Create a dangling pointer, usually to unmapped memory.
  let ref = UnsafeRawPointer(bitPattern: 1)!
  // Turn it into a dangling unmanaged reference.
  // We expect this not to crash, as this operation isn't 
  // supposed to dereference the pointer in any way.
  let unmanaged = Unmanaged<Foobar>.fromOpaque(ref)
  // Similarly, converting the unmanaged reference back to a 
  // pointer should not ever try to dereference it either.
  let ref2 = unmanaged.toOpaque()
  // ...and we must get back the same pointer.
  expectEqual(ref, ref2)
}

runAllTests()
