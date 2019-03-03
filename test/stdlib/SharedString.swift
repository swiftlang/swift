// RUN: %target-run-simple-swift
// REQUIRES: executable_test

//
// Tests for shared string APIs
//

import StdlibUnittest

var SharedStringTests = TestSuite("SharedStringTests")

SharedStringTests.test("String.withSharedUTF8") {
  let ptr = UnsafeMutablePointer<UInt8>.allocate(capacity: 4)
  defer { ptr.deallocate() }
  ptr.initialize(repeating: UInt8(ascii: "a"), count: 4)
  let buf = UnsafeBufferPointer<UInt8>(start: ptr, count: 4)

  String.withSharedUTF8(buf) { str in
    expectNotNil(str)
    expectEqual(str!, "aaaa")

    ptr.pointee = UInt8(ascii: "b")
    expectEqual(str!, "baaa")
  }
}

SharedStringTests.test("String.withSharedUTF8 invalid UTF8") {
  let ptr = UnsafeMutablePointer<UInt8>.allocate(capacity: 1)
  defer { ptr.deallocate() }
  ptr.pointee = 0x80  // orphaned continuation byte
  let buf = UnsafeBufferPointer<UInt8>(start: ptr, count: 1)

  String.withSharedUTF8(buf) { str in
    expectNil(str)
  }
}

SharedStringTests.test("String.withSharedNullTerminatedUTF8") {
  let ptr = UnsafeMutablePointer<UInt8>.allocate(capacity: 5)
  defer { ptr.deallocate() }
  ptr.initialize(repeating: UInt8(ascii: "a"), count: 4)
  ptr[4] = 0

  String.withSharedNullTerminatedUTF8(ptr) { str in
    expectNotNil(str)
    expectEqual(str!, "aaaa")

    ptr.pointee = UInt8(ascii: "b")
    expectEqual(str!, "baaa")
  }
}

SharedStringTests.test("String.withSharedNullTerminatedUTF8 invalid UTF8") {
  let ptr = UnsafeMutablePointer<UInt8>.allocate(capacity: 2)
  defer { ptr.deallocate() }
  ptr[0] = 0x80  // orphaned continuation byte
  ptr[1] = 0

  String.withSharedNullTerminatedUTF8(ptr) { str in
    expectNil(str)
  }
}

SharedStringTests.test("String.init(sharedUTF8:deallocator:)") {
  let ptr = UnsafeMutablePointer<UInt8>.allocate(capacity: 4)
  ptr.initialize(repeating: UInt8(ascii: "a"), count: 4)
  let buf = UnsafeBufferPointer<UInt8>(start: ptr, count: 4)

  let str = String(sharingUTF8: buf, deallocator: .custom({ ptr, _ in
    ptr.deallocate()
  }))
  expectNotNil(str)
  expectEqual(str!, "aaaa")

  ptr.pointee = UInt8(ascii: "b")
  expectEqual(str!, "baaa")

  // Verify that the deallocator was called by trying to free the memory again.
  // If a crash occurs, that means the deallocator already did it.
  expectCrashLater()
  ptr.deallocate()
}

SharedStringTests.test("String.withSharedUTF8 invalid UTF8") {
  let ptr = UnsafeMutablePointer<UInt8>.allocate(capacity: 1)
  defer { ptr.deallocate() }
  ptr.pointee = 0x80  // orphaned continuation byte
  let buf = UnsafeBufferPointer<UInt8>(start: ptr, count: 1)

  expectNil(String(sharingUTF8: buf, deallocator: .none))
}

SharedStringTests.test("String.init(sharingNullTerminatedUTF8:deallocator:)") {
  let ptr = UnsafeMutablePointer<UInt8>.allocate(capacity: 5)
  ptr.initialize(repeating: UInt8(ascii: "a"), count: 4)
  ptr[4] = 0

  let str = String(sharingNullTerminatedUTF8: ptr, deallocator: .custom({ ptr, _ in
    ptr.deallocate()
  }))
  expectNotNil(str)
  expectEqual(str!, "aaaa")

  ptr.pointee = UInt8(ascii: "b")
  expectEqual(str!, "baaa")

  // Verify that the deallocator was called by trying to free the memory again.
  // If a crash occurs, that means the deallocator already did it.
  expectCrashLater()
  ptr.deallocate()
}

SharedStringTests.test("String.init(sharingNullTerminatedUTF8:deallocator:) invalid UTF8") {
  let ptr = UnsafeMutablePointer<UInt8>.allocate(capacity: 2)
  defer { ptr.deallocate() }
  ptr[0] = 0x80  // orphaned continuation byte
  ptr[1] = 0

  expectNil(String(sharingNullTerminatedUTF8: ptr, deallocator: .none))
}

runAllTests()
