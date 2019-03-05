// RUN: %target-run-simple-swift
// REQUIRES: executable_test

//
// Tests for shared string APIs
//

import StdlibUnittest

var SharedStringTests = TestSuite("SharedStringTests")

func makeValidUTF8Buffer() -> UnsafeBufferPointer<UInt8> {
  let ptr = UnsafeMutablePointer<UInt8>.allocate(capacity: 4)
  ptr.initialize(repeating: UInt8(ascii: "a"), count: 4)
  return UnsafeBufferPointer<UInt8>(start: ptr, count: 4)
}

func makeInvalidUTF8Buffer() -> UnsafeBufferPointer<UInt8> {
  let ptr = UnsafeMutablePointer<UInt8>.allocate(capacity: 1)
  ptr.pointee = 0x80  // orphaned continuation byte
  return UnsafeBufferPointer<UInt8>(start: ptr, count: 1)
}

class BufferDeallocator {
  let buffer: UnsafeBufferPointer<UInt8>

  init(_ buffer: UnsafeBufferPointer<UInt8>) {
    self.buffer = buffer
  }

  deinit {
    buffer.deallocate()
  }
}

SharedStringTests.test("String.init(sharing:owner:)") {
  let buf = makeValidUTF8Buffer()
  let str = String(sharingContent: buf, owner: BufferDeallocator(buf))

  expectNotNil(str)
  expectEqual(str!, "aaaa")

  // Show that the string didn't copy the buffer by modifying it in-place.
  UnsafeMutableBufferPointer(mutating: buf)[0] = UInt8(ascii: "b")
  expectEqual(str!, "baaa")

  // Show that mutating a copy works as expected.
  var copy = str!
  copy.append("c")
  expectEqual(copy, "baaac")
  expectEqual(str!, "baaa")
}

SharedStringTests.test("String.init(sharing:owner:) invalid UTF8") {
  let buf = makeInvalidUTF8Buffer()
  let str = String(sharingContent: buf, owner: BufferDeallocator(buf))

  expectNil(str)
}

SharedStringTests.test("Substring.withSharedString(_:)") {
  let original = "abcde"
  let substr = original.dropFirst().dropLast()

  substr.withSharedString { shared in
    expectEqual(shared, "bcd")
  }
}

runAllTests()
