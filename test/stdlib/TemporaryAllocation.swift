// RUN: %target-run-simple-swift(-strict-memory-safety)
// REQUIRES: executable_test

import StdlibUnittest
import SwiftShims

var TemporaryAllocationTestSuite = TestSuite("TemporaryAllocation")

func isStackAllocated(_ pointer: UnsafeRawPointer) -> Bool? {
  guard #available(SwiftStdlib 5.6, *)
  else {
    return nil
  }

  var stackBegin: UInt = 0
  var stackEnd: UInt = 0
  if unsafe _swift_stdlib_getCurrentStackBounds(&stackBegin, &stackEnd) {
    let pointerValue = UInt(bitPattern: pointer)
    return pointerValue >= stackBegin && pointerValue < stackEnd
  }
  return nil
}

func expectStackAllocated(_ pointer: UnsafeRawPointer) {
  if let stackAllocated = unsafe isStackAllocated(pointer) {
    expectTrue(stackAllocated)
  } else {
    // Could not read stack bounds. Skip.
  }
}

func expectNotStackAllocated(_ pointer: UnsafeRawPointer) {
  if let stackAllocated = unsafe isStackAllocated(pointer) {
    expectFalse(stackAllocated)
  } else {
    // Could not read stack bounds. Skip.
  }
}

// MARK: Untyped buffers

TemporaryAllocationTestSuite.test("untypedAllocationOnStack") {
  withUnsafeTemporaryAllocation(byteCount: 8, alignment: 1) { buffer in
    unsafe expectStackAllocated(buffer.baseAddress!)
  }
}

TemporaryAllocationTestSuite.test("untypedAllocationOnHeap") {
  // EXPECTATION: a very large allocated buffer is heap-allocated. (Note if
  // swift_stdlib_isStackAllocationSafe() gets fleshed out, this test may need
  // to be changed.)
  withUnsafeTemporaryAllocation(byteCount: 100_000, alignment: 1) { buffer in
    unsafe expectNotStackAllocated(buffer.baseAddress!)
  }
}

TemporaryAllocationTestSuite.test("unprotectedUntypedAllocationOnStack") {
  unsafe _withUnprotectedUnsafeTemporaryAllocation(byteCount: 8, alignment: 1) {
    buffer in
    unsafe expectStackAllocated(buffer.baseAddress!)
  }
}

TemporaryAllocationTestSuite.test("unprotectedUntypedAllocationOnHeap") {
  // EXPECTATION: a very large allocated buffer is heap-allocated. (Note if
  // swift_stdlib_isStackAllocationSafe() gets fleshed out, this test may need
  // to be changed.)
  unsafe _withUnprotectedUnsafeTemporaryAllocation(
    byteCount: 100_000, alignment: 1
  ) { buffer in
    unsafe expectNotStackAllocated(buffer.baseAddress!)
  }
}

TemporaryAllocationTestSuite.test("untypedEmptyAllocationIsStackAllocated") {
  withUnsafeTemporaryAllocation(byteCount: 0, alignment: 1) { buffer in
    unsafe expectStackAllocated(buffer.baseAddress!)
  }
}

TemporaryAllocationTestSuite.test("crashOnNegativeByteCount")
.skip(.wasiAny(reason: "Trap tests aren't supported on WASI.")).code {
  expectCrash {
    let byteCount = Int.random(in: -2 ..< -1)
    withUnsafeTemporaryAllocation(byteCount: byteCount, alignment: 1) { _ in }
  }
}

TemporaryAllocationTestSuite.test("crashOnNegativeAlignment")
.skip(.wasiAny(reason: "Trap tests aren't supported on WASI.")).code {
  expectCrash {
    let alignment = Int.random(in: -2 ..< -1)
    withUnsafeTemporaryAllocation(byteCount: 16, alignment: alignment) { _ in }
  }
}

TemporaryAllocationTestSuite.test("untypedAllocationIsAligned") {
  withUnsafeTemporaryAllocation(byteCount: 1, alignment: 8) { buffer in
    let pointerBits = Int(bitPattern: buffer.baseAddress!)
    let alignmentMask = 0b111
    expectEqual(pointerBits & alignmentMask, 0)
  }
}

// MARK: Typed buffers

TemporaryAllocationTestSuite.test("typedAllocationOnStack") {
  withUnsafeTemporaryAllocation(of: Int.self, capacity: 1) { buffer in
    unsafe expectStackAllocated(buffer.baseAddress!)
  }
}

TemporaryAllocationTestSuite.test("spanOnStack") {
  withTemporaryAllocation(of: Int.self, capacity: 1) { span in
    unsafe span.withUnsafeMutableBufferPointer { buffer, initializedCount in
      unsafe expectStackAllocated(buffer.baseAddress!)
      initializedCount = 0
    }
  }
}

TemporaryAllocationTestSuite.test("typedAllocationOnHeap") {
  // EXPECTATION: a very large allocated buffer is heap-allocated. (Note if
  // swift_stdlib_isStackAllocationSafe() gets fleshed out, this test may need
  // to be changed.)
  withUnsafeTemporaryAllocation(of: Int.self, capacity: 100_000) { buffer in
    unsafe expectNotStackAllocated(buffer.baseAddress!)
  }
}

TemporaryAllocationTestSuite.test("spanOnHeap") {
  // EXPECTATION: a very large allocated buffer is heap-allocated. (Note if
  // swift_stdlib_isStackAllocationSafe() gets fleshed out, this test may need
  // to be changed.)
  withTemporaryAllocation(of: Int.self, capacity: 100_000) { span in
    unsafe span.withUnsafeMutableBufferPointer { buffer, initializedCount in
      unsafe expectNotStackAllocated(buffer.baseAddress!)
      initializedCount = 0
    }
  }
}

TemporaryAllocationTestSuite.test("unprotectedTypedAllocationOnStack") {
  unsafe _withUnprotectedUnsafeTemporaryAllocation(
    of: Int.self, capacity: 1
  ) { buffer in
    unsafe expectStackAllocated(buffer.baseAddress!)
  }
}

TemporaryAllocationTestSuite.test("unprotectedTypedAllocationOnHeap") {
  // EXPECTATION: a very large allocated buffer is heap-allocated. (Note if
  // swift_stdlib_isStackAllocationSafe() gets fleshed out, this test may need
  // to be changed.)
  unsafe _withUnprotectedUnsafeTemporaryAllocation(
    of: Int.self, capacity: 100_000
  ) { buffer in
    unsafe expectNotStackAllocated(buffer.baseAddress!)
  }
}

TemporaryAllocationTestSuite.test("typedEmptyAllocationIsStackAllocated") {
  withUnsafeTemporaryAllocation(of: Int.self, capacity: 0) { buffer in
    unsafe expectStackAllocated(buffer.baseAddress!)
  }
}

TemporaryAllocationTestSuite.test("voidAllocationIsStackAllocated") {
  withUnsafeTemporaryAllocation(of: Void.self, capacity: 1) { buffer in
    unsafe expectStackAllocated(buffer.baseAddress!)
  }
}

TemporaryAllocationTestSuite.test("voidSpanIsStackAllocated") {
  withTemporaryAllocation(of: Void.self, capacity: 1) { span in
    unsafe span.withUnsafeMutableBufferPointer { buffer, initializedCount in
      unsafe expectStackAllocated(buffer.baseAddress!)
      initializedCount = 0
    }
  }
}

TemporaryAllocationTestSuite.test("crashOnNegativeValueCount")
.skip(.wasiAny(reason: "Trap tests aren't supported on WASI.")).code {
  expectCrash {
    let capacity = Int.random(in: -2 ..< -1)
    withUnsafeTemporaryAllocation(of: Int.self, capacity: capacity) { _ in }
  }
}

TemporaryAllocationTestSuite.test("spanCrashOnNegativeValueCount")
.skip(.wasiAny(reason: "Trap tests aren't supported on WASI.")).code {
  expectCrash {
    let capacity = Int.random(in: -2 ..< -1)
    withTemporaryAllocation(of: Int.self, capacity: capacity) { _ in }
  }
}

TemporaryAllocationTestSuite.test("typedAllocationIsAligned") {
  withUnsafeTemporaryAllocation(of: Int.self, capacity: 1) { buffer in
    let pointerBits = Int(bitPattern: buffer.baseAddress!)
    let alignmentMask = MemoryLayout<Int>.alignment - 1
    expectEqual(pointerBits & alignmentMask, 0)
  }
}

TemporaryAllocationTestSuite.test("spanIsAligned") {
  withTemporaryAllocation(of: Int.self, capacity: 1) { span in
    unsafe span.withUnsafeMutableBufferPointer { buffer, initializedCount in
      let pointerBits = Int(bitPattern: buffer.baseAddress!)
      let alignmentMask = MemoryLayout<Int>.alignment - 1
      expectEqual(pointerBits & alignmentMask, 0)
      initializedCount = 0
    }
  }
}

// MARK: Typed throws
enum HomeworkError: Error, Equatable {
case dogAteIt
case forgot
}

TemporaryAllocationTestSuite.test("typedAllocationWithThrow") {
  do throws(HomeworkError) {
    try withUnsafeTemporaryAllocation(of: Int.self, capacity: 1) {
      (buffer) throws(HomeworkError) -> Void in
      throw HomeworkError.forgot
    }
    expectUnreachable("did not throw!?!")
  } catch {
    expectEqual(error, .forgot)
  }
}

TemporaryAllocationTestSuite.test("spanWithThrow") {
  do throws(HomeworkError) {
    try withTemporaryAllocation(of: Int.self, capacity: 1) {
      (span) throws(HomeworkError) -> Void in
      throw HomeworkError.forgot
    }
    expectUnreachable("did not throw!?!")
  } catch {
    expectEqual(error, .forgot)
  }
}

// MARK: Raw bytes span

TemporaryAllocationTestSuite.test("rawSpanOnStack") {
  withTemporaryAllocation(byteCount: 8, alignment: 1) { rawSpan in
    unsafe rawSpan.withUnsafeMutableBytes { buffer, initializedCount in
      unsafe expectStackAllocated(buffer.baseAddress!)
      initializedCount = 0
    }
  }
}

TemporaryAllocationTestSuite.test("rawSpanOnHeap") {
  withTemporaryAllocation(byteCount: 100_000, alignment: 1) { rawSpan in
    unsafe rawSpan.withUnsafeMutableBytes { buffer, initializedCount in
      unsafe expectNotStackAllocated(buffer.baseAddress!)
      initializedCount = 0
    }
  }
}

TemporaryAllocationTestSuite.test("rawSpanEmptyAllocationIsStackAllocated") {
  withTemporaryAllocation(byteCount: 0, alignment: 1) { rawSpan in
    unsafe rawSpan.withUnsafeMutableBytes { buffer, initializedCount in
      unsafe expectStackAllocated(buffer.baseAddress!)
      initializedCount = 0
    }
  }
}

TemporaryAllocationTestSuite.test("rawSpanIsAligned") {
  withTemporaryAllocation(byteCount: 1, alignment: 8) { rawSpan in
    unsafe rawSpan.withUnsafeMutableBytes { buffer, initializedCount in
      let pointerBits = Int(bitPattern: buffer.baseAddress!)
      let alignmentMask = 0b111
      expectEqual(pointerBits & alignmentMask, 0)
      initializedCount = 0
    }
  }
}

TemporaryAllocationTestSuite.test("rawSpanAppendAndByteCount") {
  withTemporaryAllocation(byteCount: 4, alignment: 1) { rawSpan in
    expectEqual(rawSpan.byteCount, 0)
    expectEqual(rawSpan.freeCapacity, 4)
    expectTrue(rawSpan.isEmpty)
    rawSpan.append(UInt8(0xAB))
    rawSpan.append(UInt8(0xCD))
    expectEqual(rawSpan.byteCount, 2)
    expectEqual(rawSpan.freeCapacity, 2)
    expectFalse(rawSpan.isEmpty)
  }
}

TemporaryAllocationTestSuite.test("rawSpanCrashOnNegativeByteCount")
.skip(.wasiAny(reason: "Trap tests aren't supported on WASI.")).code {
  expectCrash {
    let byteCount = Int.random(in: -2 ..< -1)
    withTemporaryAllocation(byteCount: byteCount, alignment: 1) { _ in }
  }
}

TemporaryAllocationTestSuite.test("rawSpanCrashOnNegativeAlignment")
.skip(.wasiAny(reason: "Trap tests aren't supported on WASI.")).code {
  expectCrash {
    let alignment = Int.random(in: -2 ..< -1)
    withTemporaryAllocation(byteCount: 16, alignment: alignment) { _ in }
  }
}

TemporaryAllocationTestSuite.test("rawSpanWithThrow") {
  do throws(HomeworkError) {
    try withTemporaryAllocation(byteCount: 8, alignment: 1) {
      (rawSpan) throws(HomeworkError) -> Void in
      throw HomeworkError.forgot
    }
    expectUnreachable("did not throw")
  } catch {
    expectEqual(error, .forgot)
  }
}

runAllTests()
