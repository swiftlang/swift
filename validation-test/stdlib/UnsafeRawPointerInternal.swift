// RUN: %target-build-swift %s -parse-stdlib -Xfrontend -disable-access-control -o %t.out
// RUN: %target-codesign %t.out
// RUN: %target-run %t.out
// REQUIRES: executable_test

import Swift
import StdlibUnittest

var UnsafeRawPointerTestSuite = TestSuite("UnsafeRawPointerTestSuite")

UnsafeRawPointerTestSuite.test("load.unaligned.largeAlignment")
.skip(.custom({
  if #available(SwiftStdlib 5.7, *) { return false } else { return true }
}, reason: "Requires standard library from Swift 5.7"))
.code {
  guard #available(SwiftStdlib 5.7, *) else { return }
  var offset = 3
  let int128 = withUnsafeTemporaryAllocation(of: UInt8.self, capacity: 64) {
    temporary -> Builtin.Int128 in
    let buffer = UnsafeRawBufferPointer(temporary)
    _ = temporary.initialize(from: repeatElement(0, count: 64))
    // Load a 128-bit floating point value
    let fp = buffer.loadUnaligned(fromByteOffset: offset, as: Builtin.FPIEEE128.self)
    noop(fp)
    temporary.baseAddress!.deinitialize(count: 64)
    _ = temporary.initialize(from: 0..<64)
    let aligned = buffer.baseAddress!.alignedUp(for: Builtin.Int128.self)
    offset += buffer.baseAddress!.distance(to: aligned)
    // Load and return a 128-bit integer value
    return buffer.loadUnaligned(fromByteOffset: offset, as: Builtin.Int128.self)
  }
  withUnsafeBytes(of: int128) {
    expectEqual(Int($0[0]), offset)
    let lastIndex = $0.indices.last!
    expectEqual(Int($0[lastIndex]), offset+lastIndex)
  }
}

UnsafeRawPointerTestSuite.test("load.unaligned.largeAlignment.mutablePointer")
.skip(.custom({
  if #available(SwiftStdlib 5.7, *) { return false } else { return true }
}, reason: "Requires standard library from Swift 5.7"))
.code {
  guard #available(SwiftStdlib 5.7, *) else { return }
  var offset = 11
  let int128 = withUnsafeTemporaryAllocation(of: UInt8.self, capacity: 64) {
    temporary -> Builtin.Int128 in
    let buffer = UnsafeMutableRawBufferPointer(temporary)
    buffer.copyBytes(from: 0..<64)
    let aligned = buffer.baseAddress!.alignedUp(for: Builtin.Int128.self)
    offset += buffer.baseAddress!.distance(to: aligned)
    return buffer.loadUnaligned(fromByteOffset: offset, as: Builtin.Int128.self)
  }
  withUnsafeBytes(of: int128) {
    expectEqual(Int($0[0]), offset)
    let lastIndex = $0.indices.last!
    expectEqual(Int($0[lastIndex]), offset+lastIndex)
  }
}

UnsafeRawPointerTestSuite.test("alignedUp.for.overflow") {
  let p = UnsafeRawPointer(bitPattern: 1-MemoryLayout<Int>.stride)!
  if _isDebugAssertConfiguration() {
    expectCrashLater()
  }
  let up = p.alignedUp(for: Int.self)
  expectEqual(Int(bitPattern: up), 0)
}

UnsafeRawPointerTestSuite.test("alignedUp.toMultiple.overflow") {
  let p = UnsafeRawPointer(bitPattern: -7)!
  if _isDebugAssertConfiguration() {
    expectCrashLater()
  }
  let up = p.alignedUp(toMultipleOf: 8)
  expectEqual(Int(bitPattern: up), 0)
}

UnsafeRawPointerTestSuite.test("alignedUp.for.overflow") {
  let p = UnsafeRawPointer(bitPattern: MemoryLayout<Int64>.stride-1)!
  if _isDebugAssertConfiguration() {
    expectCrashLater()
  }
  let up = p.alignedDown(for: Int64.self)
  expectEqual(Int(bitPattern: up), 0)
}

UnsafeRawPointerTestSuite.test("alignedUp.toMultiple.overflow") {
  let p = UnsafeRawPointer(bitPattern: 13)!
  if _isDebugAssertConfiguration() {
    expectCrashLater()
  }
  let up = p.alignedDown(toMultipleOf: 16)
  expectEqual(Int(bitPattern: up), 0)
}

UnsafeRawPointerTestSuite.test("alignedUp.for.overflow.mutable") {
  let p = UnsafeMutableRawPointer(bitPattern: 1-MemoryLayout<Int>.stride)!
  if _isDebugAssertConfiguration() {
    expectCrashLater()
  }
  let up = p.alignedUp(for: Int.self)
  expectEqual(Int(bitPattern: up), 0)
}

UnsafeRawPointerTestSuite.test("alignedUp.toMultiple.overflow.mutable") {
  let p = UnsafeMutableRawPointer(bitPattern: -7)!
  if _isDebugAssertConfiguration() {
    expectCrashLater()
  }
  let up = p.alignedUp(toMultipleOf: 8)
  expectEqual(Int(bitPattern: up), 0)
}

UnsafeRawPointerTestSuite.test("alignedUp.for.overflow.mutable") {
  let p = UnsafeMutableRawPointer(bitPattern: MemoryLayout<Int64>.stride-1)!
  if _isDebugAssertConfiguration() {
    expectCrashLater()
  }
  let up = p.alignedDown(for: Int64.self)
  expectEqual(Int(bitPattern: up), 0)
}

UnsafeRawPointerTestSuite.test("alignedUp.toMultiple.overflow.mutable") {
  let p = UnsafeMutableRawPointer(bitPattern: 13)!
  if _isDebugAssertConfiguration() {
    expectCrashLater()
  }
  let up = p.alignedDown(toMultipleOf: 16)
  expectEqual(Int(bitPattern: up), 0)
}

runAllTests()
