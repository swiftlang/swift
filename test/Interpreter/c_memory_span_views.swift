//===--- c_memory_span_views.swift ----------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Executable coverage for `Span` and `RawSpan` over memory described to Swift
// through `@_extern(c)` declarations, with a small C helper TU linked the same
// way as `refcount_overflow.swift` / `refcount_bridgeobject.swift`.
//
// This exercises the common pattern where C retains ownership of a buffer (or
// writes into Swift-allocated storage) while Swift forms short-lived,
// non-owning stdlib views instead of materializing an `Array` copy.
//
//===----------------------------------------------------------------------===//

// RUN: %empty-directory(%t)
// RUN: %target-clang -x c %S/Inputs/c_memory_span_buffers.c -c -o %t/c_memory_span_buffers.o
// RUN: %target-build-swift -swift-version 6 -enable-experimental-feature Extern %t/c_memory_span_buffers.o %s -o %t/c_memory_span_views
// RUN: %target-codesign %t/c_memory_span_views
// RUN: %target-run %t/c_memory_span_views

// REQUIRES: executable_test
// REQUIRES: swift_feature_Extern
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

import StdlibUnittest

@_extern(c)
func c_alloc_i32_pattern_buffer() -> UnsafeMutablePointer<Int32>?
@_extern(c)
func c_free_i32_buffer(_ p: UnsafeMutablePointer<Int32>?)
@_extern(c)
func c_i32_pattern_element_count() -> Int32

@_extern(c)
func c_alloc_byte_pattern_buffer() -> UnsafeMutablePointer<UInt8>?
@_extern(c)
func c_free_byte_buffer(_ p: UnsafeMutablePointer<UInt8>?)
@_extern(c)
func c_byte_pattern_length() -> Int32

@_extern(c)
func c_fill_incrementing_u8(_ bytes: UnsafeMutablePointer<UInt8>?, _ byteCount: Int)

let CMallocSpanTests = TestSuite("CMallocSpanViews")

CMallocSpanTests.test("Span over C heap Int32 buffer")
.skip(.custom(
  { if #available(SwiftStdlib 6.2, *) { false } else { true } },
  reason: "Requires Swift 6.2 Span in the standard library"
))
.code {
  guard #available(SwiftStdlib 6.2, *) else { return }

  guard let base = c_alloc_i32_pattern_buffer() else {
    expectUnreachable("C allocation failed")
    return
  }
  defer { c_free_i32_buffer(base) }

  let n = Int(c_i32_pattern_element_count())
  let mutable = UnsafeMutableBufferPointer(start: base, count: n)
  let span = Span(_unsafeElements: UnsafeBufferPointer(mutable))

  expectEqual(span.count, n)
  for i in span.indices {
    expectEqual(span[i], Int32(i) * 11)
  }
}

CMallocSpanTests.test("Span.bytes as RawSpan over C heap Int32 buffer")
.skip(.custom(
  { if #available(SwiftStdlib 6.2, *) { false } else { true } },
  reason: "Requires Swift 6.2 Span in the standard library"
))
.code {
  guard #available(SwiftStdlib 6.2, *) else { return }

  guard let base = c_alloc_i32_pattern_buffer() else {
    expectUnreachable("C allocation failed")
    return
  }
  defer { c_free_i32_buffer(base) }

  let n = Int(c_i32_pattern_element_count())
  let mutable = UnsafeMutableBufferPointer(start: base, count: n)
  let span = Span(_unsafeElements: UnsafeBufferPointer(mutable))
  let raw = span.bytes

  expectEqual(raw.byteCount, n * MemoryLayout<Int32>.stride)
  for i in 0..<n {
    let offset = i * MemoryLayout<Int32>.stride
    let v = raw.unsafeLoad(fromByteOffset: offset, as: Int32.self)
    expectEqual(v, Int32(i) * 11)
  }
}

CMallocSpanTests.test("RawSpan over C heap bytes")
.skip(.custom(
  { if #available(SwiftStdlib 6.2, *) { false } else { true } },
  reason: "Requires Swift 6.2 RawSpan in the standard library"
))
.code {
  guard #available(SwiftStdlib 6.2, *) else { return }

  guard let base = c_alloc_byte_pattern_buffer() else {
    expectUnreachable("C allocation failed")
    return
  }
  defer { c_free_byte_buffer(base) }

  let n = Int(c_byte_pattern_length())
  let raw = UnsafeRawBufferPointer(
    start: UnsafeRawPointer(base),
    count: n
  )
  let span = RawSpan(_unsafeBytes: raw)

  expectEqual(span.byteCount, n)
  span.withUnsafeBytes { bytes in
    for (i, b) in bytes.enumerated() {
      expectEqual(b, UInt8(0xA0 + (i % 16)))
    }
  }
}

CMallocSpanTests.test("C fills Swift buffer then zero-copy RawSpan read")
.skip(.custom(
  { if #available(SwiftStdlib 6.2, *) { false } else { true } },
  reason: "Requires Swift 6.2 RawSpan in the standard library"
))
.code {
  guard #available(SwiftStdlib 6.2, *) else { return }

  var storage = [UInt8](repeating: 0, count: 32)
  storage.withUnsafeMutableBytes { rawMutable in
    c_fill_incrementing_u8(rawMutable.baseAddress?.assumingMemoryBound(to: UInt8.self),
                             rawMutable.count)
  }
  let expected = [UInt8]((0..<storage.count).map { UInt8($0 & 0xFF) })
  storage.withUnsafeBytes { raw in
    let span = RawSpan(_unsafeBytes: raw)
    expectEqual(span.byteCount, storage.count)
    span.withUnsafeBytes { sb in
      expectTrue(sb.elementsEqual(expected))
    }
  }
}

runAllTests()
