//===----------------------------------------------------------------------===//
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
// RUN: %target-run-simple-swift( \
// RUN:   -enable-experimental-feature ValueGenerics \
// RUN:   -parse-as-library \
// RUN: )
// REQUIRES: executable_test
// REQUIRES: swift_feature_ValueGenerics
// UNSUPPORTED: use_os_stdlib
// END.
//
//===----------------------------------------------------------------------===//

import StdlibUnittest
import Synchronization

@available(SwiftStdlib 6.2, *)
@main
enum InlineArrayTests {

  @available(SwiftStdlib 6.2, *)
  static func main() {
    let testSuite = TestSuite("InlineArrayTests")
    testSuite.test("Empty",       testEmpty)
    testSuite.test("Nonempty",    testNonempty)
    testSuite.test("Copyable",    testCopyable)
    testSuite.test("Noncopyable", testNoncopyable)
    testSuite.test("Uninhabited", testUninhabited)
    runAllTests()
  }

  /// An *empty* array's size is zero. Its stride and alignment are one byte.
  @available(SwiftStdlib 6.2, *)
  static func testEmpty() {
    expectEqual(MemoryLayout<InlineArray<0, Int>>.size,      0)
    expectEqual(MemoryLayout<InlineArray<0, Int>>.stride,    1)
    expectEqual(MemoryLayout<InlineArray<0, Int>>.alignment, 1)
  }

  /// A *nonempty* array's size and stride are equal to the element's stride
  /// multiplied by the number of elements. Its alignment is equal to the
  /// element's alignment.
  @available(SwiftStdlib 6.2, *)
  static func testNonempty() {
    // StaticString has { Word, Word, Byte } stored properties.
    let _ = MemoryLayout<StaticString>.size       // e.g. 17 bytes.
    let s = MemoryLayout<StaticString>.stride     // e.g. 24 bytes.
    let a = MemoryLayout<StaticString>.alignment  // e.g.  8 bytes.
    expectEqual(MemoryLayout<InlineArray<1, StaticString>>.size,      s * 1)
    expectEqual(MemoryLayout<InlineArray<1, StaticString>>.stride,    s * 1)
    expectEqual(MemoryLayout<InlineArray<1, StaticString>>.alignment, a)
    expectEqual(MemoryLayout<InlineArray<2, StaticString>>.size,      s * 2)
    expectEqual(MemoryLayout<InlineArray<2, StaticString>>.stride,    s * 2)
    expectEqual(MemoryLayout<InlineArray<2, StaticString>>.alignment, a)
  }

  /// An array and its elements can be of *copyable* type.
  @available(SwiftStdlib 6.2, *)
  static func testCopyable() {
    do {
      let a: InlineArray<4, Int> = [1, 2, 4, 8]
      let b: InlineArray<_, Int> = [1, 2, 4, 8]
      let c: InlineArray<4, _>   = [1, 2, 4, 8]
      let d: InlineArray         = [1, 2, 4, 8]
      _checkInlineArray(a, oracle: [1, 2, 4, 8])
      _checkInlineArray(b, oracle: [1, 2, 4, 8])
      _checkInlineArray(c, oracle: [1, 2, 4, 8])
      _checkInlineArray(d, oracle: [1, 2, 4, 8])
    }
    do {
      let a = InlineArray<4, Int> { 1 << $0 }
      let b = InlineArray<4, Int>(first: 1) { $0 << 1 }
      var c = InlineArray<4, Int>(repeating: 9)
      var d = InlineArray<4, Int>(repeating: 9)
      _checkInlineArray(c, oracle: [9, 9, 9, 9])
      _checkInlineArray(d, oracle: [9, 9, 9, 9])
      c[0] = 1
      c[1] = 2
      c[2] = 4
      c[3] = 8
      d[unchecked: 0] = 1
      d[unchecked: 1] = 2
      d[unchecked: 2] = 4
      d[unchecked: 3] = 8
      _checkInlineArray(a, oracle: [1, 2, 4, 8])
      _checkInlineArray(b, oracle: [1, 2, 4, 8])
      _checkInlineArray(c, oracle: [1, 2, 4, 8])
      _checkInlineArray(d, oracle: [1, 2, 4, 8])
    }
  }

  /// An array and its elements can be of *noncopyable* type.
  @available(SwiftStdlib 6.2, *)
  static func testNoncopyable() {
    var a = InlineArray<4, Atomic<Int>> { Atomic(1 << $0) }
    var b = InlineArray<4, Atomic<Int>>(first: Atomic(1)) {
      Atomic($0.load(ordering: .relaxed) << 1)
    }
    let c: InlineArray = [Atomic(8), Atomic(4), Atomic(2), Atomic(1)]
    expectEqual(a.indices, c.indices)
    expectEqual(b.indices, c.indices)
    // Perform in-place reversals to test the `swapAt(_:_:)` method.
    a._reverse()
    b._reverse()
    for i in c.indices {
      expectEqual(
        a[i].load(ordering: .relaxed),
        c[i].load(ordering: .relaxed)
      )
      expectEqual(
        b[i].load(ordering: .relaxed),
        c[i].load(ordering: .relaxed)
      )
      expectEqual(
        a[unchecked: i].load(ordering: .relaxed),
        c[unchecked: i].load(ordering: .relaxed)
      )
      expectEqual(
        b[unchecked: i].load(ordering: .relaxed),
        c[unchecked: i].load(ordering: .relaxed)
      )
    }
  }

  /// An *empty* array's elements can be of *uninhabited* type.
  @available(SwiftStdlib 6.2, *)
  static func testUninhabited() {
#if false // FIXME: Empty array literals aren't supported.
    do {
      let e: InlineArray<0, Never> = []
      let f: InlineArray<_, Never> = []
      _checkInlineArray(e, oracle: [])
      _checkInlineArray(f, oracle: [])
    }
#endif
    do {
      let e = InlineArray<0, Never> { _ in fatalError() }
      let f = InlineArray<0, _>     { _ in fatalError() }
      _checkInlineArray(e, oracle: [])
      _checkInlineArray(f, oracle: [])
    }
  }
}

//===----------------------------------------------------------------------===//
// MARK: - InlineArray Additions
//===----------------------------------------------------------------------===//

@available(SwiftStdlib 6.2, *)
extension InlineArray where Element: ~Copyable {

  /// Reverses the elements of the array in place.
  ///
  /// - Complexity: O(*n*), where *n* is the number of elements in the array.
  @available(SwiftStdlib 6.2, *)
  mutating func _reverse() {
    if isEmpty { return }
    var i = startIndex
    var j = index(before: endIndex)
    while i < j {
      swapAt(i, j)
      i = index(after: i)
      j = index(before: j)
    }
  }
}

//===----------------------------------------------------------------------===//
// MARK: - StdlibUnittest Additions
//===----------------------------------------------------------------------===//

/// Tests the properties and subscripts of an `InlineArray` instance, by
/// comparing them against an `Array` oracle with the expected elements.
@available(SwiftStdlib 6.2, *)
func _checkInlineArray<let count: Int, Element: Equatable>(
  _ inlineArray: InlineArray<count, Element>,
  oracle: [Element],
  stackTrace: SourceLocStack = SourceLocStack(),
  showFrame: Bool = true,
  file: String = #file,
  line: UInt = #line
) {
  // Check the properties.
  expectEqual(
    inlineArray.count,
    oracle.count,
    stackTrace: stackTrace.pushIf(showFrame, file: file, line: line)
  )
  expectEqual(
    inlineArray.isEmpty,
    oracle.isEmpty,
    stackTrace: stackTrace.pushIf(showFrame, file: file, line: line)
  )
  expectEqual(
    inlineArray.startIndex,
    oracle.startIndex,
    stackTrace: stackTrace.pushIf(showFrame, file: file, line: line)
  )
  expectEqual(
    inlineArray.endIndex,
    oracle.endIndex,
    stackTrace: stackTrace.pushIf(showFrame, file: file, line: line)
  )
  expectEqual(
    inlineArray.indices,
    oracle.indices,
    stackTrace: stackTrace.pushIf(showFrame, file: file, line: line)
  )
  // Check the subscripts.
  for (i, j) in zip(inlineArray.indices, oracle.indices) {
    expectEqual(
      inlineArray[i],
      oracle[j],
      stackTrace: stackTrace.pushIf(showFrame, file: file, line: line)
    )
    expectEqual(
      inlineArray[unchecked: i],
      oracle[j],
      stackTrace: stackTrace.pushIf(showFrame, file: file, line: line)
    )
  }
}
