//===--- BorrowingSequence.swift --------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

// RUN: %target-run-stdlib-swift(-enable-experimental-feature SuppressedAssociatedTypesWithDefaults -enable-experimental-feature BorrowingSequence)

// REQUIRES: executable_test
// REQUIRES: swift_feature_SuppressedAssociatedTypesWithDefaults
// REQUIRES: swift_feature_BorrowingSequence

import StdlibUnittest

var suite = TestSuite("BorrowingSequence Tests")
defer { runAllTests() }

@available(SwiftStdlib 6.4, *)
extension BorrowingSequence where Self: ~Copyable & ~Escapable, Element: ~Copyable {
  func reduce<T: ~Copyable>(
    _ initial: consuming T,
    _ nextPartialResult: @escaping (consuming T, borrowing Element) -> T
  ) -> T {
    var borrowIterator = makeBorrowingIterator()
    var result = initial
    while true {
      let span = borrowIterator.nextSpan(maximumCount: .max)
      if span.isEmpty { break }
      for i in span.indices {
        result = nextPartialResult(result, span[i])
      }
    }
    return result
  }
  
  func reduce<T: ~Copyable>(
    into initial: consuming T,
    _ nextPartialResult: (inout T, borrowing Element) -> Void
  ) -> T {
    var borrowIterator = makeBorrowingIterator()
    var result = initial
    while true {
      let span = borrowIterator.nextSpan(maximumCount: .max)
      if span.isEmpty { break }
      for i in span.indices {
        nextPartialResult(&result, span[i])
      }
    }
    return result
  }
}

@available(SwiftStdlib 6.4, *)
extension BorrowingSequence where Self: ~Copyable & ~Escapable, Element: Copyable {
  func collectViaBorrowing() -> [Element] {
    var borrowIterator = makeBorrowingIterator()
    var result: [Element] = []
    while true {
      let span = borrowIterator.nextSpan(maximumCount: .max)
      if span.isEmpty { break }
      for i in span.indices {
        result.append(span[i])
      }
    }
    return result
  }
}

struct NoncopyableInt: ~Copyable, Equatable {
  var value: Int

  static func ==(lhs: borrowing Self, rhs: borrowing Self) -> Bool {
    lhs.value == rhs.value
  }
}

@available(SwiftStdlib 6.4, *)
extension BorrowingSequence where Self: ~Escapable & ~Copyable, Element: Equatable & ~Copyable {
  func elementsEqual<S: BorrowingSequence<Element>>(
    _ rhs: borrowing S
  ) -> Bool
    where S: ~Escapable & ~Copyable, S.Element: ~Copyable
  {
    var iter1 = makeBorrowingIterator()
    var iter2 = rhs.makeBorrowingIterator()
    while true {
      var el1 = iter1.nextSpan(maximumCount: .max)

      if el1.isEmpty {
        // LHS is empty - sequences are equal iff RHS is also empty
        let el2 = iter2.nextSpan(maximumCount: 1)
        return el2.isEmpty
      }

      while el1.count > 0 {
        let el2 = iter2.nextSpan(maximumCount: el1.count)
        if el2.isEmpty { return false }
        for i in 0..<el2.count {
          if el1[i] != el2[i] { return false }
        }
        el1 = el1.extracting(droppingFirst: el2.count)
      }
    }
  }
}

suite.test("BORROWING")
.require(.stdlib_6_4).code {
  guard #available(SwiftStdlib 6.4, *) else {
    expectTrue(false)
    return
  }

  let array = [1, 2, 3, 4, 5, 6, 7, 8]

  let span = array.span
  let spanCollected = span.collectViaBorrowing()
  expectTrue(span.elementsEqual(span))
//  expectTrue(span.elementsEqual(spanCollected))
//  expectTrue(spanCollected.elementsEqual(span))
  expectTrue(spanCollected.elementsEqual(spanCollected))
  expectEqual(array.reduce(0, +), span.reduce(0, +))
  expectEqual(array.reduce(into: 0, +=), span.reduce(into: 0, +=))

  let inline: [8 of Int] = [1, 2, 3, 4, 5, 6, 7, 8]
  let inlineCollected = inline.collectViaBorrowing()
  expectTrue(inline.elementsEqual(inline))
//  expectTrue(inline.elementsEqual(inlineCollected))
//  expectTrue(inlineCollected.elementsEqual(inline))
  expectTrue(inlineCollected.elementsEqual(inlineCollected))

  let nocopyInline: [8 of NoncopyableInt] = InlineArray(NoncopyableInt.init(value:))
  let nocopyBuffer = UnsafeMutableBufferPointer<NoncopyableInt>.allocate(capacity: 8)
  for i in 0..<8 {
    nocopyBuffer.initializeElement(at: i, to: NoncopyableInt(value: i))
  }
  expectTrue(nocopyInline.elementsEqual(nocopyInline))
//  expectTrue(nocopyInline.elementsEqual(nocopyBuffer))
//  expectTrue(nocopyBuffer.elementsEqual(nocopyBuffer))
//  expectTrue(nocopyBuffer.elementsEqual(nocopyInline))
}
