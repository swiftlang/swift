//===--- BorrowingSequence.swift ------------------------------------------===//
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

// RUN: %target-run-stdlib-swift(-enable-experimental-feature SuppressedAssociatedTypesWithDefaults -enable-experimental-feature BorrowingSequence -enable-experimental-feature Lifetimes)

// REQUIRES: executable_test
// REQUIRES: swift_feature_SuppressedAssociatedTypesWithDefaults
// REQUIRES: swift_feature_BorrowingSequence

import StdlibUnittest

var suite = TestSuite("BorrowingSequence Tests")
defer { runAllTests() }

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

  
  // prefix(while:) used on an iterator
  var prefixed = span
    .makeBorrowingIterator()
//    .borrowing
    .prefix(while: { $0 < 5 })
  
  let prefixSpan = prefixed.nextSpan(maximumCount: .max)
  expectEqual(prefixSpan.reduce(0, +), 10)

// prefix(while:) used on a borrowingsequence
// note: this doesn't work
//  let prefix = span.prefix(while: { $0 < 5 })
//  expectEqual(prefix.reduce(0, +), 10)
  
  var prefixed4 = span
    .makeBorrowingIterator()
    .prefix(4)
  let prefixed4Span = prefixed4.nextSpan(maximumCount: .max)
  expectEqual(prefixed4Span.reduce(0, +), 10)

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





protocol BarBorrowable: ~Copyable, ~Escapable {
    @_lifetime(borrow self)
    func borrowBar() -> Bar
}

extension BarBorrowable where Self: ~Copyable & ~Escapable {
    var borrowedBar: Bar {
        @_lifetime(borrow self)
        get {
            borrowBar()
        }
    }
}

struct Foo: BarBorrowable, ~Copyable, ~Escapable {
    @_lifetime(immortal)
    init() {}
    
    @_lifetime(borrow self)
    func borrowBar() -> Bar {
        Bar(borrowing: self)
    }
}

struct Bar: ~Copyable, ~Escapable {
    @_lifetime(borrow foo)
    init(borrowing foo: borrowing Foo) {}
    
    consuming func doTheThing() -> Int { 0 }
}

func baz() {
    let foo = Foo()
    
    let v = foo.borrowBar().doTheThing()
    let u = foo.borrowedBar.doTheThing()
}
