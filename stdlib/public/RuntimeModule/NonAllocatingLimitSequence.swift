//===--- NonAllocatingLimitSequence.swift ---------------------*- swift -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  Defines a sequence adapter that implements the ability to limit the
//  number of items in its output in various ways.  Unlike `LimitSequence`,
//  this sequence is non-allocating and therefore requires that the user
//  provide a buffer for top-of-stack capture.
//
//===----------------------------------------------------------------------===//

import Swift

/// A `Sequence` that adds the ability to limit the output of another sequence.
@usableFromInline
struct NonAllocatingLimitSequence<T: LimitableElement, S: Sequence>
  : Sequence, IteratorProtocol
  where S.Element == T
{
  /// The element type, which must conform to `LimitableElement`
  @usableFromInline
  typealias Element = T

  @usableFromInline
  typealias Source = S

  /// The source iterator
  var iterator: Source.Iterator

  /// The maximum number of items that we want in the output of this sequence.
  /// This includes `.omitted()` and `.truncated` items.
  var limit: Int

  /// The number of items to drop from the head of the sequence.
  var offset: Int

  /// The minimum number of items to capture at the tail end of the input
  /// sequence.  This can be _at most_ `limit - 1`.
  var top: Int

  /// The buffer to use to deal with top-of-stack capture.
  var topBuffer: UnsafeMutableBufferPointer<T>

  /// Initialise the `NonAllocatingLimitSequence`
  ///
  /// - source: The sequence to draw items from.
  /// - limit:  The maximum number of items of output we desire.
  /// - offset: The number of items to drop from the head of the input sequence.
  /// - top:    The minimum number of items to capture at the tail end of the
  ///           input sequence.
  ///
  /// A `NonAllocatingLimitSequence` will read from `source` and emit at most `limit` items,
  /// after discarding the first `offset` items from `source`, including a
  /// minimum of `top` items.
  ///
  /// When `NonAllocatingLimitSequence` omits items or truncates the sequence, it will
  /// insert `.omitted(count)` or `.truncated` items into its output.
  @usableFromInline
  init(_ source: Source, limit: Int, offset: Int = 0, top: Int = 0,
       topBuffer: UnsafeMutableBufferPointer<T>) {
    self.iterator = source.makeIterator()
    self.limit = limit
    self.offset = offset
    self.top = top
    self.topBuffer = topBuffer
    self.readAhead = nil
    self.state = .normal
    self.topCount = 0
    self.topBase = 0
    self.topNdx = 0

    for _ in 0..<offset {
      if self.iterator.next() == nil {
        break
      }
    }

    readNext()
  }

  /// We read one element ahead in the input sequence; that element is
  /// stored here.
  var readAhead: Element?

  /// Tracks the number of items emitted before getting to `top`.
  var count = 0

  /// Points at the first item in `topBuffer`.
  var topBase: Int

  /// A count of the number of items in `topBuffer`.
  var topCount: Int

  /// The index in `topBuffer` that we should output from the next
  /// call to `next()`.
  var topNdx: Int

  /// Tracks the iterator state.
  var state: State

  enum State {
    case normal
    case outputTop
    case done
  }

  /// Fill `readAhead` with the next element from the input sequence.
  private mutating func readNext() {
    if let elt = self.iterator.next() {
      readAhead = elt
    } else {
      readAhead = nil
    }
  }

  /// Retrieve the next element in the output sequence.
  public mutating func next() -> Element? {
    switch state {
      case .done:
        return nil
      case .outputTop:
        let result = topBuffer[topNdx]
        topNdx += 1
        if topNdx == top {
          topNdx = 0
        }
        if topNdx == topBase {
          state = .done
        }
        return result
      case .normal:
        break
    }

    guard let element = readAhead else {
      state = .done
      return nil
    }

    readNext()

    // Capture the easy part
    if count < limit - top - 1 {
      count += 1
      return element
    }

    if top == 0 && readAhead != nil {
      state = .done
      return .truncated
    }

    let beforeTop = element

    // Fill the top buffer
    while let elt = readAhead, topCount < top {
      topBuffer[topCount] = elt
      topCount += 1

      readNext()
    }

    if readAhead == nil {
      // No elements means we just output beforeTop and we're done
      if topCount == 0 {
        state = .done
        return beforeTop
      }

      // Otherwise, output beforeTop and then the top buffer
      topNdx = 0
      if topCount < top {
        topBase = topCount
      }
      state = .outputTop
      return beforeTop
    }

    // Use the top buffer as a circular buffer
    var omitted = 1
    while let elt = readAhead {
      topBuffer[topBase] = elt
      topBase += 1
      omitted += 1
      if topBase == top {
        topBase = 0
      }

      readNext()
    }

    topNdx = topBase
    state = .outputTop
    return .omitted(omitted)
  }
}
