//===--- LimitSequence.swift ----------------------------------*- swift -*-===//
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
//  number of items in its output in various ways.
//
//===----------------------------------------------------------------------===//

import Swift

/// Sequences you wish to use with `LimitSequence` must use an element type
/// that implements this protocol, so that `LimitSequence` can indicate when
/// it omits or truncates the sequence.
@usableFromInline
protocol LimitableElement {
  static func omitted(_: Int) -> Self
  static var truncated: Self { get }
}

/// A `Sequence` that adds the ability to limit the output of another sequence.
@usableFromInline
struct LimitSequence<T: LimitableElement, S: Sequence>: Sequence
  where S.Element == T
{
  /// The element type, which must conform to `LimitableElement`
  @usableFromInline
  typealias Element = T

  /// The source sequence
  @usableFromInline
  typealias Source = S

  var source: Source

  /// The maximum number of items that we want in the output of this sequence.
  /// This includes `.omitted()` and `.truncated` items.
  var limit: Int

  /// The number of items to drop from the head of the sequence.
  var offset: Int

  /// The minimum number of items to capture at the tail end of the input
  /// sequence.  This can be _at most_ `limit - 1`.
  var top: Int

  /// Initialise the `LimitSequence`
  ///
  /// - source: The sequence to draw items from.
  /// - limit:  The maximum number of items of output we desire.
  /// - offset: The number of items to drop from the head of the input sequence.
  /// - top:    The minimum number of items to capture at the tail end of the
  ///           input sequence.
  ///
  /// A `LimitSequence` will read from `source` and emit at most `limit` items,
  /// after discarding the first `offset` items from `source`, including a
  /// minimum of `top` items.
  ///
  /// When `LimitSequence` omits items or truncates the sequence, it will
  /// insert `.omitted(count)` or `.truncated` items into its output.
  @usableFromInline
  init(_ source: Source, limit: Int, offset: Int = 0, top: Int = 0) {
    self.source = source
    self.limit = limit
    self.offset = offset
    self.top = top
  }

  /// Create an iterator for this sequence.
  public func makeIterator() -> Iterator {
    return Iterator(source.makeIterator(), limit: limit, offset: offset, top: top)
  }

  /// The `LimitSequence` Iterator implementation.
  ///
  /// This works by buffering an element ahead of where we are in the input
  /// sequence, so that it can tell whether or not there is more input to
  /// follow at any given point.
  @usableFromInline
  struct Iterator: IteratorProtocol {
    /// The iterator for the input sequence.
    var iterator: Source.Iterator

    /// We read one element ahead in the input sequence; that element is
    /// stored here.
    var readAhead: Element?

    /// Tracks the number of items emitted before getting to `top`.
    var count = 0

    /// The maximum number of items to emit, including the `.truncated`
    /// or `.omitted()` markers.
    var limit: Int

    /// The minimum number of items to capture from the tail of the input
    /// sequence.  Must be strictly less than `limit`.
    var top: Int

    /// A ring buffer that we use to capture the tail.
    var topBuffer: [Element]

    /// Points at the first item in `topBuffer`.
    var topBase: Int

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

    /// Initialise the iterator, and fill in the first read ahead element.
    init(_ iterator: Source.Iterator, limit: Int, offset: Int, top: Int) {
      self.iterator = iterator

      for _ in 0..<offset {
        if self.iterator.next() == nil {
          break
        }
      }

      self.readAhead = nil
      self.limit = limit
      self.top = Swift.min(top, limit - 1)
      self.state = .normal
      self.topBuffer = []
      self.topBuffer.reserveCapacity(top)
      self.topBase = 0
      self.topNdx = 0

      readNext()
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
      while let elt = readAhead, topBuffer.count < top{
        topBuffer.append(elt)

        readNext()
      }

      if readAhead == nil {
        // No elements means we just output beforeTop and we're done
        if topBuffer.count == 0 {
          state = .done
          return beforeTop
        }

        // Otherwise, output beforeTop and then the top buffer
        topNdx = 0
        if topBuffer.count < top {
          topBase = topBuffer.count
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
}
