//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

/// A simple bitmap of a fixed number of bits, implementing a sorted set of
/// small nonnegative Int values.
///
/// Because `_UnsafeBitset` implements a flat bit vector, it isn't suitable for
/// holding arbitrarily large integers. The maximal element a bitset can store
/// is fixed at its initialization.
@frozen
@usableFromInline // @testable
@unsafe
internal struct _UnsafeBitset {
  @usableFromInline
  internal let words: UnsafeMutablePointer<Word>

  @usableFromInline
  @safe
  internal let wordCount: Int

  @inlinable
  @inline(__always)
  internal init(words: UnsafeMutablePointer<Word>, wordCount: Int) {
    unsafe self.words = unsafe words
    self.wordCount = wordCount
  }
}

@available(*, unavailable)
extension _UnsafeBitset: Sendable {}

extension _UnsafeBitset {
  @inlinable
  @inline(__always)
  internal static func word(for element: Int) -> Int {
    _internalInvariant(element >= 0)
    // Note: We perform on UInts to get faster unsigned math (shifts).
    let element = UInt(bitPattern: element)
    let capacity = UInt(bitPattern: Word.capacity)
    return Int(bitPattern: element / capacity)
  }

  @inlinable
  @inline(__always)
  internal static func bit(for element: Int) -> Int {
    _internalInvariant(element >= 0)
    // Note: We perform on UInts to get faster unsigned math (masking).
    let element = UInt(bitPattern: element)
    let capacity = UInt(bitPattern: Word.capacity)
    return Int(bitPattern: element % capacity)
  }

  @inlinable
  @inline(__always)
  internal static func split(_ element: Int) -> (word: Int, bit: Int) {
    return unsafe (word(for: element), bit(for: element))
  }

  @inlinable
  @inline(__always)
  internal static func join(word: Int, bit: Int) -> Int {
    _internalInvariant(bit >= 0 && bit < Word.capacity)
    return word &* Word.capacity &+ bit
  }
}

extension _UnsafeBitset {
  @inlinable
  @inline(__always)
  internal static func wordCount(forCapacity capacity: Int) -> Int {
    return unsafe word(for: capacity &+ Word.capacity &- 1)
  }

  @inlinable
  internal var capacity: Int {
    @inline(__always)
    get {
      return wordCount &* Word.capacity
    }
  }

  @inlinable
  @inline(__always)
  internal func isValid(_ element: Int) -> Bool {
    return unsafe element >= 0 && element <= capacity
  }

  @inlinable
  @inline(__always)
  internal func uncheckedContains(_ element: Int) -> Bool {
    unsafe _internalInvariant(isValid(element))
    let (word, bit) = unsafe _UnsafeBitset.split(element)
    return unsafe words[word].uncheckedContains(bit)
  }

  @inlinable
  @inline(__always)
  @discardableResult
  internal func uncheckedInsert(_ element: Int) -> Bool {
    unsafe _internalInvariant(isValid(element))
    let (word, bit) = unsafe _UnsafeBitset.split(element)
    return unsafe words[word].uncheckedInsert(bit)
  }

  @inlinable
  @inline(__always)
  @discardableResult
  internal func uncheckedRemove(_ element: Int) -> Bool {
    unsafe _internalInvariant(isValid(element))
    let (word, bit) = unsafe _UnsafeBitset.split(element)
    return unsafe words[word].uncheckedRemove(bit)
  }

  @inlinable
  @inline(__always)
  internal func clear() {
    unsafe words.update(repeating: .empty, count: wordCount)
  }
}

extension _UnsafeBitset: @unsafe Sequence {
  @usableFromInline
  internal typealias Element = Int

  @inlinable
  internal var count: Int {
    var count = 0
    for w in 0 ..< wordCount {
      unsafe count += words[w].count
    }
    return count
  }

  @inlinable
  internal var underestimatedCount: Int {
    return unsafe count
  }

  @inlinable
  func makeIterator() -> Iterator {
    return unsafe Iterator(self)
  }

  @unsafe
  @usableFromInline
  @frozen
  internal struct Iterator: IteratorProtocol {
    @usableFromInline
    internal let bitset: _UnsafeBitset
    @usableFromInline
    internal var index: Int
    @usableFromInline
    internal var word: Word

    @inlinable
    internal init(_ bitset: _UnsafeBitset) {
      unsafe self.bitset = unsafe bitset
      unsafe self.index = 0
      unsafe self.word = unsafe bitset.wordCount > 0 ? bitset.words[0] : .empty
    }

    @inlinable
    internal mutating func next() -> Int? {
      if let bit = unsafe word.next() {
        return unsafe _UnsafeBitset.join(word: index, bit: bit)
      }
      while unsafe (index + 1) < bitset.wordCount {
        unsafe index += 1
        unsafe word = unsafe bitset.words[index]
        if let bit = unsafe word.next() {
          return unsafe _UnsafeBitset.join(word: index, bit: bit)
        }
      }
      return nil
    }
  }
}

@available(*, unavailable)
extension _UnsafeBitset.Iterator: Sendable {}

////////////////////////////////////////////////////////////////////////////////

extension _UnsafeBitset {
  @frozen
  @usableFromInline
  internal struct Word {
    @usableFromInline
    internal var value: UInt

    @inlinable
    internal init(_ value: UInt) {
      self.value = value
    }
  }
}

extension _UnsafeBitset.Word {
  @inlinable
  internal static var capacity: Int {
    @inline(__always)
    get {
      return UInt.bitWidth
    }
  }

  @inlinable
  @inline(__always)
  internal func uncheckedContains(_ bit: Int) -> Bool {
    _internalInvariant(bit >= 0 && bit < UInt.bitWidth)
    return value & (1 &<< bit) != 0
  }

  @inlinable
  @inline(__always)
  @discardableResult
  internal mutating func uncheckedInsert(_ bit: Int) -> Bool {
    _internalInvariant(bit >= 0 && bit < UInt.bitWidth)
    let mask: UInt = 1 &<< bit
    let inserted = value & mask == 0
    value |= mask
    return inserted
  }

  @inlinable
  @inline(__always)
  @discardableResult
  internal mutating func uncheckedRemove(_ bit: Int) -> Bool {
    _internalInvariant(bit >= 0 && bit < UInt.bitWidth)
    let mask: UInt = 1 &<< bit
    let removed = value & mask != 0
    value &= ~mask
    return removed
  }
}

extension _UnsafeBitset.Word {
  @inlinable
  var minimum: Int? {
    @inline(__always)
    get {
      guard value != 0 else { return nil }
      return value.trailingZeroBitCount
    }
  }

  @inlinable
  var maximum: Int? {
    @inline(__always)
    get {
      guard value != 0 else { return nil }
      return _UnsafeBitset.Word.capacity &- 1 &- value.leadingZeroBitCount
    }
  }

  @inlinable
  var complement: _UnsafeBitset.Word {
    @inline(__always)
    get {
      return _UnsafeBitset.Word(~value)
    }
  }

  @inlinable
  @inline(__always)
  internal func subtracting(elementsBelow bit: Int) -> _UnsafeBitset.Word {
    _internalInvariant(bit >= 0 && bit < _UnsafeBitset.Word.capacity)
    let mask = UInt.max &<< bit
    return _UnsafeBitset.Word(value & mask)
  }

  @inlinable
  @inline(__always)
  internal func intersecting(elementsBelow bit: Int) -> _UnsafeBitset.Word {
    _internalInvariant(bit >= 0 && bit < _UnsafeBitset.Word.capacity)
    let mask: UInt = (1 as UInt &<< bit) &- 1
    return _UnsafeBitset.Word(value & mask)
  }

  @inlinable
  @inline(__always)
  internal func intersecting(elementsAbove bit: Int) -> _UnsafeBitset.Word {
    _internalInvariant(bit >= 0 && bit < _UnsafeBitset.Word.capacity)
    let mask = (UInt.max &<< bit) &<< 1
    return _UnsafeBitset.Word(value & mask)
  }
}

extension _UnsafeBitset.Word {
  @inlinable
  internal static var empty: _UnsafeBitset.Word {
    @inline(__always)
    get {
      return _UnsafeBitset.Word(0)
    }
  }

  @inlinable
  internal static var allBits: _UnsafeBitset.Word {
    @inline(__always)
    get {
      return _UnsafeBitset.Word(UInt.max)
    }
  }
}

// Word implements Sequence by using a copy of itself as its Iterator.
// Iteration with `next()` destroys the word's value; however, this won't cause
// problems in normal use, because `next()` is usually called on a separate
// iterator, not the original word.
extension _UnsafeBitset.Word: @unsafe Sequence, @unsafe IteratorProtocol {

  @usableFromInline
  typealias Element = Int

  @inlinable
  internal var count: Int {
    return value.nonzeroBitCount
  }

  @inlinable
  internal var underestimatedCount: Int {
    return count
  }

  @inlinable
  internal var isEmpty: Bool {
    @inline(__always)
    get {
      return value == 0
    }
  }

  /// Return the index of the lowest set bit in this word,
  /// and also destructively clear it.
  @inlinable
  internal mutating func next() -> Int? {
    guard value != 0 else { return nil }
    let bit = value.trailingZeroBitCount
    value &= value &- 1       // Clear lowest nonzero bit.
    return bit
  }
}

extension _UnsafeBitset {
  @_alwaysEmitIntoClient
  @inline(__always)
  internal static func _withTemporaryUninitializedBitset<R>(
    wordCount: Int,
    body: (_UnsafeBitset) throws -> R
  ) rethrows -> R {
    try unsafe withUnsafeTemporaryAllocation(
      of: _UnsafeBitset.Word.self, capacity: wordCount
    ) { buffer in
      let bitset = unsafe _UnsafeBitset(
        words: buffer.baseAddress!, wordCount: buffer.count)
      return try unsafe body(bitset)
    }
  }

  @_alwaysEmitIntoClient
  @inline(__always)
  internal static func withTemporaryBitset<R>(
    capacity: Int,
    body: (_UnsafeBitset) throws -> R
  ) rethrows -> R {
    let wordCount = unsafe Swift.max(1, Self.wordCount(forCapacity: capacity))
    return try unsafe _withTemporaryUninitializedBitset(
      wordCount: wordCount
    ) { bitset in
      unsafe bitset.clear()
      return try unsafe body(bitset)
    }
  }
}

extension _UnsafeBitset {
  @_alwaysEmitIntoClient
  @inline(__always)
  internal static func withTemporaryCopy<R>(
    of original: _UnsafeBitset,
    body: (_UnsafeBitset) throws -> R
  ) rethrows -> R {
    try unsafe _withTemporaryUninitializedBitset(
      wordCount: original.wordCount
    ) { bitset in
      unsafe bitset.words.initialize(from: original.words, count: original.wordCount)
      return try unsafe body(bitset)
    }
  }
}
