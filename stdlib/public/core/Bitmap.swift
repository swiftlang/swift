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

/// A simple bitmap of a fixed number of bits.
@_fixed_layout
@usableFromInline
internal struct _Bitmap {
  /// The first word's worth of bits.
  @usableFromInline
  internal var _word0: Word

  /// The storage holding the rest of the bits.
  /// Only allocated when we have more bits than can fit in an UInt.
  @usableFromInline
  internal var _storage: Storage?

  @inlinable
  internal init(bitCount: Int) {
    _word0 = Word(0)
    if bitCount > UInt.bitWidth {
      _storage = Storage.allocate(bitCount: bitCount - Word.bitWidth)
    }
  }
}

extension _Bitmap {
  @inlinable
  internal var _wordCount: Int {
    @inline(__always) get {
      return 1 + (_storage?._wordCount ?? 0)
    }
  }

  @inlinable
  internal var _bitCount: Int {
    @inline(__always) get {
      return _wordCount * Word.bitWidth
    }
  }

  @inlinable
  @inline(__always)
  internal func _isValid(_ element: Int) -> Bool {
    return element >= 0 && element <= _bitCount
  }

  @inlinable
  @inline(__always)
  internal mutating func isUniquelyReferenced() -> Bool {
    return _isUnique_native(&_storage)
  }

  @inlinable
  @inline(__always)
  internal mutating func ensureUnique() {
    let isUnique = isUniquelyReferenced()
    guard !isUnique, let storage = _storage else { return }
    _storage = storage.copy()
  }
}

extension _Bitmap {
  @inlinable
  internal func contains(_ element: Int) -> Bool {
    precondition(_isValid(element), "Value out of bounds")
    return _uncheckedContains(element)
  }

  @inlinable
  @inline(__always)
  internal func _uncheckedContains(_ element: Int) -> Bool {
    _sanityCheck(_isValid(element), "Value out of bounds")
    let (word, bit) = Word._split(element)
    if word == 0 {
      return _word0._uncheckedContains(bit)
    }
    return _storage![_unchecked: word - 1]._uncheckedContains(bit)
  }

  @inlinable
  @discardableResult
  internal mutating func insert(
    _ element: Int
  ) -> (inserted: Bool, memberAfterInsert: Int) {
    precondition(_isValid(element), "Value out of bounds")
    return (_uncheckedInsert(element), element)
  }

  @inlinable
  @inline(__always)
  @discardableResult
  internal mutating func _uncheckedInsert(_ element: Int) -> Bool {
    _sanityCheck(_isValid(element), "Value out of bounds")
    let (word, bit) = Word._split(element)
    if word == 0 {
      return _word0._uncheckedInsert(bit)
    }
    ensureUnique()
    return _storage![_unchecked: word - 1]._uncheckedInsert(bit)
  }

  @inlinable
  @discardableResult
  internal mutating func remove(_ element: Int) -> Int? {
    precondition(_isValid(element), "Value out of bounds")
    return _uncheckedRemove(element) ? element : nil
  }

  @inlinable
  @inline(__always)
  @discardableResult
  internal mutating func _uncheckedRemove(_ element: Int) -> Bool {
    _sanityCheck(_isValid(element), "Value out of bounds")
    let (word, bit) = Word._split(element)
    if word == 0 {
      return _word0._uncheckedRemove(bit)
    }
    ensureUnique()
    return _storage![_unchecked: word - 1]._uncheckedRemove(bit)
  }
}

extension _Bitmap {
  @_fixed_layout
  @usableFromInline
  internal struct Word {
    @usableFromInline
    internal var _value: UInt

    @inlinable
    internal init(_ value: UInt) {
      self._value = value
    }
  }
}

extension _Bitmap.Word {
  @inlinable
  internal static var bitWidth: Int {
    @inline(__always)
    get {
      return UInt.bitWidth
    }
  }

  @inlinable
  @inline(__always)
  internal static func _split(_ value: Int) -> (word: Int, bit: Int) {
    return (
      word: value / UInt.bitWidth,
      bit: value % UInt.bitWidth)
  }

  @inlinable
  @inline(__always)
  internal func _uncheckedContains(_ bit: Int) -> Bool {
    _sanityCheck(bit >= 0 && bit < UInt.bitWidth)
    return _value & (1 << bit) != 0
  }

  @inlinable
  @inline(__always)
  internal mutating func _uncheckedInsert(_ bit: Int) -> Bool {
    _sanityCheck(bit >= 0 && bit < UInt.bitWidth)
    let mask: UInt = 1 << bit
    let inserted = _value & mask == 0
    _value |= mask
    return inserted
  }

  @inlinable
  @inline(__always)
  internal mutating func _uncheckedRemove(_ bit: Int) -> Bool {
    _sanityCheck(bit >= 0 && bit < UInt.bitWidth)
    let mask: UInt = 1 << bit
    let removed = _value & mask != 0
    _value &= ~mask
    return removed
  }
}

extension _Bitmap.Word: Sequence, IteratorProtocol {
  @inlinable
  internal var count: Int {
    return _value.nonzeroBitCount
  }

  @inlinable
  internal var underestimatedCount: Int {
    return count
  }

  @inlinable
  internal mutating func next() -> Int? {
    guard _value != 0 else { return nil }
    let bit = _value.trailingZeroBitCount
    _value &= ~((1 as UInt) << bit)
    return bit
  }
}

extension _Bitmap {
  /// A simple bitmap storage class with room for a specific number of
  /// tail-allocated bits.
  @_fixed_layout
  @usableFromInline
  internal final class Storage {
    @usableFromInline
    internal fileprivate(set) var _wordCount: Int
    internal init(_doNotCall: ()) {
      _sanityCheckFailure("This class cannot be directly initialized")
    }
  }
}

extension _Bitmap.Storage {
  @usableFromInline
  internal typealias Word = _Bitmap.Word

  @inlinable
  @inline(__always)
  internal static func wordCount(forBitCount bitCount: Int) -> Int {
    return (bitCount + Word.bitWidth - 1) / Word.bitWidth
  }

  internal static func _allocateUninitialized(
    wordCount: Int
  ) -> _Bitmap.Storage {
    let storage = Builtin.allocWithTailElems_1(
      _Bitmap.Storage.self,
      wordCount._builtinWordValue, Word.self)
    storage._wordCount = wordCount
    return storage
  }

  @usableFromInline
  @_effects(releasenone)
  internal static func allocate(bitCount: Int) -> _Bitmap.Storage {
    let wordCount = _Bitmap.Storage.wordCount(forBitCount: bitCount)
    let storage = _allocateUninitialized(wordCount: wordCount)
    storage._words.initialize(repeating: Word(0), count: storage._wordCount)
    return storage
  }

  @usableFromInline
  @_effects(releasenone)
  internal func copy() -> _Bitmap.Storage {
    let storage = _Bitmap.Storage._allocateUninitialized(wordCount: _wordCount)
    storage._words.initialize(from: self._words, count: storage._wordCount)
    return storage
  }

  @inlinable
  internal var _words: UnsafeMutablePointer<Word> {
    @inline(__always)
    get {
      let addr = Builtin.projectTailElems(self, Word.self)
      return UnsafeMutablePointer(addr)
    }
  }

  @inlinable
  @inline(__always)
  internal func _isValid(_ word: Int) -> Bool {
    return word >= 0 && word < _wordCount
  }

  @inlinable
  internal subscript(_unchecked word: Int) -> Word {
    @inline(__always)
    get {
      _sanityCheck(_isValid(word))
      return _words[word]
    }
    @inline(__always)
    set {
      _sanityCheck(_isValid(word))
      _words[word] = newValue
    }
  }

  @inlinable
  internal var count: Int {
    var count = 0
    for word in 0 ..< _wordCount {
      count += _words[word].count
    }
    return count
  }
}

extension _Bitmap: Sequence {
  @usableFromInline
  internal typealias Element = Int

  @inlinable
  internal var count: Int {
    var count = _word0.count
    guard let storage = _storage else { return count }
    for w in 0 ..< storage._wordCount {
      count += storage._words[w].count
    }
    return count
  }

  @inlinable
  internal var underestimatedCount: Int {
    return count
  }

  @inlinable
  func makeIterator() -> Iterator {
    return Iterator(self)
  }

  @usableFromInline
  @_fixed_layout
  internal struct Iterator: IteratorProtocol {
    @usableFromInline
    internal var _word: Word
    @usableFromInline
    internal var _wordIndex: Int
    @usableFromInline
    internal let _storage: Storage?

    @inlinable
    internal init(_ bitmap: _Bitmap) {
      self._word = bitmap._word0
      self._wordIndex = 0
      self._storage = bitmap._storage
    }

    @inlinable
    internal mutating func next() -> Int? {
      if let v = _word.next() {
        return _wordIndex * Word.bitWidth + v
      }
      guard let storage = _storage else { return nil }
      while _wordIndex < storage._wordCount {
        _word = storage._words[_wordIndex]
        // Note that _wordIndex is offset by 1 due to word0;
        // this is why the index needs to be incremented at exactly this point.
        _wordIndex += 1
        if let v = _word.next() {
          return _wordIndex * Word.bitWidth + v
        }
      }
      return nil
    }
  }
}

