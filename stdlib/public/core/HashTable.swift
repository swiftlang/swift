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

@usableFromInline
internal protocol _HashTableDelegate {
  func hashValue(at index: _HashTable.Bucket) -> Int
  func moveEntry(from source: _HashTable.Bucket, to target: _HashTable.Bucket)
}

@usableFromInline
@_fixed_layout
internal struct _HashTable {
  @usableFromInline
  internal typealias Word = _UnsafeBitset.Word

  @usableFromInline
  internal var words: UnsafeMutablePointer<Word>

  @usableFromInline
  internal let bucketMask: Int

  @inlinable
  @inline(__always)
  internal init(words: UnsafeMutablePointer<Word>, bucketCount: Int) {
    _sanityCheck(bucketCount > 0 && bucketCount & (bucketCount - 1) == 0,
      "bucketCount must be a power of two")
    self.words = words
    // The bucket count is a power of two, so subtracting 1 will never overflow
    // and get us a nice mask.
    self.bucketMask = bucketCount &- 1
  }

  @inlinable
  internal var bucketCount: Int {
    @inline(__always) get {
      return bucketMask &+ 1
    }
  }

  @inlinable
  internal var wordCount: Int {
    @inline(__always) get {
      return _UnsafeBitset.wordCount(forCapacity: bucketCount)
    }
  }
}

extension _HashTable {
  /// The inverse of the maximum hash table load factor.
  private static var maxLoadFactor: Double {
    @inline(__always) get { return 3 / 4 }
  }

  internal static func capacity(forScale scale: Int8) -> Int {
    let bucketCount = (1 as Int) &<< scale
    return Int(Double(bucketCount) * maxLoadFactor)
  }

  internal static func scale(forCapacity capacity: Int) -> Int8 {
    let capacity = Swift.max(capacity, 1)
    // Calculate the minimum number of entries we need to allocate to satisfy
    // the maximum load factor. `capacity + 1` below ensures that we always
    // leave at least one hole.
    let minimumEntries = Swift.max(
      Int((Double(capacity) / maxLoadFactor).rounded(.up)),
      capacity + 1)
    // The actual number of entries we need to allocate is the lowest power of
    // two greater than or equal to the minimum entry count. Calculate its
    // exponent.
    let exponent = (Swift.max(minimumEntries, 2) - 1)._binaryLogarithm() + 1
    _sanityCheck(exponent >= 0 && exponent < Int.bitWidth)
    // The scale is the exponent corresponding to the bucket count.
    let scale = Int8(truncatingIfNeeded: exponent)
    _sanityCheck(self.capacity(forScale: scale) >= capacity)
    return scale
  }
}

extension _HashTable {
  @usableFromInline
  internal typealias Index = Bucket // FIXME: Remove

  @_fixed_layout
  @usableFromInline
  internal struct Bucket {
    @usableFromInline
    internal var bucket: Int // FIXME: Rename to offset

    @inlinable
    @inline(__always)
    internal init(bucket: Int) {
      _sanityCheck(bucket >= 0)
      self.bucket = bucket
    }

    @inlinable
    @inline(__always)
    internal init(word: Int, bit: Int) {
      self.bucket = _UnsafeBitset.join(word: word, bit: bit)
    }

    @inlinable
    internal var word: Int {
      @inline(__always) get {
        return _UnsafeBitset.word(for: bucket)
      }
    }

    @inlinable
    internal var bit: Int {
      @inline(__always) get {
        return _UnsafeBitset.bit(for: bucket)
      }
    }
  }
}

extension _HashTable.Bucket: Equatable {
  @inlinable
  @inline(__always)
  internal
  static func == (lhs: _HashTable.Bucket, rhs: _HashTable.Bucket) -> Bool {
    return lhs.bucket == rhs.bucket
  }
}

extension _HashTable.Bucket: Comparable {
  @inlinable
  @inline(__always)
  internal
  static func < (lhs: _HashTable.Bucket, rhs: _HashTable.Bucket) -> Bool {
    return lhs.bucket < rhs.bucket
  }
}

extension _HashTable {
  @usableFromInline
  @_fixed_layout
  internal struct AgedIndex { // FIXME: Rename to Index
    @usableFromInline
    let bucket: Bucket

    @usableFromInline
    let age: Int32

    @inlinable
    internal init(bucket: Bucket, age: Int32) {
      self.bucket = bucket
      self.age = age
    }
  }
}

extension _HashTable.AgedIndex: Equatable {
  @inlinable
  internal static func ==(
    lhs: _HashTable.AgedIndex,
    rhs: _HashTable.AgedIndex
  ) -> Bool {
    _precondition(lhs.age == rhs.age,
      "Can't compare indices belonging to different collections")
    return lhs.bucket == rhs.bucket
  }
}

extension _HashTable.AgedIndex: Comparable {
  @inlinable
  internal static func <(
    lhs: _HashTable.AgedIndex,
    rhs: _HashTable.AgedIndex
  ) -> Bool {
    _precondition(lhs.age == rhs.age,
      "Can't compare indices belonging to different collections")
    return lhs.bucket < rhs.bucket
  }
}

extension _HashTable: Sequence {
  @usableFromInline
  @_fixed_layout
  internal struct Iterator: IteratorProtocol {
    @usableFromInline
    let hashTable: _HashTable
    @usableFromInline
    var wordIndex: Int
    @usableFromInline
    var word: Word

    @inlinable
    init(_ hashTable: _HashTable) {
      self.hashTable = hashTable
      self.wordIndex = 0
      self.word = hashTable.words[0]
      if hashTable.bucketCount < Word.capacity {
        self.word = self.word.intersecting(elementsBelow: hashTable.bucketCount)
      }
    }

    @inlinable
    @inline(__always)
    internal mutating func next() -> Index? {
      if let bit = word.next() {
        return Index(word: wordIndex, bit: bit)
      }
      while wordIndex + 1 < hashTable.wordCount {
        wordIndex += 1
        word = hashTable.words[wordIndex]
        if let bit = word.next() {
          return Index(word: wordIndex, bit: bit)
        }
      }
      return nil
    }
  }

  @inlinable
  internal func makeIterator() -> Iterator {
    return Iterator(self)
  }
}

extension _HashTable {
  @inlinable
  @inline(__always)
  internal func isValid(_ index: Index) -> Bool {
    return index.bucket >= 0 && index.bucket < bucketCount
  }

  @inlinable
  @inline(__always)
  internal func _isOccupied(_ index: Index) -> Bool {
    _sanityCheck(isValid(index))
    return words[index.word].uncheckedContains(index.bit)
  }

  @inlinable
  @inline(__always)
  internal func isOccupied(_ index: Index) -> Bool {
    return isValid(index) && _isOccupied(index)
  }

  @inlinable
  @inline(__always)
  internal func checkOccupied(_ i: Index) {
    _precondition(isOccupied(i),
      "Attempting to access Collection elements using an invalid Index")
  }

  @inlinable
  @inline(__always)
  internal func _firstOccupiedIndex(fromWord word: Int) -> Index {
    _sanityCheck(word >= 0 && word <= wordCount)
    var word = word
    while word < wordCount {
      if let bit = words[word].minimum {
        return Index(word: word, bit: bit)
      }
      word += 1
    }
    return endIndex
  }

  @inlinable
  internal func index(after index: Index) -> Index {
    _sanityCheck(isValid(index))
    let word = index.word
    if let bit = words[word].intersecting(elementsAbove: index.bit).minimum {
      return Index(word: word, bit: bit)
    }
    return _firstOccupiedIndex(fromWord: word + 1)
  }

  @inlinable
  internal var startIndex: Index {
    return _firstOccupiedIndex(fromWord: 0)
  }

  @inlinable
  internal var endIndex: Index {
    @inline(__always)
    get {
      return Index(bucket: bucketCount)
    }
  }
}

extension _HashTable {
  @inlinable
  @inline(__always)
  internal func idealIndex(forHashValue hashValue: Int) -> Index {
    return Index(bucket: hashValue & bucketMask)
  }

  /// The next bucket after `bucket`, with wraparound at the end of the table.
  @inlinable
  @inline(__always)
  internal func index(wrappedAfter index: Index) -> Index {
    // The bucket is less than bucketCount, which is power of two less than
    // Int.max. Therefore adding 1 does not overflow.
    return Index(bucket: (index.bucket &+ 1) & bucketMask)
  }
}

extension _HashTable {
  @inlinable
  internal func previousHole(before index: Index) -> Index {
    _sanityCheck(isValid(index))
    // Note that if we have only a single partial word, its out-of-bounds bits
    // are guaranteed to be all set, so the formula below gives correct results.
    var word = index.word
    if let bit =
      words[word]
        .complement
        .intersecting(elementsBelow: index.bit)
        .maximum {
      return Index(word: word, bit: bit)
    }
    var wrap = false
    while true {
      word -= 1
      if word < 0 {
        _precondition(!wrap, "Hash table has no holes")
        wrap = true
        word = wordCount - 1
      }
      if let bit = words[word].complement.maximum {
        return Index(word: word, bit: bit)
      }
    }
  }

  @inlinable
  internal func nextHole(atOrAfter index: Index) -> Index {
    _sanityCheck(isValid(index))
    // Note that if we have only a single partial word, its out-of-bounds bits
    // are guaranteed to be all set, so the formula below gives correct results.
    var word = index.word
    if let bit =
      words[word]
        .complement
        .subtracting(elementsBelow: index.bit)
        .minimum {
      return Index(word: word, bit: bit)
    }
    var wrap = false
    while true {
      word += 1
      if word == wordCount {
        _precondition(!wrap, "Hash table has no holes")
        wrap = true
        word = 0
      }
      if let bit = words[word].complement.minimum {
        return Index(word: word, bit: bit)
      }
    }
  }
}

extension _HashTable {
  @inlinable
  @inline(__always)
  @_effects(releasenone)
  internal func copyContents(of other: _HashTable) {
    _sanityCheck(bucketCount == other.bucketCount)
    self.words.assign(from: other.words, count: bucketCount)
  }

  /// Insert a new entry with the specified hash value into the table.
  /// The entry must not already exist in the table -- duplicates are ignored.
  @inlinable
  @inline(__always)
  internal func insertNew(hashValue: Int) -> Index {
    let hole = nextHole(atOrAfter: idealIndex(forHashValue: hashValue))
    insert(hole)
    return hole
  }

  /// Insert a new entry for an element at `index`.
  @inlinable
  @inline(__always)
  internal func insert(_ index: Index) {
    _sanityCheck(!isOccupied(index))
    words[index.word].uncheckedInsert(index.bit)
  }

  @inlinable
  @inline(__always)
  internal func clear() {
    if bucketCount < Word.capacity {
      // We have only a single partial word. Set all out of bounds bits, so that
      // `index(after:)` and `nextHole(atOrAfter:)` works correctly without a
      // special case.
      words[0] = Word.allBits.subtracting(elementsBelow: bucketCount)
    } else {
      words.assign(repeating: .empty, count: wordCount)
    }
  }

  @inline(__always)
  @inlinable
  internal func delete<D: _HashTableDelegate>(
    at index: Index,
    with delegate: D
  ) {
    _sanityCheck(isOccupied(index))

    // If we've put a hole in a chain of contiguous elements, some element after
    // the hole may belong where the new hole is.

    var hole = index
    var candidate = self.index(wrappedAfter: hole)

    guard _isOccupied(candidate) else {
      // Fast path: Don't get the first bucket when there's nothing to do.
      words[hole.word].uncheckedRemove(hole.bit)
      return
    }

    // Find the first bucket in the contiguous chain that contains the entry
    // we've just deleted.
    let start = self.index(wrappedAfter: previousHole(before: index))

    // Relocate out-of-place elements in the chain, repeating until we get to
    // the end of the chain.
    while _isOccupied(candidate) {
      let candidateHash = delegate.hashValue(at: candidate)
      let ideal = idealIndex(forHashValue: candidateHash)

      // Does this element belong between start and hole?  We need two
      // separate tests depending on whether [start, hole] wraps around the
      // end of the storage.
      let c0 = ideal >= start
      let c1 = ideal <= hole
      if start <= hole ? (c0 && c1) : (c0 || c1) {
        delegate.moveEntry(from: candidate, to: hole)
        hole = candidate
      }
      candidate = self.index(wrappedAfter: candidate)
    }

    words[hole.word].uncheckedRemove(hole.bit)
  }
}

extension _HashTable {
  /// Check for consistency and return the count of occupied entries.
  internal func _invariantCheck(with delegate: _HashTableDelegate) -> Int {
#if INTERNAL_CHECKS_ENABLED
    _sanityCheck(bucketCount > 0 && bucketCount & (bucketCount &- 1) == 0,
      "Invalid bucketCount")
    _sanityCheck(_isValidAddress(UInt(bitPattern: words)),
      "Invalid words pointer")
    _sanityCheck(_isValidAddress(UInt(bitPattern: words + wordCount - 1)),
      "Invalid words buffer")

    var occupiedCount = 0
    for i in self {
      occupiedCount += 1
      let hashValue = delegate.hashValue(at: i)
      var c = idealIndex(forHashValue: hashValue)
      // There must be no holes between the ideal and actual buckets for this
      // hash value.
      while c != i {
        _sanityCheck(_isOccupied(c),
          "Some hash table elements are stored outside their collision chain")
        c = index(wrappedAfter: c)
      }
    }
    return occupiedCount
#else
    return 0
#endif
  }
}
