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
  func hashValue(at bucket: _HashTable.Bucket) -> Int
  func moveEntry(from source: _HashTable.Bucket, to target: _HashTable.Bucket)
}

@usableFromInline
@frozen
@unsafe
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
    _internalInvariant(bucketCount > 0 && bucketCount & (bucketCount - 1) == 0,
      "bucketCount must be a power of two")
    unsafe self.words = unsafe words
    // The bucket count is a power of two, so subtracting 1 will never overflow
    // and get us a nice mask.
    unsafe self.bucketMask = bucketCount &- 1
  }

  @inlinable
  internal var bucketCount: Int {
    @inline(__always) get {
      return unsafe _assumeNonNegative(bucketMask &+ 1)
    }
  }

  @inlinable
  internal var wordCount: Int {
    @inline(__always) get {
      return unsafe _UnsafeBitset.wordCount(forCapacity: bucketCount)
    }
  }

  /// Return a bitset representation of the occupied buckets in this table.
  ///
  /// Note that if we have only a single partial word in the hash table's
  /// bitset, then its out-of-bounds bits are guaranteed to be all set. These
  /// filler bits are there to speed up finding holes -- they don't correspond
  /// to occupied buckets in the table.
  @_alwaysEmitIntoClient
  internal var bitset: _UnsafeBitset {
    unsafe _UnsafeBitset(words: words, wordCount: wordCount)
  }
}

@available(*, unavailable)
extension _HashTable: Sendable {}

extension _HashTable {
  /// The inverse of the maximum hash table load factor.
  private static var maxLoadFactor: Double {
    @inline(__always) get { return 3 / 4 }
  }

  internal static func capacity(forScale scale: Int8) -> Int {
    let bucketCount = (1 as Int) &<< scale
    return unsafe Int(Double(bucketCount) * maxLoadFactor)
  }

  internal static func scale(forCapacity capacity: Int) -> Int8 {
    let capacity = Swift.max(capacity, 1)
    // Calculate the minimum number of entries we need to allocate to satisfy
    // the maximum load factor. `capacity + 1` below ensures that we always
    // leave at least one hole.
    let minimumEntries = unsafe Swift.max(
      Int((Double(capacity) / maxLoadFactor).rounded(.up)),
      capacity + 1)
    // The actual number of entries we need to allocate is the lowest power of
    // two greater than or equal to the minimum entry count. Calculate its
    // exponent.
    let exponent = (Swift.max(minimumEntries, 2) - 1)._binaryLogarithm() + 1
    _internalInvariant(exponent >= 0 && exponent < Int.bitWidth)
    // The scale is the exponent corresponding to the bucket count.
    let scale = Int8(truncatingIfNeeded: exponent)
    unsafe _internalInvariant(self.capacity(forScale: scale) >= capacity)
    return scale
  }

  // The initial age to use for native copies of a Cocoa NSSet/NSDictionary.
  internal static func age(for cocoa: AnyObject) -> Int32 {
    let hash = ObjectIdentifier(cocoa).hashValue
    return Int32(truncatingIfNeeded: hash)
  }

  internal static func hashSeed(
    for object: Builtin.NativeObject,
    scale: Int8
  ) -> Int {
    // We generate a new hash seed whenever a new hash table is allocated and
    // whenever an existing table is resized, so that we avoid certain copy
    // operations becoming quadratic. For background details, see
    // https://github.com/apple/swift/issues/45856.
    //
    // Note that we do reuse the existing seed when making copy-on-write copies
    // so that we avoid breaking value semantics.
    if Hasher._isDeterministic {
      // When we're using deterministic hashing, the scale value as the seed is
      // still allowed, and it covers most cases. (Unfortunately some operations
      // that merge two similar-sized hash tables will still be quadratic.)
      return Int(scale)
    }
    // Use the object address as the hash seed. This is cheaper than
    // SystemRandomNumberGenerator, while it has the same practical effect.
    // Addresses aren't entirely random, but that's not the goal here -- the
    // 128-bit execution seed takes care of randomization. We only need to
    // guarantee that no two tables with the same seed can coexist at the same
    // time (apart from copy-on-write derivatives of the same table).
    return unsafe unsafeBitCast(object, to: Int.self)
  }
}

extension _HashTable {
  @frozen
  @usableFromInline
  internal struct Bucket {
    @usableFromInline
    internal var offset: Int

    @inlinable
    @inline(__always)
    internal init(offset: Int) {
      unsafe self.offset = offset
    }

    @inlinable
    @inline(__always)
    internal init(word: Int, bit: Int) {
      unsafe self.offset = unsafe _UnsafeBitset.join(word: word, bit: bit)
    }

    @inlinable
    internal var word: Int {
      @inline(__always) get {
        return unsafe _UnsafeBitset.word(for: offset)
      }
    }

    @inlinable
    internal var bit: Int {
      @inline(__always) get {
        return unsafe _UnsafeBitset.bit(for: offset)
      }
    }
  }
}

extension _HashTable.Bucket: Equatable {
  @inlinable
  @inline(__always)
  internal
  static func == (lhs: _HashTable.Bucket, rhs: _HashTable.Bucket) -> Bool {
    return unsafe lhs.offset == rhs.offset
  }
}

extension _HashTable.Bucket: Comparable {
  @inlinable
  @inline(__always)
  internal
  static func < (lhs: _HashTable.Bucket, rhs: _HashTable.Bucket) -> Bool {
    return unsafe lhs.offset < rhs.offset
  }
}

extension _HashTable {
  @usableFromInline
  @frozen
  internal struct Index {
    @usableFromInline
    let bucket: Bucket

    @usableFromInline
    let age: Int32

    @inlinable
    @inline(__always)
    internal init(bucket: Bucket, age: Int32) {
      unsafe self.bucket = unsafe bucket
      unsafe self.age = age
    }
  }
}

extension _HashTable.Index: Equatable {
  @inlinable
  @inline(__always)
  internal static func ==(
    lhs: _HashTable.Index,
    rhs: _HashTable.Index
  ) -> Bool {
    unsafe _precondition(lhs.age == rhs.age,
      "Can't compare indices belonging to different collections")
    return unsafe lhs.bucket == rhs.bucket
  }
}

extension _HashTable.Index: Comparable {
  @inlinable
  @inline(__always)
  internal static func <(
    lhs: _HashTable.Index,
    rhs: _HashTable.Index
  ) -> Bool {
    unsafe _precondition(lhs.age == rhs.age,
      "Can't compare indices belonging to different collections")
    return unsafe lhs.bucket < rhs.bucket
  }
}

extension _HashTable: @unsafe Sequence {
  @usableFromInline
  @frozen
  internal struct Iterator: @unsafe IteratorProtocol {
    @usableFromInline
    let hashTable: _HashTable
    @usableFromInline
    var wordIndex: Int
    @usableFromInline
    var word: Word

    @inlinable
    @inline(__always)
    init(_ hashTable: _HashTable) {
      unsafe self.hashTable = unsafe hashTable
      unsafe self.wordIndex = 0
      unsafe self.word = unsafe hashTable.words[0]
      if unsafe hashTable.bucketCount < Word.capacity {
        unsafe self.word = unsafe self.word.intersecting(elementsBelow: hashTable.bucketCount)
      }
    }

    @inlinable
    @inline(__always)
    internal mutating func next() -> Bucket? {
      if let bit = unsafe word.next() {
        return unsafe Bucket(word: wordIndex, bit: bit)
      }
      while unsafe wordIndex + 1 < hashTable.wordCount {
        unsafe wordIndex += 1
        unsafe word = unsafe hashTable.words[wordIndex]
        if let bit = unsafe word.next() {
          return unsafe Bucket(word: wordIndex, bit: bit)
        }
      }
      return nil
    }
  }

  @inlinable
  @inline(__always)
  internal func makeIterator() -> Iterator {
    return unsafe Iterator(self)
  }
}

@available(*, unavailable)
extension _HashTable.Iterator: Sendable {}

extension _HashTable {
  @inlinable
  @inline(__always)
  internal func isValid(_ bucket: Bucket) -> Bool {
    return unsafe bucket.offset >= 0 && bucket.offset < bucketCount
  }

  @inlinable
  @inline(__always)
  internal func _isOccupied(_ bucket: Bucket) -> Bool {
    unsafe _internalInvariant(isValid(bucket))
    return unsafe words[bucket.word].uncheckedContains(bucket.bit)
  }

  @inlinable
  @inline(__always)
  internal func isOccupied(_ bucket: Bucket) -> Bool {
    return unsafe isValid(bucket) && _isOccupied(bucket)
  }

  @inlinable
  @inline(__always)
  internal func checkOccupied(_ bucket: Bucket) {
    unsafe _precondition(isOccupied(bucket),
      "Attempting to access Collection elements using an invalid Index")
  }

  @inlinable
  @inline(__always)
  internal func _firstOccupiedBucket(fromWord word: Int) -> Bucket {
    unsafe _internalInvariant(word >= 0 && word <= wordCount)
    var word = word
    while unsafe word < wordCount {
      if let bit = unsafe words[word].minimum {
        return unsafe Bucket(word: word, bit: bit)
      }
      word += 1
    }
    return unsafe endBucket
  }

  @inlinable
  internal func occupiedBucket(after bucket: Bucket) -> Bucket {
    unsafe _internalInvariant(isValid(bucket))
    let word = unsafe bucket.word
    if let bit = unsafe words[word].intersecting(elementsAbove: bucket.bit).minimum {
      return unsafe Bucket(word: word, bit: bit)
    }
    return unsafe _firstOccupiedBucket(fromWord: word + 1)
  }

  @inlinable
  internal var startBucket: Bucket {
    return unsafe _firstOccupiedBucket(fromWord: 0)
  }

  @inlinable
  internal var endBucket: Bucket {
    @inline(__always)
    get {
      return unsafe Bucket(offset: bucketCount)
    }
  }
}

extension _HashTable {
  @inlinable
  @inline(__always)
  internal func idealBucket(forHashValue hashValue: Int) -> Bucket {
    return unsafe Bucket(offset: hashValue & bucketMask)
  }

  /// The next bucket after `bucket`, with wraparound at the end of the table.
  @inlinable
  @inline(__always)
  internal func bucket(wrappedAfter bucket: Bucket) -> Bucket {
    // The bucket is less than bucketCount, which is power of two less than
    // Int.max. Therefore adding 1 does not overflow.
    return unsafe Bucket(offset: (bucket.offset &+ 1) & bucketMask)
  }
}

extension _HashTable {
  @inlinable
  internal func previousHole(before bucket: Bucket) -> Bucket {
    unsafe _internalInvariant(isValid(bucket))
    // Note that if we have only a single partial word, its out-of-bounds bits
    // are guaranteed to be all set, so the formula below gives correct results.
    var word = unsafe bucket.word
    if let bit =
      unsafe words[word]
        .complement
        .intersecting(elementsBelow: bucket.bit)
        .maximum {
      return unsafe Bucket(word: word, bit: bit)
    }
    var wrap = false
    while true {
      word -= 1
      if word < 0 {
        _precondition(!wrap, "Hash table has no holes")
        wrap = true
        word = unsafe wordCount - 1
      }
      if let bit = unsafe words[word].complement.maximum {
        return unsafe Bucket(word: word, bit: bit)
      }
    }
  }

  @inlinable
  internal func nextHole(atOrAfter bucket: Bucket) -> Bucket {
    unsafe _internalInvariant(isValid(bucket))
    // Note that if we have only a single partial word, its out-of-bounds bits
    // are guaranteed to be all set, so the formula below gives correct results.
    var word = unsafe bucket.word
    if let bit =
      unsafe words[word]
        .complement
        .subtracting(elementsBelow: bucket.bit)
        .minimum {
      return unsafe Bucket(word: word, bit: bit)
    }
    var wrap = false
    while true {
      word &+= 1
      if unsafe word == wordCount {
        _precondition(!wrap, "Hash table has no holes")
        wrap = true
        word = 0
      }
      if let bit = unsafe words[word].complement.minimum {
        return unsafe Bucket(word: word, bit: bit)
      }
    }
  }
}

extension _HashTable {
  @inlinable
  @inline(__always)
  @_effects(releasenone)
  internal func copyContents(of other: _HashTable) {
    unsafe _internalInvariant(bucketCount == other.bucketCount)
    unsafe self.words.update(from: other.words, count: wordCount)
  }

  /// Insert a new entry with the specified hash value into the table.
  /// The entry must not already exist in the table -- duplicates are ignored.
  @inlinable
  @inline(__always)
  internal func insertNew(hashValue: Int) -> Bucket {
    let hole = unsafe nextHole(atOrAfter: idealBucket(forHashValue: hashValue))
    unsafe insert(hole)
    return unsafe hole
  }

  /// Insert a new entry for an element at `index`.
  @inlinable
  @inline(__always)
  internal func insert(_ bucket: Bucket) {
    unsafe _internalInvariant(!isOccupied(bucket))
    unsafe words[bucket.word].uncheckedInsert(bucket.bit)
  }

  @inlinable
  @inline(__always)
  internal func clear() {
    if unsafe bucketCount < Word.capacity {
      // We have only a single partial word. Set all out of bounds bits, so that
      // `occupiedBucket(after:)` and `nextHole(atOrAfter:)` works correctly
      // without a special case.
      unsafe words[0] = Word.allBits.subtracting(elementsBelow: bucketCount)
    } else {
      unsafe words.update(repeating: .empty, count: wordCount)
    }
  }

  @inline(__always)
  @inlinable
  internal func delete<D: _HashTableDelegate>(
    at bucket: Bucket,
    with delegate: D
  ) {
    unsafe _internalInvariant(isOccupied(bucket))

    // If we've put a hole in a chain of contiguous elements, some element after
    // the hole may belong where the new hole is.

    var hole = unsafe bucket
    var candidate = unsafe self.bucket(wrappedAfter: hole)

    guard unsafe _isOccupied(candidate) else {
      // Fast path: Don't get the first bucket when there's nothing to do.
      unsafe words[hole.word].uncheckedRemove(hole.bit)
      return
    }

    // Find the first bucket in the contiguous chain that contains the entry
    // we've just deleted.
    let start = unsafe self.bucket(wrappedAfter: previousHole(before: bucket))

    // Relocate out-of-place elements in the chain, repeating until we get to
    // the end of the chain.
    while unsafe _isOccupied(candidate) {
      let candidateHash = unsafe delegate.hashValue(at: candidate)
      let ideal = unsafe idealBucket(forHashValue: candidateHash)

      // Does this element belong between start and hole?  We need two
      // separate tests depending on whether [start, hole] wraps around the
      // end of the storage.
      let c0 = unsafe ideal >= start
      let c1 = unsafe ideal <= hole
      if unsafe start <= hole ? (c0 && c1) : (c0 || c1) {
        unsafe delegate.moveEntry(from: candidate, to: hole)
        unsafe hole = unsafe candidate
      }
      unsafe candidate = unsafe self.bucket(wrappedAfter: candidate)
    }

    unsafe words[hole.word].uncheckedRemove(hole.bit)
  }
}
