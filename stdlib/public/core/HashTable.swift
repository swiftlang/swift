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
  func hashValue(at index: _HashTable.Index) -> Int
  func moveEntry(from source: _HashTable.Index, to target: _HashTable.Index)
}

@usableFromInline
@_fixed_layout
internal struct _HashTable {
  /// Unsafe pointer to the beginning of metadata storage.
  @usableFromInline
  internal let buckets: UnsafeMutablePointer<Bucket>

  @usableFromInline
  internal let bucketMask: Int

  @inlinable
  @inline(__always)
  internal init(buckets: UnsafeMutablePointer<Bucket>, bucketCount: Int) {
    self.buckets = buckets
    // The bucket count is a positive power of two, so subtracting 1 will
    // never overflow and get us a nice mask.
    self.bucketMask = bucketCount &- 1
  }

  @inlinable
  internal var entryCount: Int {
    @inline(__always) get {
      return bucketCount &<< 3
    }
  }

  @inlinable
  internal var bucketCount: Int {
    @inline(__always) get {
      return bucketMask &+ 1
    }
  }
}

extension _HashTable {
  @inlinable
  internal static func newSeed(forScale scale: Int) -> Hasher._Seed {
    if Hasher._isDeterministic {
      // We can't use per-instance seeding when deterministic hashing is
      // enabled. The next best thing is per-capacity seeding -- however, this
      // means there will be a correlation between hash values in same-sized
      // hash tables, so some operations will become quadratic.
      return (
        Hasher._seed.0,
        Hasher._seed.1 ^ UInt64(truncatingIfNeeded: scale))
    }
    return Hasher._randomSeed()
  }

  /// The inverse of the maximum hash table load factor.
  private static var maxLoadFactorInverse: Double {
    @inline(__always) get { return 100 / 75 }
  }

  internal static func capacity(forScale scale: Int) -> Int {
    let entryCount = 1 &<< (scale + 3)
    return Int(Double(entryCount) / maxLoadFactorInverse)
  }

  @usableFromInline
  @_effects(readnone)
  internal static func scale(
    forCapacity capacity: Int
  ) -> Int {
    let capacity = Swift.max(capacity, 1)
    // Calculate the minimum number of entries we need to allocate to satisfy
    // the maximum load factor. `capacity + 1` below ensures that we always
    // leave at least one hole.
    let minimumEntries = Swift.max(
      Int((Double(capacity) * maxLoadFactorInverse).rounded(.up)),
      capacity + 1)
    // The actual number of entries we need to allocate is the lowest power of
    // two greater than or equal to the minimum entry count. Calculate its
    // exponent.
    let exponent = (Swift.max(minimumEntries, 2) - 1)._binaryLogarithm() + 1
    _sanityCheck(exponent < Int.bitWidth)
    // The scale is the exponent corresponding to the bucket count, which is the
    // entry count divided by 8. Ensure we always have at least one bucket.
    return Swift.max(exponent - 3, 0)
  }
}

extension _HashTable {
  /// Metadata for an entry in the hash table. Includes a flag indicating
  /// whether there is an element at this entry, as well as a 7-bit payload
  /// value. Occupied entries use the payload to store additional bits of the
  /// entry's hash value; this is used to speed up lookup operations.
  /// Unoccupied entries are called "holes"; they have a zero payload of zero.
  @usableFromInline
  @_fixed_layout
  internal struct Entry {
    @inlinable
    internal static var unoccupied: Entry {
      @inline(__always) get { return Entry(_value: 0) }
    }

    @usableFromInline
    internal var _value: UInt8

    @inlinable
    @inline(__always)
    internal init(_value: UInt8) {
      self._value = _value
    }

    @inlinable
    @inline(__always)
    internal init(payload: UInt8) {
      _sanityCheck(payload != 0)
      self.init(_value: payload)
    }

    @inlinable
    @inline(__always)
    internal init(forHashValue hashValue: Int) {
      let payload = UInt(bitPattern: hashValue) &>> (Int.bitWidth &- 8)
      self.init(payload: Swift.max(1, UInt8(truncatingIfNeeded: payload)))
    }

    @inlinable
    internal var isOccupied: Bool {
      @inline(__always) get { return _value != 0 }
    }

    @inlinable
    internal var payload: UInt8 {
      @inline(__always) get {
        return _value
      }
    }
  }
}

extension _HashTable.Entry {
  @inlinable
  var pattern: UInt64 {
    @inline(__always) get {
      // Fill a 64-bit integer with 8 copies of this entry.
      let p = UInt64(truncatingIfNeeded: _value)
      return p &* 0x01010101_01010101
    }
  }
}

extension _HashTable.Entry: Equatable {
  @inlinable
  @inline(__always)
  internal static func ==(
    left: _HashTable.Entry,
    right: _HashTable.Entry
  ) -> Bool {
    return left._value == right._value
  }
}

extension _HashTable {
  /// A bucket in the hash table. Buckets hold eight slots, each of which
  /// contain an Entry. Holes (if any) are always at the highest-numbered slots,
  /// sorted after occupied entries.
  ///
  /// Internally, each bucket is represented by a single 64-bit integer value.
  /// This enables us to perform operations on all 8 slots at once.
  @_fixed_layout
  @usableFromInline
  internal struct Bucket {
    @usableFromInline
    internal var _value: UInt64

    @inlinable
    @inline(__always)
    internal init(_ value: UInt64) {
      self._value = value
    }
  }
}

extension _HashTable {
  /// An index-like value identifying one of the eight slots of a bucket.
  @_fixed_layout
  @usableFromInline
  internal struct Slot: Comparable {
    /// The index of the first bit of the byte corresponding to this slot within
    /// the bucket's value.
    @usableFromInline
    var shift: Int

    @inlinable
    @inline(__always)
    internal init(slot: Int) {
      _sanityCheck(slot >= 0 && slot <= UInt8.bitWidth)
      self.shift = slot &<< 3
    }

    @inlinable
    @inline(__always)
    internal init(shift: Int) {
      _sanityCheck(shift >= 0 && shift <= UInt64.bitWidth)
      _sanityCheck(shift & 7 == 0)
      self.shift = shift
    }

    @inlinable
    internal static var start: Slot {
      @inline(__always) get { return Slot(shift: 0) }
    }
    @inlinable
    internal static var end: Slot {
      @inline(__always) get { return Slot(shift: UInt64.bitWidth) }
    }

    @inlinable
    internal var offset: Int {
      @inline(__always) get { return shift &>> 3 }
    }

    @inlinable
    @inline(__always)
    internal mutating func formSuccessor() {
      _sanityCheck(self != Slot.end)
      shift += UInt8.bitWidth
    }

    @inlinable
    @inline(__always)
    internal static func ==(left: Slot, right: Slot) -> Bool {
      return left.shift == right.shift
    }
    @inlinable
    @inline(__always)
    internal static func <(left: Slot, right: Slot) -> Bool {
      return left.shift < right.shift
    }
  }
}

extension _HashTable.Bucket {
  @inlinable
  internal subscript(slot: _HashTable.Slot) -> _HashTable.Entry {
    @inline(__always) get {
      return _HashTable.Entry(
        _value: UInt8(truncatingIfNeeded: _value &>> slot.shift))
    }
    @inline(__always) set {
      _value &= ~(0xFF &<< slot.shift)
      _value |= (UInt64(newValue._value) &<< slot.shift)
    }
  }
}

extension _HashTable.Bucket {
  /// Returns true if there are no holes in this bucket.
  @inlinable
  internal var isFull: Bool {
    @inline(__always)
    get {
      // Holes are always at the end, so it's enough to check the highest byte.
      return _value.leadingZeroBitCount < UInt8.bitWidth
    }
  }

  /// Returns true if this bucket has no occupied slots.
  @inlinable
  internal var isEmpty: Bool {
    @inline(__always) get { return _value == 0 }
  }

  @inlinable
  @inline(__always)
  internal func isOccupied(at slot: _HashTable.Slot) -> Bool {
    // Unoccupied slots are never followed by occupied ones, so we don't need to
    // isolate the entry.
    return (_value &>> slot.shift) != 0
  }

  /// Find the slot for the first hole in this bucket.
  /// Returns `Slot.end` if the bucket is full.
  @inlinable
  internal var slotForLowestHole: _HashTable.Slot {
    @inline(__always)
    get {
      // Holes are zero bytes at the most significant places.
      let shift = UInt64.bitWidth &- (_value.leadingZeroBitCount & ~7)
      return _HashTable.Slot(shift: shift)
    }
  }

  /// Insert an entry to this bucket for the specified hash value, and return
  /// its slot. The bucket must not be full.
  @inlinable
  internal mutating func insertEntry(
    forHashValue hashValue: Int
  ) -> _HashTable.Slot {
    _sanityCheck(!isFull)
    let slot = self.slotForLowestHole
    self[slot] = _HashTable.Entry(forHashValue: hashValue)
    return slot
  }

  /// Remove the entry at the specified index. If there are occupied entries at
  /// higher indices, compress the bucket by moving the highest occupied entry
  /// in place of the removed one, and return its index. Otherwise return
  /// `index`.
  @inlinable
  internal mutating func removeEntry(
    at slot: _HashTable.Slot
  ) -> _HashTable.Slot {
    _sanityCheck(self[slot].isOccupied)
    let replacement = _HashTable.Slot(
      shift: self.slotForLowestHole.shift - UInt8.bitWidth)
    if slot == replacement {
      self[slot] = .unoccupied
    } else {
      let hm: UInt64 = 0xFF &<< replacement.shift
      let lm: UInt64 = 0xFF &<< slot.shift
      let distance = replacement.shift - slot.shift
      _value = ((_value &>> distance) & lm) | (_value & ~(hm | lm))
    }
    return replacement
  }

  // Represents a subset of bucket indices, using a single bit pattern.
  @_fixed_layout
  @usableFromInline
  internal struct SlotSet: Sequence, IteratorProtocol {
    // Each bit set to one in this pattern sits at the beginning of a byte,
    // corresponding to a single slot shift within this set.
    @usableFromInline
    internal var _shifts: UInt64

    @inlinable
    @inline(__always)
    internal init(_shifts: UInt64) {
      _sanityCheck(_shifts & ~0x01010101_01010101 == 0)
      self._shifts = _shifts
    }

    @inlinable
    @inline(__always)
    internal mutating func next() -> _HashTable.Slot? {
      if _shifts == 0 { return nil }
      let shift = _shifts.trailingZeroBitCount
      // Clear the lowest bit set in _shifts.
      _shifts &= _shifts &- 1
      return _HashTable.Slot(shift: shift)
    }
  }

  /// Returns a sequence of Slots matching the given entry.
  @inlinable
  @inline(__always)
  internal func _slots(matching pattern: UInt64) -> SlotSet {
    let p = self._value ^ pattern
    // The problem now reduces to finding zero bytes in `p`.  For every 8-bit
    // integer `b`, `x = ((b & 0x7F) + 0x7F) | b` has bit 7 set iff `b !=
    // 0`. Further, `~(x | 0x7F)` leaves bit 7 set to one iff `b == 0` and
    // clears all other bits.  Use this formula to check for zero values in all
    // 8 bytes of `p` at the same time. These 8-bit calculations never overflow,
    // so it's safe to perform them in parallel on bytes within the same word.
    let m: UInt64 = 0x7F7F7F7F7F7F7F7F
    let y = ~(((p & m) + m) | p | m)
    // `y` has bit 7 set in all bytes that match the payload in `self`, with all
    // other bits cleared. Shifting it seven places to the right moves the set
    // bits to the start of their corresponding bytes.
    return SlotSet(_shifts: y &>> 7)
  }

  /// Returns a sequence of Slots matching the given entry.
  @inlinable
  @inline(__always)
  internal func slots(matching entry: _HashTable.Entry) -> SlotSet {
    return _slots(matching: entry.pattern)
  }
}

extension _HashTable {
  @_fixed_layout
  @usableFromInline
  internal struct Index {
    @usableFromInline
    internal var offset: Int

    @inlinable
    @inline(__always)
    internal init(offset: Int) {
      self.offset = offset
    }

    @inlinable
    @inline(__always)
    internal init(bucket: Int, slotOffset: Int = 0) {
      _sanityCheck(bucket >= 0)
      _sanityCheck(slotOffset >= 0 && slotOffset < 8)
      self.offset = (bucket &<< 3) | slotOffset
    }

    @inlinable
    @inline(__always)
    internal init(bucket: Int, slot: Slot) {
      self.init(bucket: bucket, slotOffset: slot.offset)
    }

    @inlinable
    internal var bucket: Int {
      @inline(__always) get {
        return offset &>> 3
      }
    }

    @inlinable
    internal var slot: Slot {
      @inline(__always) get {
        return Slot(slot: offset & 7)
      }
    }
  }
}

extension _HashTable.Index: Equatable {
  @inlinable
  @inline(__always)
  internal
  static func ==(lhs: _HashTable.Index, rhs: _HashTable.Index) -> Bool {
    return lhs.offset == rhs.offset
  }
}

extension _HashTable.Index: Comparable {
  @inlinable
  @inline(__always)
  internal
  static func < (lhs: _HashTable.Index, rhs: _HashTable.Index) -> Bool {
    return lhs.offset < rhs.offset
  }
}

extension _HashTable: Collection {
  @inlinable
  @inline(__always)
  internal func isValid(_ index: Index) -> Bool {
    return index.offset >= 0 && index.offset < entryCount
  }

  @inlinable
  @inline(__always)
  internal func _isOccupied(_ index: Index) -> Bool {
    _sanityCheck(isValid(index))
    return self[index].isOccupied
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
  internal func index(after index: Index) -> Index {
    var index = index
    formIndex(after: &index)
    return index
  }

  @inlinable
  internal func formIndex(after index: inout Index) {
    index.offset += 1
    if index.bucket >= bucketCount { return }
    if buckets[index.bucket].isOccupied(at: index.slot) { return }
    // Find next occupied bucket.
    var bucket = index.bucket + 1
    while bucket < bucketCount, buckets[bucket].isEmpty {
      bucket += 1
    }
    index = Index(bucket: bucket)
  }

  @inlinable
  internal var startIndex: Index {
    get {
      // We start at the index after offset -1 instead of the index at offset 0
      // because we need to find the first occupied slot.
      return index(after: Index(offset: -1))
    }
  }

  @inlinable
  internal var endIndex: Index {
    @inline(__always)
    get {
      return Index(offset: entryCount)
    }
  }

  @inlinable
  internal subscript(index: Index) -> Entry {
    @inline(__always) get {
      _sanityCheck(isValid(index))
      return buckets[index.bucket][index.slot]
    }
    @inline(__always) nonmutating set {
      _sanityCheck(isValid(index))
      buckets[index.bucket][index.slot] = newValue
    }
  }
}

extension _HashTable {
  @inlinable
  @inline(__always)
  internal func _idealBucket(forHashValue hashValue: Int) -> Int {
    return hashValue & bucketMask
  }

  /// The next bucket after `bucket`, with wraparound at the end of the table.
  @inlinable
  @inline(__always)
  internal func _succ(_ bucket: Int) -> Int {
    // Bucket is less than bucketCount, which is power of two less than
    // Int.max. Therefore adding 1 does not overflow.
    return (bucket &+ 1) & bucketMask
  }

  /// The previous bucket after `bucket`, with wraparound at the beginning of
  /// the table.
  @inlinable
  @inline(__always)
  internal func _pred(_ bucket: Int) -> Int {
    // Bucket is not negative. Therefore subtracting 1 does not overflow.
    return (bucket &- 1) & bucketMask
  }
}


extension _HashTable {
  @_fixed_layout
  @usableFromInline
  internal struct LookupCandidates {
    @usableFromInline
    let _hashTable: _HashTable
    @usableFromInline
    let _pattern: UInt64
    @usableFromInline
    var _bucket: Int
    @usableFromInline
    var _matches: Bucket.SlotSet

    @inlinable
    internal init(hashTable: _HashTable, hashValue: Int) {
      self._hashTable = hashTable
      self._bucket = hashTable._idealBucket(forHashValue: hashValue)
      let b = hashTable.buckets[_bucket]
      self._pattern = Entry(forHashValue: hashValue).pattern
      self._matches = b._slots(matching: _pattern)
    }

    @inlinable
    internal mutating func next() -> (index: Index, found: Bool) {
      while true {
        if let slot = _matches.next() {
          return (Index(bucket: _bucket, slot: slot), true)
        }
        let b = _hashTable.buckets[_bucket]
        if !b.isFull {
          let hole = b.slotForLowestHole
          return (Index(bucket: _bucket, slot: hole), false)
        }
        _bucket = _hashTable._succ(_bucket)
        _matches = _hashTable.buckets[_bucket]._slots(matching: _pattern)
      }
    }
  }

  @inlinable
  func lookup(hashValue: Int) -> LookupCandidates {
    return LookupCandidates(hashTable: self, hashValue: hashValue)
  }
}

extension _HashTable {
  @inlinable
  @inline(__always)
  func contains<Element: Equatable>(
    hashValue: Int,
    element: Element,
    elements: UnsafePointer<Element>
  ) -> Bool {
    var bucket = _idealBucket(forHashValue: hashValue)
    var b = self.buckets[bucket]
    let pattern = Entry(forHashValue: hashValue).pattern
    var matches = b._slots(matching: pattern)._shifts
    while true {
      if _fastPath(matches != 0) {
        let shift = matches.trailingZeroBitCount
        let index = Index(bucket: bucket, slotOffset: shift &>> 3)
        if elements[index.offset] == element { return true }
        matches &= matches &- 1
      } else if b.isFull {
        bucket = _succ(bucket)
        b = self.buckets[bucket]
        matches = b._slots(matching: pattern)._shifts
      } else {
        return false
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
    self.buckets.assign(from: other.buckets, count: bucketCount)
  }

  /// Insert a new entry with the specified hash value into the table.
  /// The entry must not already exist in the table -- duplicates are ignored.
  @inlinable
  @inline(__always)
  @_effects(releasenone)
  internal func insertNew(hashValue: Int) -> Index {
    var bucket = _idealBucket(forHashValue: hashValue)
    while buckets[bucket].isFull {
      bucket = _succ(bucket)
    }
    let slot = buckets[bucket].insertEntry(forHashValue: hashValue)
    return Index(bucket: bucket, slot: slot)
  }

  /// Insert a new entry for an element with the specified hash value at
  /// `bucket`. The bucket must have been returned by `lookupFirst` or
  /// `lookupNext` for the same hash value, with `found == false`.
  @inlinable
  @inline(__always)
  @_effects(releasenone)
  internal func insert(hashValue: Int, at index: Index) {
    _sanityCheck(!isOccupied(index))
    self[index] = Entry(forHashValue: hashValue)
  }

  @inlinable
  @inline(__always)
  internal func removeAll() {
    buckets.assign(repeating: Bucket(0), count: bucketCount)
  }

  @inlinable
  internal func _startBucket(forChainContaining bucket: Int) -> Int {
    var bucket = bucket
    while true {
      let previous = _pred(bucket)
      guard buckets[previous].isFull else { return bucket }
      bucket = previous
    }
  }

  @inlinable
  @inline(__always)
  internal func delete<D: _HashTableDelegate>(
    hashValue: Int,
    at index: Index,
    with delegate: D
  ) {
    _sanityCheck(self[index] == Entry(forHashValue: hashValue))

    if !buckets[index.bucket].isFull {
      // Fast path: If bucket already has another hole, then we aren't splitting
      // any chains, so we're done.
      let replacement = buckets[index.bucket].removeEntry(at: index.slot)
      delegate.moveEntry(
        from: Index(bucket: index.bucket, slot: replacement),
        to: index)
      return
    }

    // If we've put a hole in a chain of contiguous elements, some element after
    // the hole may belong where the new hole is.

    // Find the first bucket in the contiguous chain that contains the entry
    // we've just deleted.
    let startBucket = _startBucket(
      forChainContaining: _idealBucket(forHashValue: hashValue))

    var hole = index
    var candidate = Index(bucket: _succ(hole.bucket))

    // Relocate out-of-place elements in the chain, repeating until we get to
    // the end of the chain or until we move the hole into a bucket that already
    // has another.
    while self[candidate].isOccupied {
      let candidateHash = delegate.hashValue(at: candidate)
      _sanityCheck(self[candidate] == Entry(forHashValue: candidateHash))
      let idealBucket = _idealBucket(forHashValue: candidateHash)

      // Does this element belong between start and hole?  We need two
      // separate tests depending on whether [start, hole] wraps around the
      // end of the storage.
      let c0 = idealBucket >= startBucket
      let c1 = idealBucket <= hole.bucket
      if startBucket <= hole.bucket ? (c0 && c1) : (c0 || c1) {
        self[hole] = Entry(forHashValue: candidateHash)
        delegate.moveEntry(from: candidate, to: hole)
        hole = candidate
        guard buckets[hole.bucket].isFull else {
          break
        }
      }
      candidate.offset = (candidate.offset &+ 1) & (entryCount &- 1)
    }

    // Compact bucket with the newly created hole.
    let replacement = buckets[hole.bucket].removeEntry(at: hole.slot)
    delegate.moveEntry(
      from: Index(bucket: hole.bucket, slot: replacement),
      to: hole)
  }
}

extension _HashTable {
  /// Check for consistency and return the count of occupied entries.
  internal func _invariantCheck(with delegate: _HashTableDelegate) -> Int {
#if INTERNAL_CHECKS_ENABLED
    _sanityCheck(bucketCount > 0 && bucketCount & (bucketCount &- 1) == 0,
      "Invalid bucketCount")
    _sanityCheck(_isValidAddress(UInt(bitPattern: buckets)),
      "Invalid buckets pointer")
    _sanityCheck(_isValidAddress(UInt(bitPattern: buckets + bucketCount - 1)),
      "Invalid buckets buffer")

    var occupiedCount = 0
    for b in 0 ..< bucketCount {
      let bucket = buckets[b]
      var slot = Slot.start
      var foundHole = false
      while slot != Slot.end {
        defer { slot.formSuccessor() }
        guard bucket[slot].isOccupied else {
          foundHole = true
          continue
        }
        _sanityCheck(!foundHole, "Bucket's holes aren't all at the end")
        occupiedCount += 1
        let hashValue = delegate.hashValue(at: Index(bucket: b, slot: slot))
        _sanityCheck(bucket[slot] == Entry(forHashValue: hashValue),
          "Some hash table elements are stored with mismatching hash bits")
        // There must be no holes between the ideal and actual buckets for this
        // hash value.
        var i = _idealBucket(forHashValue: hashValue)
        while i != b {
          _sanityCheck(buckets[i].isFull,
            "Some hash table elements are stored outside their collision chain")
          i = _succ(i)
        }
      }
    }
    return occupiedCount
#else
    return 0
#endif
  }
}
