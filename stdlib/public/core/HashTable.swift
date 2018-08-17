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

internal protocol _HashTableDelegate {
  func hashValue(at index: _HashTable.Index) -> Int
  func moveEntry(from source: _HashTable.Index, to target: _HashTable.Index)
}

@usableFromInline
@_fixed_layout
internal struct _HashTable {
  @usableFromInline
  internal let scale: Int
  @usableFromInline
  internal let capacity: Int
  @usableFromInline
  internal var count: Int
  @usableFromInline
  internal let rawMap: UnsafeMutableRawPointer

  internal init(scale: Int, count: Int, map: UnsafeMutablePointer<MapEntry>) {
    _sanityCheck(scale >= 0 && scale < Int.bitWidth - 1)
    _sanityCheck(count >= 0 && count < (1 << scale) - 1)
    let capacity = Int(Double(1 &<< scale) / _HashTable.maxLoadFactorInverse)
    _sanityCheck(count <= capacity)
    self.scale = scale
    self.capacity = capacity
    self.count = count
    self.rawMap = UnsafeMutableRawPointer(map)

    map.assign(repeating: .unoccupied, count: bucketCount)
  }
}

extension _HashTable {
  /// The inverse of the hash table load factor.
  @_transparent
  private static var maxLoadFactorInverse: Double {
    return 4 / 3
  }

  @usableFromInline
  internal static func scale(
    forCapacity capacity: Int
  ) -> Int {
    let capacity = Swift.max(capacity, 1)
    // `capacity + 1` below ensures that we don't fill in the last hole.
    let bucketCount = Swift.max(
      Int((Double(capacity) * maxLoadFactorInverse).rounded(.up)),
      capacity + 1)
    // Actual bucket count is the next power of two greater than or equal to the
    // minimum count satisying the load factor constraint.
    let scale = (Swift.max(bucketCount, 2) - 1)._binaryLogarithm() + 1
    _sanityCheck(scale < Int.bitWidth)
    return scale
  }
}

extension _HashTable {
  internal struct MapEntry: Equatable {
    internal static var payloadMask: UInt8 { return 0x7F }
    internal static var unoccupied: MapEntry { return MapEntry(_value: 0) }

    internal var value: UInt8

    @inline(__always)
    private init(_value: UInt8) {
      self.value = _value
    }

    @inline(__always)
    internal init(payload: UInt8) {
      _sanityCheck(payload < 0x80)
      self.init(_value: 0x80 | payload)
    }

    internal var isOccupied: Bool {
      @inline(__always) get { return value & 0x80 != 0 }
    }

    internal var payload: UInt8 {
      @inline(__always) get {
        return value & _HashTable.MapEntry.payloadMask
      }
    }

    @inline(__always)
    internal static func forHashValue(_ hashValue: Int) -> MapEntry {
      let payload = hashValue &>> (Int.bitWidth &- 7)
      return MapEntry(
        payload: UInt8(truncatingIfNeeded: payload) & MapEntry.payloadMask)
    }
  }

  internal var map: UnsafeMutablePointer<MapEntry> {
    @inline(__always)
    get {
      return rawMap.assumingMemoryBound(to: MapEntry.self)
    }
  }
}

extension _HashTable {
  @usableFromInline
  internal var bucketCount: Int {
    @inline(__always) get {
      return 1 &<< scale
    }
  }

  @usableFromInline
  internal var bucketMask: Int {
    @inline(__always) get {
      // The bucket count is a positive power of two, so subtracting 1 will
      // never overflow and get us a nice mask.
      return bucketCount &- 1
    }
  }

  @inlinable
  @inline(__always)
  internal func _isValid(_ bucket: Int) -> Bool {
    return bucket >= 0 && bucket < bucketCount
  }

  @usableFromInline
  @_effects(readonly)
  internal func _isOccupied(_ bucket: Int) -> Bool {
    _sanityCheck(_isValid(bucket))
    return map[bucket].isOccupied
  }

  /// The next bucket after `bucket`, with wraparound at the end of the table.
  internal func _succ(_ bucket: Int) -> Int {
    // Bucket is less than bucketCount, which is power of two less than
    // Int.max. Therefore adding 1 does not overflow.
    return (bucket &+ 1) & bucketMask
  }

  /// The previous bucket after `bucket`, with wraparound at the beginning of
  /// the table.
  internal func _pred(_ bucket: Int) -> Int {
    // Bucket is not negative. Therefore subtracting 1 does not overflow.
    return (bucket &- 1) & bucketMask
  }

  /// The next unoccupied bucket after `bucket`, with wraparound.
  internal func _nextHole(atOrAfter bucket: Int) -> Int {
    var bucket = bucket
    while map[bucket].isOccupied {
      bucket = _succ(bucket)
    }
    return bucket
  }

  /// The next unoccupied bucket after `bucket`, with wraparound.
  internal func _nextHole(after bucket: Int) -> Int {
    return _nextHole(atOrAfter: _succ(bucket))
  }

  /// The previous unoccupied bucket before `bucket`, with wraparound.
  internal func _prevHole(before bucket: Int) -> Int {
    var bucket = _pred(bucket)
    while map[bucket].isOccupied {
      bucket = _pred(bucket)
    }
    return bucket
  }

  /// The next occupied bucket after `bucket`, without wraparound.
  internal func _occupiedBucket(after bucket: Int) -> Int {
    _sanityCheck(bucket < bucketCount)
    var bucket = bucket + 1
    while bucket < bucketCount && !map[bucket].isOccupied {
      bucket += 1
    }
    return bucket
  }
}

extension _HashTable {
  @_fixed_layout
  @usableFromInline
  internal struct OccupiedIndices: Sequence, IteratorProtocol {
    internal var bucket: Int
    internal let count: Int
    internal let base: UnsafeMutablePointer<MapEntry>

    @usableFromInline
    @_effects(releasenone)
    internal init(
      base: UnsafeMutableRawPointer,
      count: Int) {
      self.bucket = -1
      self.count = count
      self.base = base.assumingMemoryBound(to: MapEntry.self)
    }

    @usableFromInline
    @_effects(releasenone)
    internal mutating func next() -> Index? {
      guard bucket != count else { return nil }
      bucket += 1
      while bucket != count && !base[bucket].isOccupied {
        bucket += 1
      }
      guard bucket != count else { return nil }
      return Index(bucket: bucket)
    }
  }

  @inlinable
  var occupiedIndices: OccupiedIndices {
    return OccupiedIndices(base: rawMap, count: bucketCount)
  }
}

extension _HashTable {
  @_fixed_layout
  @usableFromInline
  internal struct Index {
    @usableFromInline
    internal var bucket: Int

    @inlinable
    internal init(bucket: Int) {
      self.bucket = bucket
    }
  }
}

extension _HashTable.Index: Equatable {
  @inlinable
  internal
  static func ==(lhs: _HashTable.Index, rhs: _HashTable.Index) -> Bool {
    return lhs.bucket == rhs.bucket
  }
}

extension _HashTable.Index: Comparable {
  @inlinable
  internal
  static func < (lhs: _HashTable.Index, rhs: _HashTable.Index) -> Bool {
    return lhs.bucket < rhs.bucket
  }
}


extension _HashTable {
  @inlinable
  internal func isValid(_ i: Index) -> Bool {
    return _isValid(i.bucket)
  }

  @usableFromInline
  @_effects(readonly)
  internal func isOccupied(_ i: Index) -> Bool {
    return _isValid(i.bucket) && _isOccupied(i.bucket)
  }

  @usableFromInline
  @_effects(readonly)
  internal func checkOccupied(_ i: Index) {
    _precondition(isOccupied(i),
      "Attempting to access Collection elements using an invalid Index")
  }

  @usableFromInline
  internal var startIndex: Index {
    @_effects(readonly)
    get {
      // We start at "bucket after -1" instead of "0" because we need to find
      // the first occupied slot.
      return Index(bucket: _occupiedBucket(after: -1))
    }
  }

  @inlinable
  internal var endIndex: Index {
    return Index(bucket: bucketCount)
  }

  @usableFromInline
  @_effects(readonly)
  internal func index(after i: Index) -> Index {
    checkOccupied(i)
    return Index(bucket: _occupiedBucket(after: i.bucket))
  }

  /// Return the bucket for the first member that may have a matching hash
  /// value, or if there's no such member, return an unoccupied bucket that is
  /// suitable for inserting a new member with the specified hash value.
  @usableFromInline
  @_effects(readonly)
  internal func lookupFirst(hashValue: Int) -> (index: Index, found: Bool) {
    let index = Index(bucket: hashValue & bucketMask)
    let entry = MapEntry.forHashValue(hashValue)
    return _lookupChain(startingAt: index, lookingFor: entry)
  }

  /// Return the next bucket after `bucket` in the collision chain for the
  /// specified hash value. `bucket` must have been returned by `lookupFirst` or
  /// `lookupNext`, with `found == true`.
  @usableFromInline
  @_effects(readonly)
  internal func lookupNext(
    hashValue: Int,
    after index: Index
  ) -> (index: Index, found: Bool) {
    let bucket = _succ(index.bucket)
    let entry = MapEntry.forHashValue(hashValue)
    return _lookupChain(startingAt: Index(bucket: bucket), lookingFor: entry)
  }

  internal func _lookupChain(
    startingAt index: Index,
    lookingFor entry: MapEntry
  ) -> (index: Index, found: Bool) {
    var bucket = index.bucket
    // We guarantee there's always a hole in the table, so we just loop until we
    // find one.
    while true {
      switch map[bucket] {
      case entry:
        return (Index(bucket: bucket), true)
      case MapEntry.unoccupied:
        return (Index(bucket: bucket), false)
      default:
        bucket = _succ(bucket)
      }
    }
  }


  @usableFromInline
  @_effects(releasenone)
  internal mutating func copyContents(of other: _HashTable) {
    _sanityCheck(scale == other.scale)
    self.count = other.count
    self.map.assign(from: other.map, count: self.bucketCount)
  }

  /// Insert a new entry with the specified hash value into the table.
  /// The entry must not already exist in the table -- duplicates are ignored.
  @usableFromInline
  @_effects(releasenone)
  internal mutating func insertNew(hashValue: Int) -> Index {
    _sanityCheck(count < capacity)
    let bucket = _nextHole(atOrAfter: hashValue & bucketMask)
    map[bucket] = MapEntry.forHashValue(hashValue)
    count += 1
    return Index(bucket: bucket)
  }

  /// Insert a new entry for an element with the specified hash value at
  /// `bucket`. The bucket must have been returned by `lookupFirst` or
  /// `lookupNext` for the same hash value, with `found == false`.
  @usableFromInline
  @_effects(releasenone)
  internal mutating func insert(hashValue: Int, at index: Index) {
    _sanityCheck(count < capacity)
    _sanityCheck(!map[index.bucket].isOccupied)
    map[index.bucket] = MapEntry.forHashValue(hashValue)
    count += 1
  }

  internal mutating func removeAll() {
    map.assign(repeating: .unoccupied, count: bucketCount)
    count = 0
  }

  @inline(__always)
  internal mutating func delete<D: _HashTableDelegate>(
    at index: Index,
    hashValue: Int,
    with delegate: D
  ) {
    _sanityCheck(map[index.bucket] == MapEntry.forHashValue(hashValue))

    // Remove the element.
    self.count -= 1

    let idealBucket = hashValue & bucketMask

    // If we've put a hole in a chain of contiguous elements, some element after
    // the hole may belong where the new hole is.
    var hole = index.bucket

    // Find the first and last buckets in the contiguous chain containing hole.
    let start = _prevHole(before: idealBucket)
    let end = _pred(_nextHole(after: hole))

    // Relocate out-of-place elements in the chain, repeating until none are
    // found.
    while hole != end {
      // Walk backwards from the end of the chain looking for something out of
      // place.
      // FIXME: Walking forwards may be more efficient if we expect long chains.
      var candidate = end
      while candidate != hole {
        let candidateHash = delegate.hashValue(at: Index(bucket: candidate))
        _sanityCheck(map[candidate] == MapEntry.forHashValue(candidateHash))
        let ideal = candidateHash & bucketMask

        // Does this element belong between start and hole?  We need two
        // separate tests depending on whether [start, hole] wraps around the
        // end of the storage.
        let c0 = ideal >= start
        let c1 = ideal <= hole
        if start <= hole ? (c0 && c1) : (c0 || c1) {
          break // Found it
        }
        candidate = _pred(candidate)
      }

      if candidate == hole {
        // No out-of-place elements found. It is safe to leave the hole in
        // place; we're done adjusting.
        break
      }

      // Move the found element into the hole.
      map[hole] = map[candidate]
      delegate.moveEntry(
        from: Index(bucket: candidate),
        to: Index(bucket: hole))
      hole = candidate
    }
    // Mark new hole as empty.
    map[hole] = .unoccupied
  }
}
