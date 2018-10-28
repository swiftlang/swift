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

// COW helpers
extension _StringGuts {
  internal var nativeCapacity: Int? {
      guard hasNativeStorage else { return nil }
      return _object.nativeStorage.capacity
  }

  internal var nativeUnusedCapacity: Int? {
      guard hasNativeStorage else { return nil }
      return _object.nativeStorage.unusedCapacity
  }

  // If natively stored and uniquely referenced, return the storage's total
  // capacity. Otherwise, nil.
  internal var uniqueNativeCapacity: Int? {
    @inline(__always) mutating get {
      guard isUniqueNative else { return nil }
      return _object.nativeStorage.capacity
    }
  }

  // If natively stored and uniquely referenced, return the storage's spare
  // capacity. Otherwise, nil.
  internal var uniqueNativeUnusedCapacity: Int? {
    @inline(__always) mutating get {
      guard isUniqueNative else { return nil }
      return _object.nativeStorage.unusedCapacity
    }
  }

  @usableFromInline // @testable
  internal var isUniqueNative: Bool {
    @inline(__always) mutating get {
      // Note: mutating so that self is `inout`.
      guard hasNativeStorage else { return false }
      defer { _fixLifetime(self) }
      var bits: UInt = _object.largeAddressBits
      return _isUnique_native(&bits)
    }
  }
}

// Range-replaceable operation support
extension _StringGuts {
  internal mutating func reserveCapacity(_ n: Int) {
    // Check if there's nothing to do
    if n <= _SmallString.capacity { return }
    if let currentCap = self.uniqueNativeCapacity, currentCap >= n { return }

    // Grow
    self.grow(n)
  }

  // Grow to accomodate at least `n` code units
  private mutating func grow(_ n: Int) {
    defer { self._invariantCheck() }

    _sanityCheck(
      self.uniqueNativeCapacity == nil || self.uniqueNativeCapacity! < n)

    let growthTarget = Swift.max(n, (self.uniqueNativeCapacity ?? 0) * 2)

    if _fastPath(isFastUTF8) {
      let isASCII = self.isASCII
      let storage = self.withFastUTF8 {
        _StringStorage.create(
          initializingFrom: $0, capacity: growthTarget, isASCII: isASCII)
      }

      self = _StringGuts(storage)
      return
    }

    _foreignGrow(growthTarget)
  }

  @inline(never) // slow-path
  private mutating func _foreignGrow(_ n: Int) {
    // TODO(UTF8 perf): skip the intermediary arrays
    let selfUTF8 = Array(String(self).utf8)
    selfUTF8.withUnsafeBufferPointer {
      self = _StringGuts(_StringStorage.create(
        initializingFrom: $0, capacity: n, isASCII: self.isASCII))
    }
  }

  // Ensure unique native storage with sufficient capacity for the following
  // append.
  private mutating func prepareForAppendInPlace(
    otherUTF8Count otherCount: Int
  ) {
    defer {
      _sanityCheck(self.uniqueNativeUnusedCapacity != nil,
        "growth should produce uniqueness")
      _sanityCheck(self.uniqueNativeUnusedCapacity! >= otherCount,
        "growth should produce enough capacity")
    }

    // See if we can accomodate without growing or copying. If we have
    // sufficient capacity, we do not need to grow, and we can skip the copy if
    // unique. Otherwise, growth is required.
    let sufficientCapacity: Bool
    if let unused = self.nativeUnusedCapacity, unused >= otherCount {
      sufficientCapacity = true
    } else {
      sufficientCapacity = false
    }
    if self.isUniqueNative && sufficientCapacity {
      return
    }

    let totalCount = self.utf8Count + otherCount

    // Non-unique storage: just make a copy of the appropriate size, otherwise
    // grow like an array.
    let growthTarget: Int
    if sufficientCapacity {
      growthTarget = totalCount
    } else {
      growthTarget = Swift.max(
        totalCount, _growArrayCapacity(nativeCapacity ?? 0))
    }
    self.grow(growthTarget)
  }

  internal mutating func append(_ other: _StringGuts) {
    // TODO(UTF8 perf): Minor potential perf win to elevating smol fast-path
    // prior to slicing.
    append(_StringGutsSlice(other))
  }

  internal mutating func append(_ slicedOther: _StringGutsSlice) {
    defer { self._invariantCheck() }

    // Try to form a small string if possible
    if !hasNativeStorage {
      if let smol = _SmallString(
        base: _StringGutsSlice(self), appending: slicedOther
      ) {
        self = _StringGuts(smol)
        return
      }
    }

    prepareForAppendInPlace(otherUTF8Count: slicedOther.utf8Count)

    if slicedOther.isFastUTF8 {
      let otherIsASCII = slicedOther.isASCII
      slicedOther.withFastUTF8 { otherUTF8 in
        self.appendInPlace(otherUTF8, isASCII: otherIsASCII)
      }
      return
    }

    _foreignAppendInPlace(slicedOther)
  }

  internal mutating func appendInPlace(
    _ other: UnsafeBufferPointer<UInt8>, isASCII: Bool
  ) {
    self._object.nativeStorage.appendInPlace(other, isASCII: isASCII)

    // We re-initialize from the modified storage to pick up new count, flags,
    // etc.
    self = _StringGuts(self._object.nativeStorage)
  }

  @inline(never) // slow-path
  private mutating func _foreignAppendInPlace(_ other: _StringGutsSlice) {
    _sanityCheck(!other.isFastUTF8)
    _sanityCheck(self.uniqueNativeUnusedCapacity != nil)

    var iter = Substring(other).utf8.makeIterator()
    self._object.nativeStorage.appendInPlace(&iter, isASCII: other.isASCII)

    // We re-initialize from the modified storage to pick up new count, flags,
    // etc.
    self = _StringGuts(self._object.nativeStorage)
  }

  internal mutating func clear() {
    guard isUniqueNative else {
      self = _StringGuts()
      return
    }

    // Reset the count
    _object.nativeStorage.clear()
    self = _StringGuts(_object.nativeStorage)
  }

  internal mutating func remove(from lower: Index, to upper: Index) {
    let lowerOffset = lower.encodedOffset
    let upperOffset = upper.encodedOffset
    _sanityCheck(lower.transcodedOffset == 0 && upper.transcodedOffset == 0)
    _sanityCheck(lowerOffset <= upperOffset && upperOffset <= self.count)

    if isUniqueNative {
      _object.nativeStorage.remove(from: lowerOffset, to: upperOffset)
      // We re-initialize from the modified storage to pick up new count, flags,
      // etc.
      self = _StringGuts(self._object.nativeStorage)
      return
    }

    // TODO(UTF8 perf): Add append on guts taking range, use that
    var result = String()
    result.reserveCapacity(self.count &- (upperOffset &- lowerOffset))
    result.append(contentsOf: String(self)[..<lower])
    result.append(contentsOf: String(self)[upper...])
    self = result._guts
  }

  @inline(__always) // Always-specialize
  internal mutating func replaceSubrange<C>(
    _ bounds: Range<Index>,
    with newElements: C
  ) where C : Collection, C.Iterator.Element == Character {
    if isUniqueNative {
      if let replStr = newElements as? String, replStr._guts.isFastUTF8 {
        replStr._guts.withFastUTF8 {
          uniqueNativeReplaceSubrange(
            bounds, with: $0, isASCII: replStr._guts.isASCII)
        }
        return
      }
      // TODO(UTF8 perf): Iterate and insert
      // TODO(UTF8 merge): Needed for `reserveCapacity` test in
      //   validation-test/stdlib/String.swift
    }

    var result = String()
    let selfStr = String(self)
    result.append(contentsOf: selfStr[..<bounds.lowerBound])
    result.append(contentsOf: newElements)
    result.append(contentsOf: selfStr[bounds.upperBound...])
    self = result._guts
  }

  internal mutating func uniqueNativeReplaceSubrange(
    _ bounds: Range<Index>,
    with codeUnits: UnsafeBufferPointer<UInt8>,
    isASCII: Bool
  ) {
    let neededCapacity =
      bounds.lowerBound.encodedOffset
      + codeUnits.count + (self.count - bounds.upperBound.encodedOffset)
    reserveCapacity(neededCapacity)

    _sanityCheck(bounds.lowerBound.transcodedOffset == 0)
    _sanityCheck(bounds.upperBound.transcodedOffset == 0)

    _object.nativeStorage.replace(
      from: bounds.lowerBound.encodedOffset,
      to: bounds.upperBound.encodedOffset,
      with: codeUnits)
    self = _StringGuts(_object.nativeStorage)
  }
}



