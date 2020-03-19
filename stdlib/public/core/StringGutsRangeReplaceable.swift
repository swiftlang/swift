//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
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
  @inlinable
  internal init(_initialCapacity capacity: Int) {
    self.init()
    if _slowPath(capacity > _SmallString.capacity) {
      self.grow(capacity) // TODO: no factor should be applied
    }
  }

  internal mutating func reserveCapacity(_ n: Int) {
    // Check if there's nothing to do
    if n <= _SmallString.capacity { return }
    if let currentCap = self.uniqueNativeCapacity, currentCap >= n { return }

    // Grow
    self.grow(n) // TODO: no factor should be applied
  }

  // Grow to accomodate at least `n` code units
  @usableFromInline
  internal mutating func grow(_ n: Int) {
    defer { self._invariantCheck() }

    _internalInvariant(
      self.uniqueNativeCapacity == nil || self.uniqueNativeCapacity! < n)

    // TODO: Dont' do this! Growth should only happen for append...
    let growthTarget = Swift.max(n, (self.uniqueNativeCapacity ?? 0) * 2)

    if _fastPath(isFastUTF8) {
      let isASCII = self.isASCII
      let storage = self.withFastUTF8 {
        __StringStorage.create(
          initializingFrom: $0,
          codeUnitCapacity: growthTarget,
          isASCII: isASCII)
      }

      self = _StringGuts(storage)
      return
    }

    _foreignGrow(growthTarget)
  }

  @inline(never) // slow-path
  private mutating func _foreignGrow(_ n: Int) {
    let newString = String(_uninitializedCapacity: n) { buffer in
      guard let count = _foreignCopyUTF8(into: buffer) else {
       fatalError("String capacity was smaller than required")
      }
      return count
    }
    self = newString._guts
  }

  // Ensure unique native storage with sufficient capacity for the following
  // append.
  private mutating func prepareForAppendInPlace(
    totalCount: Int,
    otherUTF8Count otherCount: Int
  ) {
    defer {
      _internalInvariant(self.uniqueNativeUnusedCapacity != nil,
        "growth should produce uniqueness")
      _internalInvariant(self.uniqueNativeUnusedCapacity! >= otherCount,
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

    // If we have to resize anyway, and we fit in smol, we should have made one
    _internalInvariant(totalCount > _SmallString.capacity)

    // Non-unique storage: just make a copy of the appropriate size, otherwise
    // grow like an array.
    let growthTarget: Int
    if sufficientCapacity {
      growthTarget = totalCount
    } else {
      growthTarget = Swift.max(
        totalCount, _growArrayCapacity(nativeCapacity ?? 0))
    }
    self.grow(growthTarget) // NOTE: this already has exponential growth...
  }

  internal mutating func append(_ other: _StringGuts) {
    if self.isSmall && other.isSmall {
      if let smol = _SmallString(self.asSmall, appending: other.asSmall) {
        self = _StringGuts(smol)
        return
      }
    }
    append(_StringGutsSlice(other))
  }

  @inline(never)
  @_effects(readonly)
  private func _foreignConvertedToSmall() -> _SmallString {
    let smol = String(_uninitializedCapacity: _SmallString.capacity) { buffer in
      guard let count = _foreignCopyUTF8(into: buffer) else {
        fatalError("String capacity was smaller than required")
      }
      return count
    }
    _internalInvariant(smol._guts.isSmall)
    return smol._guts.asSmall
  }

  private func _convertedToSmall() -> _SmallString {
    _internalInvariant(utf8Count <= _SmallString.capacity)
    if _fastPath(isSmall) {
      return asSmall
    }
    if isFastUTF8 {
      return withFastUTF8 { _SmallString($0)! }
    }
    return _foreignConvertedToSmall()
  }

  internal mutating func append(_ slicedOther: _StringGutsSlice) {
    defer { self._invariantCheck() }

    let otherCount = slicedOther.utf8Count

    let totalCount = utf8Count + otherCount

    /*
     Goal: determine if we need to allocate new native capacity
     Possible scenarios in which we need to allocate:
     • Not uniquely owned and native: we can't use the capacity to grow into,
        have to become unique + native by allocating
     • Not enough capacity: have to allocate to grow

     Special case: a non-smol String that can fit in a smol String but doesn't
        meet the above criteria shouldn't throw away its buffer just to be smol.
        The reasoning here is that it may be bridged or have reserveCapacity'd
        in preparation for appending more later, in which case we would end up
        have to allocate anyway to convert back from smol.

        If we would have to re-allocate anyway then that's not a problem and we
        should just be smol.

        e.g. consider
        var str = "" // smol
        str.reserveCapacity(100) // large native unique
        str += "<html>" // don't convert back to smol here!
        str += htmlContents // because this would have to anyway!
     */
    let hasEnoughUsableSpace = isUniqueNative &&
      nativeUnusedCapacity! >= otherCount
    let shouldBeSmol = totalCount <= _SmallString.capacity &&
      (isSmall || !hasEnoughUsableSpace)

    if shouldBeSmol {
      let smolSelf = _convertedToSmall()
      let smolOther = String(Substring(slicedOther))._guts._convertedToSmall()
      // TODO: In-register slicing
      self = _StringGuts(_SmallString(smolSelf, appending: smolOther)!)
      return
    }

    prepareForAppendInPlace(totalCount: totalCount, otherUTF8Count: otherCount)

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
    _internalInvariant(!other.isFastUTF8)
    _internalInvariant(self.uniqueNativeUnusedCapacity != nil)

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
    let lowerOffset = lower._encodedOffset
    let upperOffset = upper._encodedOffset
    _internalInvariant(lower.transcodedOffset == 0 && upper.transcodedOffset == 0)
    _internalInvariant(lowerOffset <= upperOffset && upperOffset <= self.count)

    if isUniqueNative {
      _object.nativeStorage.remove(from: lowerOffset, to: upperOffset)
      // We re-initialize from the modified storage to pick up new count, flags,
      // etc.
      self = _StringGuts(self._object.nativeStorage)
      return
    }

    // TODO(cleanup): Add append on guts taking range, use that
    var result = String()
    // FIXME: It should be okay to get rid of excess capacity
    // here. rdar://problem/45635432
    result.reserveCapacity(
      nativeCapacity ?? (count &- (upperOffset &- lowerOffset)))
    result.append(contentsOf: String(self)[..<lower])
    result.append(contentsOf: String(self)[upper...])
    self = result._guts
  }

  internal mutating func replaceSubrange<C>(
    _ bounds: Range<Index>,
    with newElements: C
  ) where C: Collection, C.Iterator.Element == Character {
    if isUniqueNative {
      if let replStr = newElements as? String, replStr._guts.isFastUTF8 {
        replStr._guts.withFastUTF8 {
          uniqueNativeReplaceSubrange(
            bounds, with: $0, isASCII: replStr._guts.isASCII)
        }
        return
      }
      uniqueNativeReplaceSubrange(
        bounds, with: newElements.lazy.flatMap { $0.utf8 })
      return
    }

    var result = String()
    // FIXME: It should be okay to get rid of excess capacity
    // here. rdar://problem/45635432
    if let capacity = self.nativeCapacity {
      result.reserveCapacity(capacity)
    }
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
      bounds.lowerBound._encodedOffset
      + codeUnits.count + (self.count - bounds.upperBound._encodedOffset)
    reserveCapacity(neededCapacity)

    _internalInvariant(bounds.lowerBound.transcodedOffset == 0)
    _internalInvariant(bounds.upperBound.transcodedOffset == 0)

    _object.nativeStorage.replace(
      from: bounds.lowerBound._encodedOffset,
      to: bounds.upperBound._encodedOffset,
      with: codeUnits)
    self = _StringGuts(_object.nativeStorage)
  }

  internal mutating func uniqueNativeReplaceSubrange<C: Collection>(
    _ bounds: Range<Index>,
    with codeUnits: C
  ) where C.Element == UInt8 {
    let replCount = codeUnits.count

    let neededCapacity =
      bounds.lowerBound._encodedOffset
      + replCount + (self.count - bounds.upperBound._encodedOffset)
    reserveCapacity(neededCapacity)

    _internalInvariant(bounds.lowerBound.transcodedOffset == 0)
    _internalInvariant(bounds.upperBound.transcodedOffset == 0)

    _object.nativeStorage.replace(
      from: bounds.lowerBound._encodedOffset,
      to: bounds.upperBound._encodedOffset,
      with: codeUnits,
      replacementCount: replCount)
    self = _StringGuts(_object.nativeStorage)
  }
}

