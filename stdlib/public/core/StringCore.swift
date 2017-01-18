//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

/// The core implementation of a highly-optimizable String that
/// can store both ASCII and UTF-16, and can wrap native Swift
/// _StringBuffer or NSString instances.
///
/// Usage note: when elements are 8 bits wide, this code may
/// dereference one past the end of the byte array that it owns, so
/// make sure that storage is allocated!  You want a null terminator
/// anyway, so it shouldn't be a burden.
//
// Implementation note: We try hard to avoid branches in this code, so
// for example we use integer math to avoid switching on the element
// size with the ternary operator.  This is also the cause of the
// extra element requirement for 8 bit elements.  See the
// implementation of subscript(Int) -> UTF16.CodeUnit below for details.
@_fixed_layout
public struct _StringCore {
  //===--------------------------------------------------------------------===//
  // Internals
  public var _baseAddress: UnsafeMutableRawPointer?
  var _countAndFlags: UInt
  public var _owner: AnyObject?

  /// (private) create the implementation of a string from its component parts.
  init(
    baseAddress: UnsafeMutableRawPointer?,
    _countAndFlags: UInt,
    owner: AnyObject?
  ) {
    self._baseAddress = baseAddress
    self._countAndFlags = _countAndFlags
    self._owner = owner
    _invariantCheck()
  }

  func _invariantCheck() {
    // Note: this code is intentionally #if'ed out.  It unconditionally
    // accesses lazily initialized globals, and thus it is a performance burden
    // in non-checked builds.
#if INTERNAL_CHECKS_ENABLED
    _sanityCheck(count >= 0)

    if _baseAddress == nil {
#if _runtime(_ObjC)
      _sanityCheck(hasCocoaBuffer,
        "Only opaque cocoa strings may have a null base pointer")
#endif
      _sanityCheck(elementWidth == 2,
        "Opaque cocoa strings should have an elementWidth of 2")
    }
    else if _baseAddress == _emptyStringBase {
      _sanityCheck(!hasCocoaBuffer)
      _sanityCheck(count == 0, "Empty string storage with non-zero count")
      _sanityCheck(_owner == nil, "String pointing at empty storage has owner")
    }
    else if let buffer = nativeBuffer {
      _sanityCheck(!hasCocoaBuffer)
      _sanityCheck(elementWidth == buffer.elementWidth,
        "_StringCore elementWidth doesn't match its buffer's")
      _sanityCheck(_baseAddress! >= buffer.start)
      _sanityCheck(_baseAddress! <= buffer.usedEnd)
      _sanityCheck(_pointer(toElementAt: count) <= buffer.usedEnd)
    }
#endif
  }

  /// Bitmask for the count part of `_countAndFlags`.
  var _countMask: UInt {
    return UInt.max >> 2
  }

  /// Bitmask for the flags part of `_countAndFlags`.
  var _flagMask: UInt {
    return ~_countMask
  }

  /// Value by which to multiply a 2nd byte fetched in order to
  /// assemble a UTF-16 code unit from our contiguous storage.  If we
  /// store ASCII, this will be zero.  Otherwise, it will be 0x100.
  var _highByteMultiplier: UTF16.CodeUnit {
    return UTF16.CodeUnit(elementShift) << 8
  }

  /// Returns a pointer to the Nth element of contiguous
  /// storage.  Caveats: The string must have contiguous storage; the
  /// element may be 1 or 2 bytes wide, depending on elementWidth; the
  /// result may be null if the string is empty.
  func _pointer(toElementAt n: Int) -> UnsafeMutableRawPointer {
    _sanityCheck(hasContiguousStorage && n >= 0 && n <= count)
    return _baseAddress! + (n << elementShift)
  }

  static func _copyElements(
    _ srcStart: UnsafeMutableRawPointer, srcElementWidth: Int,
    dstStart: UnsafeMutableRawPointer, dstElementWidth: Int,
    count: Int
  ) {
    // Copy the old stuff into the new storage
    if _fastPath(srcElementWidth == dstElementWidth) {
      // No change in storage width; we can use memcpy
      _memcpy(
        dest: dstStart,
        src: srcStart,
        size: UInt(count << (srcElementWidth - 1)))
    }
    else if srcElementWidth < dstElementWidth {
      // Widening ASCII to UTF-16; we need to copy the bytes manually
      var dest = dstStart.assumingMemoryBound(to: UTF16.CodeUnit.self)
      var src = srcStart.assumingMemoryBound(to: UTF8.CodeUnit.self)
      let srcEnd = src + count
      while src != srcEnd {
        dest.pointee = UTF16.CodeUnit(src.pointee)
        dest += 1
        src += 1
      }
    }
    else {
      // Narrowing UTF-16 to ASCII; we need to copy the bytes manually
      var dest = dstStart.assumingMemoryBound(to: UTF8.CodeUnit.self)
      var src = srcStart.assumingMemoryBound(to: UTF16.CodeUnit.self)
      let srcEnd = src + count
      while src != srcEnd {
        dest.pointee = UTF8.CodeUnit(src.pointee)
        dest += 1
        src += 1
      }
    }
  }

  //===--------------------------------------------------------------------===//
  // Initialization
  public init(
    baseAddress: UnsafeMutableRawPointer?,
    count: Int,
    elementShift: Int,
    hasCocoaBuffer: Bool,
    owner: AnyObject?
  ) {
    _sanityCheck(elementShift == 0 || elementShift == 1)
    self._baseAddress = baseAddress

    self._countAndFlags
      = (UInt(elementShift) << (UInt._sizeInBits - 1))
      | ((hasCocoaBuffer ? 1 : 0) << (UInt._sizeInBits - 2))
      | UInt(count)

    self._owner = owner
    _sanityCheck(UInt(count) & _flagMask == 0, "String too long to represent")
    _invariantCheck()
  }

  /// Create a _StringCore that covers the entire length of the _StringBuffer.
  init(_ buffer: _StringBuffer) {
    self = _StringCore(
      baseAddress: buffer.start,
      count: buffer.usedCount,
      elementShift: buffer.elementShift,
      hasCocoaBuffer: false,
      owner: buffer._anyObject
    )
  }

  /// Create the implementation of an empty string.
  ///
  /// - Note: There is no null terminator in an empty string.
  public init() {
    self._baseAddress = _emptyStringBase
    self._countAndFlags = 0
    self._owner = nil
    _invariantCheck()
  }

  //===--------------------------------------------------------------------===//
  // Properties

  /// The number of elements stored
  /// - Complexity: O(1).
  public var count: Int {
    get {
      return Int(_countAndFlags & _countMask)
    }
    set(newValue) {
      _sanityCheck(UInt(newValue) & _flagMask == 0)
      _countAndFlags = (_countAndFlags & _flagMask) | UInt(newValue)
    }
  }

  /// Left shift amount to apply to an offset N so that when
  /// added to a UnsafeMutableRawPointer, it traverses N elements.
  var elementShift: Int {
    return Int(_countAndFlags >> (UInt._sizeInBits - 1))
  }

  /// The number of bytes per element.
  ///
  /// If the string does not have an ASCII buffer available (including the case
  /// when we don't have a utf16 buffer) then it equals 2.
  public var elementWidth: Int {
    return elementShift &+ 1
  }

  public var hasContiguousStorage: Bool {
#if _runtime(_ObjC)
    return _fastPath(_baseAddress != nil)
#else
    return true
#endif
  }

  /// Are we using an `NSString` for storage?
  public var hasCocoaBuffer: Bool {
    return Int((_countAndFlags << 1)._value) < 0
  }

  public var startASCII: UnsafeMutablePointer<UTF8.CodeUnit> {
    _sanityCheck(elementWidth == 1, "String does not contain contiguous ASCII")
    return _baseAddress!.assumingMemoryBound(to: UTF8.CodeUnit.self)
  }

  /// True iff a contiguous ASCII buffer available.
  public var isASCII: Bool {
    return elementWidth == 1
  }

  public var startUTF16: UnsafeMutablePointer<UTF16.CodeUnit> {
    _sanityCheck(
      count == 0 || elementWidth == 2,
      "String does not contain contiguous UTF-16")
    return _baseAddress!.assumingMemoryBound(to: UTF16.CodeUnit.self)
  }

  public var asciiBuffer: UnsafeMutableBufferPointer<UTF8.CodeUnit>? {
    if elementWidth != 1 {
      return nil
    }
    return UnsafeMutableBufferPointer(start: startASCII, count: count)
  }

  /// the native _StringBuffer, if any, or `nil`.
  public var nativeBuffer: _StringBuffer? {
    if !hasCocoaBuffer {
      return _owner.map {
        unsafeBitCast($0, to: _StringBuffer.self)
      }
    }
    return nil
  }

#if _runtime(_ObjC)
  /// the Cocoa String buffer, if any, or `nil`.
  public var cocoaBuffer: _CocoaString? {
    if hasCocoaBuffer {
      return _owner
    }
    return nil
  }
#endif

  //===--------------------------------------------------------------------===//
  // slicing

  /// Returns the given sub-`_StringCore`.
  public subscript(bounds: Range<Int>) -> _StringCore {
    _precondition(
      bounds.lowerBound >= 0,
      "subscript: subrange start precedes String start")

    _precondition(
      bounds.upperBound <= count,
      "subscript: subrange extends past String end")

    let newCount = bounds.upperBound - bounds.lowerBound
    _sanityCheck(UInt(newCount) & _flagMask == 0)

    if hasContiguousStorage {
      return _StringCore(
        baseAddress: _pointer(toElementAt: bounds.lowerBound),
        _countAndFlags: (_countAndFlags & _flagMask) | UInt(newCount),
        owner: _owner)
    }
#if _runtime(_ObjC)
    return _cocoaStringSlice(self, bounds)
#else
    _sanityCheckFailure("subscript: non-native string without objc runtime")
#endif
  }

  /// Get the Nth UTF-16 Code Unit stored.
  @_versioned
  func _nthContiguous(_ position: Int) -> UTF16.CodeUnit {
    let p =
        UnsafeMutablePointer<UInt8>(_pointer(toElementAt: position)._rawValue)
    // Always dereference two bytes, but when elements are 8 bits we
    // multiply the high byte by 0.
    // FIXME(performance): use masking instead of multiplication.
#if _endian(little)
    return UTF16.CodeUnit(p.pointee)
      + UTF16.CodeUnit((p + 1).pointee) * _highByteMultiplier
#else
    return _highByteMultiplier == 0 ? UTF16.CodeUnit(p.pointee) : UTF16.CodeUnit((p + 1).pointee) + UTF16.CodeUnit(p.pointee) * _highByteMultiplier
#endif
  }

  /// Get the Nth UTF-16 Code Unit stored.
  public subscript(position: Int) -> UTF16.CodeUnit {
    @inline(__always)
    get {
      _precondition(
        position >= 0,
        "subscript: index precedes String start")

      _precondition(
        position <= count,
        "subscript: index points past String end")

      if _fastPath(_baseAddress != nil) {
        return _nthContiguous(position)
      }
#if _runtime(_ObjC)
      return _cocoaStringSubscript(self, position)
#else
      _sanityCheckFailure("subscript: non-native string without objc runtime")
#endif
    }
  }

  /// Write the string, in the given encoding, to output.
  func encode<Encoding: UnicodeCodec>(
    _ encoding: Encoding.Type,
    into processCodeUnit: (Encoding.CodeUnit) -> Void)
  {
    if _fastPath(_baseAddress != nil) {
      if _fastPath(elementWidth == 1) {
        for x in UnsafeBufferPointer(
          start: _baseAddress!.assumingMemoryBound(to: UTF8.CodeUnit.self),
          count: count
        ) {
          Encoding.encode(UnicodeScalar(x), into: processCodeUnit)
        }
      }
      else {
        let hadError = transcode(
          UnsafeBufferPointer(
            start: _baseAddress!.assumingMemoryBound(to: UTF16.CodeUnit.self),
            count: count
          ).makeIterator(),
          from: UTF16.self,
          to: encoding,
          stoppingOnError: true,
          into: processCodeUnit
        )
        _sanityCheck(!hadError, "Swift.String with native storage should not have unpaired surrogates")
      }
    }
    else if hasCocoaBuffer {
#if _runtime(_ObjC)
      _StringCore(
        _cocoaStringToContiguous(
          source: cocoaBuffer!, range: 0..<count, minimumCapacity: 0)
      ).encode(encoding, into: processCodeUnit)
#else
      _sanityCheckFailure("encode: non-native string without objc runtime")
#endif
    }
  }

  /// Attempt to claim unused capacity in the String's existing
  /// native buffer, if any.  Return zero and a pointer to the claimed
  /// storage if successful. Otherwise, returns a suggested new
  /// capacity and a null pointer.
  ///
  /// - Note: If successful, effectively appends garbage to the String
  ///   until it has newSize UTF-16 code units; you must immediately copy
  ///   valid UTF-16 into that storage.
  ///
  /// - Note: If unsuccessful because of insufficient space in an
  ///   existing buffer, the suggested new capacity will at least double
  ///   the existing buffer's storage.
  mutating func _claimCapacity(
    _ newSize: Int, minElementWidth: Int) -> (Int, UnsafeMutableRawPointer?) {
    if _fastPath((nativeBuffer != nil) && elementWidth >= minElementWidth) {
      var buffer = nativeBuffer!

      // In order to grow the substring in place, this _StringCore should point
      // at the substring at the end of a _StringBuffer.  Otherwise, some other
      // String is using parts of the buffer beyond our last byte.
      let usedStart = _pointer(toElementAt:0)
      let usedEnd = _pointer(toElementAt:count)

      // Attempt to claim unused capacity in the buffer
      if _fastPath(
        buffer.grow(
          oldBounds: UnsafeRawPointer(usedStart)..<UnsafeRawPointer(usedEnd),
          newUsedCount: newSize)
      ) {
        count = newSize
        return (0, usedEnd)
      }
      else if newSize > buffer.capacity {
        // Growth failed because of insufficient storage; double the size
        return (Swift.max(_growArrayCapacity(buffer.capacity), newSize), nil)
      }
    }
    return (newSize, nil)
  }

  /// Ensure that this String references a _StringBuffer having
  /// a capacity of at least newSize elements of at least the given width.
  /// Effectively appends garbage to the String until it has newSize
  /// UTF-16 code units.  Returns a pointer to the garbage code units;
  /// you must immediately copy valid data into that storage.
  mutating func _growBuffer(
    _ newSize: Int, minElementWidth: Int
  ) -> UnsafeMutableRawPointer {
    let (newCapacity, existingStorage)
      = _claimCapacity(newSize, minElementWidth: minElementWidth)

    if _fastPath(existingStorage != nil) {
      return existingStorage!
    }

    let oldCount = count

    _copyInPlace(
      newSize: newSize,
      newCapacity: newCapacity,
      minElementWidth: minElementWidth)

    return _pointer(toElementAt:oldCount)
  }

  /// Replace the storage of self with a native _StringBuffer having a
  /// capacity of at least newCapacity elements of at least the given
  /// width.  Effectively appends garbage to the String until it has
  /// newSize UTF-16 code units.
  mutating func _copyInPlace(
    newSize: Int, newCapacity: Int, minElementWidth: Int
  ) {
    _sanityCheck(newCapacity >= newSize)
    let oldCount = count

    // Allocate storage.
    let newElementWidth =
      minElementWidth >= elementWidth
      ? minElementWidth
      : isRepresentableAsASCII() ? 1 : 2

    let newStorage = _StringBuffer(capacity: newCapacity, initialSize: newSize,
                                   elementWidth: newElementWidth)

    if hasContiguousStorage {
      _StringCore._copyElements(
        _baseAddress!, srcElementWidth: elementWidth,
        dstStart: UnsafeMutableRawPointer(newStorage.start),
        dstElementWidth: newElementWidth, count: oldCount)
    }
    else {
#if _runtime(_ObjC)
      // Opaque cocoa buffers might not store ASCII, so assert that
      // we've allocated for 2-byte elements.
      // FIXME: can we get Cocoa to tell us quickly that an opaque
      // string is ASCII?  Do we care much about that edge case?
      _sanityCheck(newStorage.elementShift == 1)
      _cocoaStringReadAll(cocoaBuffer!,
        newStorage.start.assumingMemoryBound(to: UTF16.CodeUnit.self))
#else
      _sanityCheckFailure("_copyInPlace: non-native string without objc runtime")
#endif
    }

    self = _StringCore(newStorage)
  }

  /// Append `c` to `self`.
  ///
  /// - Complexity: O(1) when amortized over repeated appends of equal
  ///   character values.
  mutating func append(_ c: UnicodeScalar) {
    let width = UTF16.width(c)
    append(
      width == 2 ? UTF16.leadSurrogate(c) : UTF16.CodeUnit(c.value),
      width == 2 ? UTF16.trailSurrogate(c) : nil
    )
  }

  /// Append `u` to `self`.
  ///
  /// - Complexity: Amortized O(1).
  public mutating func append(_ u: UTF16.CodeUnit) {
    append(u, nil)
  }

  mutating func append(_ u0: UTF16.CodeUnit, _ u1: UTF16.CodeUnit?) {
    _invariantCheck()
    let minBytesPerCodeUnit = u0 <= 0x7f ? 1 : 2
    let utf16Width = u1 == nil ? 1 : 2

    let destination = _growBuffer(
      count + utf16Width, minElementWidth: minBytesPerCodeUnit)

    if _fastPath(elementWidth == 1) {
      _sanityCheck(_pointer(toElementAt:count) == destination + 1)

      destination.assumingMemoryBound(to: UTF8.CodeUnit.self)[0]
        = UTF8.CodeUnit(u0)
    }
    else {
      let destination16
      = destination.assumingMemoryBound(to: UTF16.CodeUnit.self)

      destination16[0] = u0
      if u1 != nil {
        destination16[1] = u1!
      }
    }
    _invariantCheck()
  }

  @inline(never)
  mutating func append(_ rhs: _StringCore) {
    _invariantCheck()
    let minElementWidth
    = elementWidth >= rhs.elementWidth
      ? elementWidth
      : rhs.isRepresentableAsASCII() ? 1 : 2

    let destination = _growBuffer(
      count + rhs.count, minElementWidth: minElementWidth)

    if _fastPath(rhs.hasContiguousStorage) {
      _StringCore._copyElements(
        rhs._baseAddress!, srcElementWidth: rhs.elementWidth,
        dstStart: destination, dstElementWidth:elementWidth, count: rhs.count)
    }
    else {
#if _runtime(_ObjC)
      _sanityCheck(elementWidth == 2)
      _cocoaStringReadAll(rhs.cocoaBuffer!,
        destination.assumingMemoryBound(to: UTF16.CodeUnit.self))
#else
      _sanityCheckFailure("subscript: non-native string without objc runtime")
#endif
    }
    _invariantCheck()
  }

  /// Returns `true` iff the contents of this string can be
  /// represented as pure ASCII.
  ///
  /// - Complexity: O(*n*) in the worst case.
  func isRepresentableAsASCII() -> Bool {
    if _slowPath(!hasContiguousStorage) {
      return false
    }
    if _fastPath(elementWidth == 1) {
      return true
    }
    let unsafeBuffer =
      UnsafeBufferPointer(
        start: _baseAddress!.assumingMemoryBound(to: UTF16.CodeUnit.self),
        count: count)
    return !unsafeBuffer.contains { $0 > 0x7f }
  }
}

extension _StringCore : RandomAccessCollection {
  
  public typealias Indices = CountableRange<Int>

  public // @testable
  var startIndex: Int {
    return 0
  }

  public // @testable
  var endIndex: Int {
    return count
  }
}

extension _StringCore : RangeReplaceableCollection {

  /// Replace the elements within `bounds` with `newElements`.
  ///
  /// - Complexity: O(`bounds.count`) if `bounds.upperBound
  ///   == self.endIndex` and `newElements.isEmpty`, O(*n*) otherwise.
  public mutating func replaceSubrange<C>(
    _ bounds: Range<Int>,
    with newElements: C
  ) where C : Collection, C.Iterator.Element == UTF16.CodeUnit {
    _precondition(
      bounds.lowerBound >= 0,
      "replaceSubrange: subrange start precedes String start")

    _precondition(
      bounds.upperBound <= count,
      "replaceSubrange: subrange extends past String end")

    let width = elementWidth == 2 || newElements.contains { $0 > 0x7f } ? 2 : 1
    let replacementCount = numericCast(newElements.count) as Int
    let replacedCount = bounds.count
    let tailCount = count - bounds.upperBound
    let growth = replacementCount - replacedCount
    let newCount = count + growth

    // Successfully claiming capacity only ensures that we can modify
    // the newly-claimed storage without observably mutating other
    // strings, i.e., when we're appending.  Already-used characters
    // can only be mutated when we have a unique reference to the
    // buffer.
    let appending = bounds.lowerBound == endIndex

    let existingStorage = !hasCocoaBuffer && (
      appending || isKnownUniquelyReferenced(&_owner)
    ) ? _claimCapacity(newCount, minElementWidth: width).1 : nil

    if _fastPath(existingStorage != nil) {
      let rangeStart = _pointer(toElementAt:bounds.lowerBound)
      let tailStart = rangeStart + (replacedCount << elementShift)

      if growth > 0 {
        (tailStart + (growth << elementShift)).copyBytes(
          from: tailStart, count: tailCount << elementShift)
      }

      if _fastPath(elementWidth == 1) {
        var dst = rangeStart.assumingMemoryBound(to: UTF8.CodeUnit.self)
        for u in newElements {
          dst.pointee = UInt8(truncatingBitPattern: u)
          dst += 1
        }
      }
      else {
        var dst = rangeStart.assumingMemoryBound(to: UTF16.CodeUnit.self)
        for u in newElements {
          dst.pointee = u
          dst += 1
        }
      }

      if growth < 0 {
        (tailStart + (growth << elementShift)).copyBytes(
          from: tailStart, count: tailCount << elementShift)
      }
    }
    else {
      var r = _StringCore(
        _StringBuffer(
          capacity: newCount,
          initialSize: 0,
          elementWidth:
            width == 1 ? 1
            : isRepresentableAsASCII() && !newElements.contains { $0 > 0x7f } ? 1
            : 2
        ))
      r.append(contentsOf: self[0..<bounds.lowerBound])
      r.append(contentsOf: newElements)
      r.append(contentsOf: self[bounds.upperBound..<count])
      self = r
    }
  }

  public mutating func reserveCapacity(_ n: Int) {
    if _fastPath(!hasCocoaBuffer) {
      if _fastPath(isKnownUniquelyReferenced(&_owner)) {

        let bounds: Range<UnsafeRawPointer>
          = UnsafeRawPointer(_pointer(toElementAt:0))
          ..< UnsafeRawPointer(_pointer(toElementAt:count))

        if _fastPath(nativeBuffer!.hasCapacity(n, forSubRange: bounds)) {
          return
        }
      }
    }
    _copyInPlace(
      newSize: count,
      newCapacity: Swift.max(count, n),
      minElementWidth: 1)
  }

  public mutating func append<S : Sequence>(contentsOf s: S)
    where S.Iterator.Element == UTF16.CodeUnit {
    var width = elementWidth
    if width == 1 {
      if let hasNonAscii = s._preprocessingPass({
          s.contains { $0 > 0x7f }
        }) {
        width = hasNonAscii ? 2 : 1
      }
    }

    let growth = s.underestimatedCount
    var iter = s.makeIterator()

    if _fastPath(growth > 0) {
      let newSize = count + growth
      let destination = _growBuffer(newSize, minElementWidth: width)
      if elementWidth == 1 {
        let destination8
          = destination.assumingMemoryBound(to: UTF8.CodeUnit.self)
        for i in 0..<growth {
          destination8[i] = UTF8.CodeUnit(iter.next()!)
        }
      }
      else {
        let destination16
          = destination.assumingMemoryBound(to: UTF16.CodeUnit.self)
        for i in 0..<growth {
          destination16[i] = iter.next()!
        }
      }
    }
    // Append any remaining elements
    for u in IteratorSequence(iter) {
      self.append(u)
    }
  }
}

// Used to support a tighter invariant: all strings with contiguous
// storage have a non-NULL base address.
var _emptyStringStorage: UInt32 = 0

var _emptyStringBase: UnsafeMutableRawPointer {
  return UnsafeMutableRawPointer(Builtin.addressof(&_emptyStringStorage))
}
