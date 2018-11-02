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

internal
typealias _SmallUTF16StringBuffer = _FixedArray16<UInt16>

//
// NOTE: Small string is not available on 32-bit platforms (not enough bits!),
// but we don't want to #if-def all use sites (at least for now). So, provide a
// minimal unavailable interface.
//
#if arch(i386) || arch(arm)
// Helper method for declaring something as not supported in 32-bit. Use inside
// a function body inside a #if block so that callers don't have to be
// conditional.
@_transparent @inlinable
func unsupportedOn32bit() -> Never { _conditionallyUnreachable() }

// Trivial type declaration for type checking. Never present at runtime.
@_fixed_layout public struct _SmallUTF8String {}

#else
@_fixed_layout
public // @testable
struct _SmallUTF8String {
  typealias _RawBitPattern = (low: UInt, high: UInt)

  //
  // TODO: pretty ASCII art.
  //
  // TODO: endianess awareness day
  //
  // The low byte of the first word stores the first code unit. There is up to
  // 15 such code units encodable, with the second-highest byte of the second
  // word being the final code unit. The high byte of the final word stores the
  // count.
  //
  @usableFromInline
  var _storage: _RawBitPattern = (0,0)
  @inlinable
  @inline(__always)
  init() {
    self._storage = (0,0)
  }
}
#endif // 64-bit

//
// Small string creation interface
//
extension _SmallUTF8String {
  @inlinable
  public // @testable
  static var capacity: Int { return 15 }

#if _runtime(_ObjC)
  public // @testable
  init?(_cocoaString cocoa: _CocoaString) {
#if arch(i386) || arch(arm)
    return nil // Never form small strings on 32-bit
#else
    self.init()
    let len = self._withAllUnsafeMutableBytes { bufPtr -> Int? in
      guard let len = _bridgeASCIICocoaString(cocoa, intoUTF8: bufPtr),
            len <= _SmallUTF8String.capacity
      else {
        return nil
      }
      return len
    }
    guard let count = len else { return nil }
    _sanityCheck(self.count == 0, "overwrote count early?")

    self.count = count
    _invariantCheck()
#endif
  }
#endif // _runtime(_ObjC)

  @inlinable
  public // @testable
  init?<C: RandomAccessCollection>(_ codeUnits: C) where C.Element == UInt16 {
#if arch(i386) || arch(arm)
    return nil // Never form small strings on 32-bit
#else
    guard codeUnits.count <= _SmallUTF8String.capacity else { return nil }
    // TODO(TODO: JIRA): Just implement this directly

    self.init()
    var bufferIdx = 0
    for encodedScalar in Unicode._ParsingIterator(
      codeUnits: codeUnits.makeIterator(),
      parser: Unicode.UTF16.ForwardParser()
    ) {
      guard let transcoded = Unicode.UTF8.transcode(
        encodedScalar, from: Unicode.UTF16.self
      ) else {
        // FIXME: can this fail with unpaired surrogates?
        _sanityCheckFailure("UTF-16 should be transcodable to UTF-8")
        return nil
      }
      _sanityCheck(transcoded.count <= 4, "how?")
      guard bufferIdx + transcoded.count <= _SmallUTF8String.capacity else {
        return nil
      }
      for i in transcoded.indices {
        self._uncheckedSetCodeUnit(at: bufferIdx, to: transcoded[i])
        bufferIdx += 1
      }
    }
    _sanityCheck(self.count == 0, "overwrote count early?")
    self.count = bufferIdx

    // FIXME: support transcoding
    if !self.isASCII { return nil }

    _invariantCheck()
#endif
  }

  @inlinable
  public // @testable
  init?<C: RandomAccessCollection>(_ codeUnits: C) where C.Element == UInt8 {
#if arch(i386) || arch(arm)
    return nil // Never form small strings on 32-bit
#else
    let count = codeUnits.count
    guard count <= _SmallUTF8String.capacity else { return nil }
    self.init()
    self._withAllUnsafeMutableBytes { rawBufPtr in
      let bufPtr = UnsafeMutableBufferPointer(
        start: rawBufPtr.baseAddress.unsafelyUnwrapped.assumingMemoryBound(
          to: UInt8.self),
        count: rawBufPtr.count)
      var (itr, written) = codeUnits._copyContents(initializing: bufPtr)
      _sanityCheck(itr.next() == nil)
      _sanityCheck(count == written)
    }
    _sanityCheck(self.count == 0, "overwrote count early?")
    self.count = count

    // FIXME: support transcoding
    if !self.isASCII { return nil }

    _invariantCheck()
#endif
  }
}

//
// Small string read interface
//
extension _SmallUTF8String {
  @inlinable
  @inline(__always)
  func withUTF8CodeUnits<Result>(
    _ body: (UnsafeBufferPointer<UInt8>) throws -> Result
  ) rethrows -> Result {
#if arch(i386) || arch(arm)
    unsupportedOn32bit()
#else
    return try _withAllUnsafeBytes { bufPtr in
      let ptr = bufPtr.baseAddress._unsafelyUnwrappedUnchecked
        .assumingMemoryBound(to: UInt8.self)
      return try body(UnsafeBufferPointer(start: ptr, count: self.count))
    }
#endif
  }

  @inlinable
  @inline(__always)
  public // @testable
  func withTranscodedUTF16CodeUnits<Result>(
    _ body: (UnsafeBufferPointer<UInt16>) throws -> Result
  ) rethrows -> Result {
#if arch(i386) || arch(arm)
    unsupportedOn32bit()
#else
    var (transcoded, transcodedCount) = self.transcoded
    return try Swift.withUnsafeBytes(of: &transcoded.storage) {
      bufPtr -> Result in
      let ptr = bufPtr.baseAddress._unsafelyUnwrappedUnchecked
        .assumingMemoryBound(to: UInt16.self)
      return try body(UnsafeBufferPointer(start: ptr, count: transcodedCount))
    }
#endif
  }

  @inlinable
  @inline(__always)
  func withUnmanagedUTF16<Result>(
    _ body: (_UnmanagedString<UInt16>) throws -> Result
  ) rethrows -> Result {
#if arch(i386) || arch(arm)
    unsupportedOn32bit()
#else
    return try withTranscodedUTF16CodeUnits {
      return try body(_UnmanagedString($0))
    }
#endif
  }

  @inlinable
  @inline(__always)
  func withUnmanagedASCII<Result>(
    _ body: (_UnmanagedString<UInt8>) throws -> Result
  ) rethrows -> Result {
#if arch(i386) || arch(arm)
    unsupportedOn32bit()
#else
    _sanityCheck(self.isASCII)
    return try withUTF8CodeUnits {
      return try body(_UnmanagedString($0))
    }
#endif
  }
}
extension _SmallUTF8String {
  @inlinable
  public // @testable
  // FIXME:  internal(set)
  var count: Int {
    @inline(__always) get {
#if arch(i386) || arch(arm)
    unsupportedOn32bit()
#else
      return Int(bitPattern: UInt(self._uncheckedCodeUnit(at: 15)))
#endif
    }
    @inline(__always) set {
#if arch(i386) || arch(arm)
    unsupportedOn32bit()
#else
      _sanityCheck(newValue <= _SmallUTF8String.capacity, "out of bounds")
      self._uncheckedSetCodeUnit(
        at: 15, to: UInt8(truncatingIfNeeded: UInt(bitPattern: newValue)))
#endif
    }
  }

  @inlinable
  public // @testable
  var capacity: Int { @inline(__always) get { return 15 } }

  @inlinable
  public // @testable
  var unusedCapacity: Int { @inline(__always) get { return capacity - count } }

  @inlinable
  public // @testable
  var isASCII: Bool {
    @inline(__always) get {
#if arch(i386) || arch(arm)
      unsupportedOn32bit()
#else
      // TODO (TODO: JIRA): Consider using our last bit for this
      _sanityCheck(_uncheckedCodeUnit(at: 15) & 0xF0 == 0)

      let topBitMask: UInt = 0x8080_8080_8080_8080
      return (_storage.low | _storage.high) & topBitMask == 0
#endif
    }
  }

  @inlinable
  func _invariantCheck() {
#if arch(i386) || arch(arm)
    unsupportedOn32bit()
#else
    #if INTERNAL_CHECKS_ENABLED
    _sanityCheck(count <= _SmallUTF8String.capacity)
    _sanityCheck(self.isASCII, "UTF-8 currently unsupported")
    #endif // INTERNAL_CHECKS_ENABLED
#endif
  }

  internal
  func _dump() {
#if arch(i386) || arch(arm)
    unsupportedOn32bit()
#else
    #if INTERNAL_CHECKS_ENABLED
    print("""
      smallUTF8: count: \(self.count), codeUnits: \(
        self.map { String($0, radix: 16) }.dropLast()
      )
      """)
    #endif // INTERNAL_CHECKS_ENABLED
#endif
  }

  @inlinable // FIXME(sil-serialize-all)
  internal func _copy<TargetCodeUnit>(
    into target: UnsafeMutableBufferPointer<TargetCodeUnit>
  ) where TargetCodeUnit : FixedWidthInteger & UnsignedInteger {
#if arch(i386) || arch(arm)
    unsupportedOn32bit()
#else
    _sanityCheck(target.count >= self.count)
    guard count > 0 else { return }

    if _fastPath(TargetCodeUnit.bitWidth == 8) {
      _sanityCheck(TargetCodeUnit.self == UInt8.self)
      let target = _castBufPtr(target, to: UInt8.self)

      // TODO: Inspect generated code. Consider checking count for alignment so
      // we can just copy our UInts directly when possible.
      var ptr = target.baseAddress._unsafelyUnwrappedUnchecked
      for cu in self {
        ptr[0] = cu
        ptr += 1
      }
      return
    }

    _sanityCheck(TargetCodeUnit.self == UInt16.self)
    self.transcode(_uncheckedInto: _castBufPtr(target, to: UInt16.self))
#endif
  }
}
extension _SmallUTF8String: RandomAccessCollection {
  public // @testable
  typealias Index = Int
  public // @testable
  typealias Element = UInt8
  public // @testable
  typealias SubSequence = _SmallUTF8String

  @inlinable
  public // @testable
  var startIndex: Int { @inline(__always) get { return 0 } }

  @inlinable
  public // @testable
  var endIndex: Int { @inline(__always) get { return count } }

  @inlinable
  public // @testable
  subscript(_ idx: Int) -> UInt8 {
    @inline(__always) get {
#if arch(i386) || arch(arm)
      unsupportedOn32bit()
#else
      _sanityCheck(idx >= 0 && idx <= count)
      return _uncheckedCodeUnit(at: idx)
#endif
    }
    @inline(__always) set {
#if arch(i386) || arch(arm)
      unsupportedOn32bit()
#else
      _sanityCheck(idx >= 0 && idx <= count)
      _uncheckedSetCodeUnit(at: idx, to: newValue)
#endif
    }
  }

  @inlinable
  public // @testable
  subscript(_ bounds: Range<Index>) -> SubSequence {
    @inline(__always) get {
#if arch(i386) || arch(arm)
    unsupportedOn32bit()
#else
      _sanityCheck(bounds.lowerBound >= 0 && bounds.upperBound <= count)
      return self._uncheckedClamp(
        lowerBound: bounds.lowerBound, upperBound: bounds.upperBound)
#endif
    }
  }
}

extension _SmallUTF8String {
  @inlinable
  public // @testable
  func _repeated(_ n: Int) -> _SmallUTF8String? {
#if arch(i386) || arch(arm)
      unsupportedOn32bit()
#else
    _sanityCheck(n > 1)
    let finalCount = self.count * n
    guard finalCount <= 15 else { return nil }
    var ret = self
    for _ in 0..<(n &- 1) {
      ret = ret._appending(self)._unsafelyUnwrappedUnchecked
    }
    return ret
#endif
  }

  @inlinable
  public // @testable
  func _appending<C: RandomAccessCollection>(_ other: C) -> _SmallUTF8String?
  where C.Element == UInt8 {
#if arch(i386) || arch(arm)
      unsupportedOn32bit()
#else
    guard other.count <= self.unusedCapacity else { return nil }

    // TODO: as _copyContents
    var result = self
    result._withMutableExcessCapacityBytes { rawBufPtr in
      var i = 0
      for cu in other {
        rawBufPtr[i] = cu
        i += 1
      }
    }
    result.count = self.count &+ other.count
    return result
#endif
  }

  @inlinable
  func _appending<C: RandomAccessCollection>(_ other: C) -> _SmallUTF8String?
  where C.Element == UInt16 {
#if arch(i386) || arch(arm)
      unsupportedOn32bit()
#else
    guard other.count <= self.unusedCapacity else { return nil }

    // TODO: as _copyContents
    var result = self
    let success = result._withMutableExcessCapacityBytes { rawBufPtr -> Bool in
      var i = 0
      for cu in other {
        guard cu <= 0x7F else {
          // TODO: transcode and communicate count back
          return false
        }
        rawBufPtr[i] = UInt8(truncatingIfNeeded: cu)
        i += 1
      }
      return true
    }
    guard success else { return nil }

    result.count = self.count &+ other.count
    return result
#endif
  }

  // NOTE: This exists to facilitate _fromCodeUnits, which is awful for this use
  // case. Please don't call this from anywhere else.
  @usableFromInline
  @inline(never) // @outlined
  // @_specialize(where Encoding == UTF16)
  // @_specialize(where Encoding == UTF8)
  init?<S: Sequence, Encoding: Unicode.Encoding>(
    _fromCodeUnits codeUnits: S,
    utf16Length: Int,
    isASCII: Bool,
    _: Encoding.Type = Encoding.self
  ) where S.Element == Encoding.CodeUnit {
#if arch(i386) || arch(arm)
    return nil // Never form small strings on 32-bit
#else
    guard utf16Length <= 15 else { return nil }

    // TODO: transcode
    guard isASCII else { return nil }

    self.init()
    var bufferIdx = 0
    for encodedScalar in Unicode._ParsingIterator(
      codeUnits: codeUnits.makeIterator(),
      parser: Encoding.ForwardParser()
    ) {
      guard let transcoded = Unicode.UTF8.transcode(
        encodedScalar, from: Encoding.self
      ) else {
        fatalError("Somehow un-transcodable?")
      }
      _sanityCheck(transcoded.count <= 4, "how?")
      guard bufferIdx + transcoded.count <= 15 else { return nil }
      for i in transcoded.indices {
        self._uncheckedSetCodeUnit(at: bufferIdx, to: transcoded[i])
        bufferIdx += 1
      }
    }
    _sanityCheck(self.count == 0, "overwrote count early?")
    self.count = bufferIdx

    // FIXME: support transcoding
    if !self.isASCII { return nil }

    _invariantCheck()
#endif
  }
}

extension _SmallUTF8String {
#if arch(i386) || arch(arm)
  @_fixed_layout @usableFromInline struct UnicodeScalarIterator {
    @inlinable @inline(__always)
    func next() -> Unicode.Scalar? { unsupportedOn32bit() }
  }
  @inlinable @inline(__always)
  func makeUnicodeScalarIterator() -> UnicodeScalarIterator {
    unsupportedOn32bit()
  }
#else
  // FIXME (TODO: JIRA): Just make a real decoding iterator
  @_fixed_layout
  @usableFromInline // FIXME(sil-serialize-all)
  struct UnicodeScalarIterator {
    @usableFromInline // FIXME(sil-serialize-all)
    var buffer: _SmallUTF16StringBuffer
    @usableFromInline // FIXME(sil-serialize-all)
    var count: Int
    @usableFromInline // FIXME(sil-serialize-all)
    var _offset: Int

    @inlinable // FIXME(sil-serialize-all)
    init(_ base: _SmallUTF8String) {
      (self.buffer, self.count) = base.transcoded
      self._offset = 0
    }

    @inlinable // FIXME(sil-serialize-all)
    mutating func next() -> Unicode.Scalar? {
      if _slowPath(_offset == count) { return nil }
      let u0 = buffer[_offset]
      if _fastPath(UTF16._isScalar(u0)) {
        _offset += 1
        return Unicode.Scalar(u0)
      }
      if UTF16.isLeadSurrogate(u0) && _offset + 1 < count {
        let u1 = buffer[_offset + 1]
        if UTF16.isTrailSurrogate(u1) {
          _offset += 2
          return UTF16._decodeSurrogates(u0, u1)
        }
      }
      _offset += 1
      return Unicode.Scalar._replacementCharacter
    }
  }

  @inlinable
  func makeUnicodeScalarIterator() -> UnicodeScalarIterator {
    return UnicodeScalarIterator(self)
  }
#endif // 64-bit
}

#if arch(i386) || arch(arm)
#else
extension _SmallUTF8String {
  @inlinable
  @inline(__always)
  init(_rawBits: _RawBitPattern) {
    self._storage.low = _rawBits.low
    self._storage.high = _rawBits.high
    _invariantCheck()
  }

  @inlinable
  @inline(__always)
  init(low: UInt, high: UInt, count: Int) {
    self.init()
    self._storage.low = low
    self._storage.high = high
    self.count = count
    _invariantCheck()
  }

  @inlinable
  internal var _rawBits: _RawBitPattern {
    @inline(__always) get { return _storage }
  }

  @inlinable
  internal var lowUnpackedBits: UInt {
    @inline(__always) get { return _storage.low }
  }
  @inlinable
  internal var highUnpackedBits: UInt {
    @inline(__always) get { return _storage.high & 0x00FF_FFFF_FFFF_FFFF }
  }

  @inlinable
  internal var unpackedBits: (low: UInt, high: UInt, count: Int) {
    @inline(__always)
    get { return (lowUnpackedBits, highUnpackedBits, count) }
  }
}
extension _SmallUTF8String {
  // Operate with a pointer to the entire struct, including unused capacity
  // and inline count. You should almost never call this directly.
  @inlinable
  @inline(__always)
  mutating func _withAllUnsafeMutableBytes<Result>(
    _ body: (UnsafeMutableRawBufferPointer) throws -> Result
  ) rethrows -> Result {
    var copy = self
    defer { self = copy }
    return try Swift.withUnsafeMutableBytes(of: &copy._storage) { try body($0) }
  }
  @inlinable
  @inline(__always)
  func _withAllUnsafeBytes<Result>(
    _ body: (UnsafeRawBufferPointer) throws -> Result
  ) rethrows -> Result {
    var copy = self
    return try Swift.withUnsafeBytes(of: &copy._storage) { try body($0) }
  }
  @inlinable
  @inline(__always)
  mutating func _withMutableExcessCapacityBytes<Result>(
    _ body: (UnsafeMutableRawBufferPointer) throws -> Result
  ) rethrows -> Result {
    let unusedCapacity = self.unusedCapacity
    let count = self.count
    return try self._withAllUnsafeMutableBytes { allBufPtr in
      let ptr = allBufPtr.baseAddress._unsafelyUnwrappedUnchecked + count
      return try body(
        UnsafeMutableRawBufferPointer(start: ptr, count: unusedCapacity))
    }
  }

}
extension _SmallUTF8String {
  @inlinable
  @inline(__always)
  func _uncheckedCodeUnit(at i: Int) -> UInt8 {
    _sanityCheck(i >= 0 && i <= 15)
    if i < 8 {
      return _storage.low._uncheckedGetByte(at: i)
    } else {
      return _storage.high._uncheckedGetByte(at: i &- 8)
    }
  }
  @inlinable
  @inline(__always)
  mutating func _uncheckedSetCodeUnit(at i: Int, to: UInt8) {
    // TODO(TODO: JIRA): in-register operation instead
    self._withAllUnsafeMutableBytes { $0[i] = to }
  }
}

extension _SmallUTF8String {
  @inlinable
  @inline(__always)
  internal func _uncheckedClamp(upperBound: Int) -> _SmallUTF8String {
    _sanityCheck(upperBound <= self.count)
    guard upperBound >= 8 else {
      var low = self.lowUnpackedBits
      let shift = upperBound &* 8
      let mask: UInt = (1 &<< shift) &- 1
      low &= mask
      return _SmallUTF8String(low: low, high: 0, count: upperBound)
    }
    let shift = (upperBound &- 8) &* 8
    _sanityCheck(shift % 8 == 0)

    var high = self.highUnpackedBits
    high &= (1 &<< shift) &- 1
    return _SmallUTF8String(
      low: self.lowUnpackedBits, high: high, count: upperBound)
  }

  @inlinable
  @inline(__always)
  internal func _uncheckedClamp(lowerBound: Int) -> _SmallUTF8String {
    _sanityCheck(lowerBound < self.count)
    let low: UInt
    let high: UInt
    if lowerBound < 8 {
      let shift: UInt = UInt(bitPattern: lowerBound) &* 8
      let newLowHigh: UInt = self.highUnpackedBits & ((1 &<< shift) &- 1)
      low = (self.lowUnpackedBits &>> shift) | (newLowHigh &<< (64 &- shift))
      high = self.highUnpackedBits &>> shift
    } else {
      high = 0
      low = self.highUnpackedBits &>> ((lowerBound &- 8) &* 8)
    }

    return _SmallUTF8String(
      low: low, high: high, count: self.count &- lowerBound)
  }

  @inlinable
  @inline(__always)
  internal func _uncheckedClamp(
    lowerBound: Int, upperBound: Int
  ) -> _SmallUTF8String {
    // TODO: More efficient to skip the intermediary shifts and just mask up
    // front.
    _sanityCheck(upperBound >= lowerBound)
    if lowerBound == upperBound { return _SmallUTF8String() }
    let dropTop = self._uncheckedClamp(upperBound: upperBound)
    return dropTop._uncheckedClamp(lowerBound: lowerBound)
  }
}

extension _SmallUTF8String {//}: _StringVariant {
  typealias TranscodedBuffer = _SmallUTF16StringBuffer

  @inlinable
  @discardableResult
  func transcode(
    _uncheckedInto buffer: UnsafeMutableBufferPointer<UInt16>
  ) -> Int {
      if _fastPath(isASCII) {
        _sanityCheck(buffer.count >= self.count)
        var bufferIdx = 0
        for cu in self {
            buffer[bufferIdx] = UInt16(cu)
            bufferIdx += 1
        }
        return bufferIdx
    }

    let length = _transcodeNonASCII(_uncheckedInto: buffer)
    _sanityCheck(length <= buffer.count) // TODO: assert ahead-of-time

    return length
  }

  @inlinable
  @inline(__always)
  func transcode(into buffer: UnsafeMutablePointer<TranscodedBuffer>) -> Int {
    let ptr = UnsafeMutableRawPointer(buffer).assumingMemoryBound(
      to: UInt16.self)

    return transcode(
      _uncheckedInto: UnsafeMutableBufferPointer(start: ptr, count: count))
  }

  @inlinable
  var transcoded: (TranscodedBuffer, count: Int) {
    @inline(__always) get {
      // TODO: in-register zero-extension for ascii
      var buffer = TranscodedBuffer(allZeros:())
      let count = transcode(into: &buffer)
      return (buffer, count: count)
    }
  }

  @usableFromInline
  @inline(never) // @outlined
  func _transcodeNonASCII(
    _uncheckedInto buffer: UnsafeMutableBufferPointer<UInt16>
  ) -> Int {
    _sanityCheck(!isASCII)

    // TODO(TODO: JIRA): Just implement this directly

    var bufferIdx = 0
    for encodedScalar in Unicode._ParsingIterator(
      codeUnits: self.makeIterator(),
      parser: Unicode.UTF8.ForwardParser()
    ) {
      guard let transcoded = Unicode.UTF16.transcode(
        encodedScalar, from: Unicode.UTF8.self
      ) else {
        fatalError("Somehow un-transcodable?")
      }
      switch transcoded.count {
      case 1:
        buffer[bufferIdx] = transcoded.first!
        bufferIdx += 1
      case 2:
        buffer[bufferIdx] = transcoded.first!
        buffer[bufferIdx+1] = transcoded.dropFirst().first!
        bufferIdx += 2
      case _: fatalError("Somehow, not transcoded or more than 2?")
      }
    }

    _sanityCheck(bufferIdx <= buffer.count) // TODO: assert earlier
    return bufferIdx
  }
}

@inlinable
@inline(__always)
internal
func _castBufPtr<A, B>(
  _ bufPtr: UnsafeMutableBufferPointer<A>, to: B.Type = B.self
) -> UnsafeMutableBufferPointer<B> {
  let numBytes = bufPtr.count &* MemoryLayout<A>.stride
  _sanityCheck(numBytes % MemoryLayout<B>.stride == 0)

  let ptr = UnsafeMutableRawPointer(
    bufPtr.baseAddress._unsafelyUnwrappedUnchecked
  ).assumingMemoryBound(to: B.self)
  let count = numBytes / MemoryLayout<B>.stride
  return UnsafeMutableBufferPointer(start: ptr, count: count)
}

#endif // 64-bit

extension UInt {
  // Fetches the `i`th byte, from least-significant to most-significant
  //
  // TODO: endianess awareness day
  @inlinable
  @inline(__always)
  func _uncheckedGetByte(at i: Int) -> UInt8 {
    _sanityCheck(i >= 0 && i < MemoryLayout<UInt>.stride)
    let shift = UInt(bitPattern: i) &* 8
    return UInt8(truncatingIfNeeded: (self &>> shift))
  }
}

