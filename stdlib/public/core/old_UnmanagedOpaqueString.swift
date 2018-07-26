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

public protocol _OpaqueString: class {
  var length: Int { get }
  func character(at index: Int) -> UInt16

  // FIXME: This is not an NSString method; I'd like to use
  // `getCharacters(_:,range:)`, but it would be weird to define
  // `_SwiftNSRange` without an Objective-C runtime.
  func copyCodeUnits(
    from range: Range<Int>,
    into dest: UnsafeMutablePointer<UInt16>)
}

@usableFromInline
@_fixed_layout
internal struct _UnmanagedOpaqueString {
#if _runtime(_ObjC) // FIXME unify
  @usableFromInline
  unowned(unsafe) let object: _CocoaString
#else
  @usableFromInline
  unowned(unsafe) let object: _OpaqueString
#endif

  @usableFromInline
  let range: Range<Int>

  @usableFromInline
  let isSlice: Bool

#if _runtime(_ObjC) // FIXME unify
  @inlinable
  init(_ object: _CocoaString, range: Range<Int>, isSlice: Bool) {
    self.object = object
    self.range = range
    self.isSlice = isSlice
  }

  @inline(never)
  init(_ object: _CocoaString) {
    let count = _stdlib_binary_CFStringGetLength(object)
    self.init(object, count: count)
  }

  @inlinable
  init(_ object: _CocoaString, count: Int) {
    self.init(object, range: 0..<count, isSlice: false)
  }
#else
  @inlinable
  init(_ object: _OpaqueString, range: Range<Int>, isSlice: Bool) {
    self.object = object
    self.range = range
    self.isSlice = isSlice
  }

  @inline(never)
  init(_ object: _OpaqueString) {
    self.init(object, count: object.length)
  }

  @inlinable
  init(_ object: _OpaqueString, count: Int) {
    self.init(object, range: 0..<count, isSlice: false)
  }
#endif
}

extension _UnmanagedOpaqueString : Sequence {
  typealias Element = UTF16.CodeUnit

  @inlinable
  func makeIterator() -> Iterator {
    return Iterator(self, startingAt: range.lowerBound)
  }

  @inlinable // FIXME(sil-serialize-all)
  internal func makeIterator(startingAt position: Int) -> Iterator {
    return Iterator(self, startingAt: position)
  }

  @usableFromInline
  @_fixed_layout
  struct Iterator : IteratorProtocol {
    @usableFromInline
    internal typealias Element = UTF16.CodeUnit

#if _runtime(_ObjC) // FIXME unify
    @usableFromInline
    internal let _object: _CocoaString
#else
    @usableFromInline
    internal let _object: _OpaqueString
#endif

    @usableFromInline
    internal var _range: Range<Int>

    @usableFromInline
    internal var _buffer = _FixedArray16<Element>()

    @usableFromInline
    internal var _bufferIndex: Int8 = 0

    @inlinable
    init(_ string: _UnmanagedOpaqueString, startingAt start: Int) {
      self._object = string.object
      self._range = start..<string.range.upperBound
    }

    @inlinable
    @inline(__always)
    mutating func next() -> Element? {
      if _fastPath(_bufferIndex < _buffer.count) {
        let result = _buffer[Int(_bufferIndex)]
        _bufferIndex += 1
        return result
      }
      if _slowPath(_range.isEmpty) { return nil }
      return _nextOnSlowPath()
    }

    @usableFromInline
    @inline(never)
    mutating func _nextOnSlowPath() -> Element {
      // Fill buffer
      _sanityCheck(!_range.isEmpty)
      let end = Swift.min(
        _range.lowerBound + _buffer.capacity,
        _range.upperBound)
      let r: Range<Int> = _range.lowerBound..<end
      let opaque = _UnmanagedOpaqueString(_object, range: r, isSlice: true)
      _buffer.count = r.count
      _buffer.withUnsafeMutableBufferPointer { b in
        _sanityCheck(b.count == r.count)
        opaque._copy(into: b)
      }
      _bufferIndex = 1
      _range = r.upperBound ..< _range.upperBound
      _fixLifetime(_object)
      return _buffer[0]
    }
  }
}

extension _UnmanagedOpaqueString : RandomAccessCollection {
  internal typealias IndexDistance = Int
  internal typealias Indices = Range<Index>

  @usableFromInline // FIXME(sil-serialize-all)
  internal typealias SubSequence = _UnmanagedOpaqueString

  @_fixed_layout
  @usableFromInline
  struct Index : Strideable {
    @usableFromInline
    internal var _value: Int

    @inlinable
    @inline(__always)
    init(_ value: Int) {
      self._value = value
    }

    @inlinable
    @inline(__always)
    func distance(to other: Index) -> Int {
      return other._value - self._value
    }

    @inlinable
    @inline(__always)
    func advanced(by n: Int) -> Index {
      return Index(_value + n)
    }
  }

  @inlinable
  var startIndex: Index {
    return Index(range.lowerBound)
  }

  @inlinable
  var endIndex: Index {
    return Index(range.upperBound)
  }

  @inlinable
  var count: Int {
    return range.count
  }

  @inlinable // FIXME(sil-serialize-all)
  subscript(position: Index) -> UTF16.CodeUnit {
    _sanityCheck(position._value >= range.lowerBound)
    _sanityCheck(position._value < range.upperBound)
#if _runtime(_ObjC) // FIXME unify
    return _cocoaStringSubscript(object, position._value)
#else
    return object.character(at: position._value)
#endif
  }

  @inlinable // FIXME(sil-serialize-all)
  subscript(bounds: Range<Index>) -> _UnmanagedOpaqueString {
    _sanityCheck(bounds.lowerBound._value >= range.lowerBound)
    _sanityCheck(bounds.upperBound._value <= range.upperBound)
    let b: Range<Int> = bounds.lowerBound._value ..< bounds.upperBound._value
    let newSlice = self.isSlice || b.count != range.count
    return _UnmanagedOpaqueString(object, range: b, isSlice: newSlice)
  }
}

extension _UnmanagedOpaqueString : _StringVariant {
  @usableFromInline internal typealias Encoding = Unicode.UTF16
  @usableFromInline internal typealias CodeUnit = Encoding.CodeUnit

  @inlinable
  var isASCII: Bool {
    @inline(__always) get { return false }
  }

  @inlinable
  @inline(__always)
  func _boundsCheck(_ i: Index) {
    _precondition(i._value >= range.lowerBound && i._value < range.upperBound,
      "String index is out of bounds")
  }

  @inlinable
  @inline(__always)
  func _boundsCheck(_ range: Range<Index>) {
    _precondition(
      range.lowerBound._value >= self.range.lowerBound &&
      range.upperBound._value <= self.range.upperBound,
      "String index range is out of bounds")
  }

  @inlinable
  @inline(__always)
  func _boundsCheck(offset: Int) {
    _precondition(offset >= 0 && offset < range.count,
      "String index is out of bounds")
  }

  @inlinable
  @inline(__always)
  func _boundsCheck(offsetRange range: Range<Int>) {
    _precondition(range.lowerBound >= 0 && range.upperBound <= count,
      "String index range is out of bounds")
  }

  @inlinable // FIXME(sil-serialize-all)
  subscript(offset: Int) -> UTF16.CodeUnit {
    _sanityCheck(offset >= 0 && offset < count)
#if _runtime(_ObjC) // FIXME unify
    return _cocoaStringSubscript(object, range.lowerBound + offset)
#else
    return object.character(at: range.lowerBound + offset)
#endif
  }

  @inlinable // FIXME(sil-serialize-all)
  subscript(offsetRange: Range<Int>) -> _UnmanagedOpaqueString {
    _sanityCheck(offsetRange.lowerBound >= 0)
    _sanityCheck(offsetRange.upperBound <= range.count)
    let b: Range<Int> =
      range.lowerBound + offsetRange.lowerBound ..<
      range.lowerBound + offsetRange.upperBound
    let newSlice = self.isSlice || b.count != range.count
    return _UnmanagedOpaqueString(object, range: b, isSlice: newSlice)
  }

  @inlinable // FIXME(sil-serialize-all)
  internal subscript(offsetRange: PartialRangeUpTo<Int>) -> SubSequence {
    _sanityCheck(offsetRange.upperBound <= range.count)
    let b: Range<Int> =
      range.lowerBound ..<
      range.lowerBound + offsetRange.upperBound
    let newSlice = self.isSlice || b.count != range.count
    return _UnmanagedOpaqueString(object, range: b, isSlice: newSlice)
  }

  @inlinable // FIXME(sil-serialize-all)
  internal subscript(offsetRange: PartialRangeThrough<Int>) -> SubSequence {
    _sanityCheck(offsetRange.upperBound <= range.count)
    let b: Range<Int> =
      range.lowerBound ..<
      range.lowerBound + offsetRange.upperBound + 1
    let newSlice = self.isSlice || b.count != range.count
    return _UnmanagedOpaqueString(object, range: b, isSlice: newSlice)
  }

  @inlinable // FIXME(sil-serialize-all)
  internal subscript(offsetRange: PartialRangeFrom<Int>) -> SubSequence {
    _sanityCheck(offsetRange.lowerBound < range.count)
    let b: Range<Int> =
      range.lowerBound + offsetRange.lowerBound ..<
      range.upperBound
    let newSlice = self.isSlice || b.count != range.count
    return _UnmanagedOpaqueString(object, range: b, isSlice: newSlice)
  }

  @inlinable // FIXME(sil-serialize-all)
  internal func _copy(
    into dest: UnsafeMutableBufferPointer<UTF16.CodeUnit>
  ) {
    _sanityCheck(dest.count >= range.count)
    guard range.count > 0 else { return }
#if _runtime(_ObjC) // FIXME unify
    _cocoaStringCopyCharacters(
      from: object,
      range: range,
      into: dest.baseAddress!)
#else
    object.copyCodeUnits(from: range, into: dest.baseAddress!)
#endif
  }

  @inlinable // FIXME(sil-serialize-all)
  internal func _copy<TargetCodeUnit>(
    into dest: UnsafeMutableBufferPointer<TargetCodeUnit>
  )
  where TargetCodeUnit : FixedWidthInteger & UnsignedInteger {
    guard TargetCodeUnit.bitWidth == 16 else {
      _sanityCheckFailure("Narrowing copy from opaque strings is not implemented")
    }
    _sanityCheck(dest.count >= range.count)
    guard range.count > 0 else { return }
    let d = UnsafeMutableRawPointer(dest.baseAddress!)
      .assumingMemoryBound(to: UTF16.CodeUnit.self)
#if _runtime(_ObjC) // FIXME unify
    _cocoaStringCopyCharacters(from: object, range: range, into: d)
#else
    object.copyCodeUnits(from: range, into: d)
#endif
  }

  @usableFromInline // FIXME(sil-serialize-all)
  @_fixed_layout // FIXME(resilience)
  internal struct UnicodeScalarIterator : IteratorProtocol {
    var _base: _UnmanagedOpaqueString.Iterator
    var _peek: UTF16.CodeUnit?

    @usableFromInline // FIXME(sil-serialize-all)
    init(_ base: _UnmanagedOpaqueString) {
      self._base = base.makeIterator()
      self._peek = _base.next()
    }

    @usableFromInline // FIXME(sil-serialize-all)
    mutating func next() -> Unicode.Scalar? {
      if _slowPath(_peek == nil) { return nil }
      let u0 = _peek._unsafelyUnwrappedUnchecked
      _peek = _base.next()
      if _fastPath(UTF16._isScalar(u0)) {
        return Unicode.Scalar(_unchecked: UInt32(u0))
      }
      if UTF16.isLeadSurrogate(u0) && _peek != nil {
        let u1 = _peek._unsafelyUnwrappedUnchecked
        if UTF16.isTrailSurrogate(u1) {
          _peek = _base.next()
          return UTF16._decodeSurrogates(u0, u1)
        }
      }
      return Unicode.Scalar._replacementCharacter
    }
  }

  @usableFromInline // FIXME(sil-serialize-all)
  @inline(never)
  func makeUnicodeScalarIterator() -> UnicodeScalarIterator {
    return UnicodeScalarIterator(self)
  }
}

#if _runtime(_ObjC)
extension _UnmanagedOpaqueString {
  @usableFromInline
  @inline(never)
  internal func cocoaSlice() -> _CocoaString {
    guard isSlice else { return object }
    // FIXME: This usually copies storage; maybe add an NSString subclass
    // for opaque slices?
    return _cocoaStringSlice(object, range)
  }
}
#endif
