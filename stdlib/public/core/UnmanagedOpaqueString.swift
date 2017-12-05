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

@_versioned
@_fixed_layout
internal struct _UnmanagedOpaqueString {
  @_versioned
  unowned(unsafe) let object: _CocoaString

  @_versioned
  let range: Range<Int>

  @_versioned
  let isSlice: Bool

  @_inlineable
  @_versioned
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

  @_inlineable
  @_versioned
  init(_ object: _CocoaString, count: Int) {
    self.init(object, range: 0..<count, isSlice: false)
  }
}

extension _UnmanagedOpaqueString : Sequence {
  typealias Element = UTF16.CodeUnit

  @_inlineable
  @_versioned
  func makeIterator() -> Iterator {
    return Iterator(self, startingAt: range.lowerBound)
  }

  @_inlineable // FIXME(sil-serialize-all)
  @_versioned // FIXME(sil-serialize-all)
  internal func makeIterator(startingAt position: Int) -> Iterator {
    return Iterator(self, startingAt: position)
  }

  @_versioned
  @_fixed_layout
  struct Iterator : IteratorProtocol {
    @_versioned
    internal let _object: _CocoaString

    @_versioned
    internal let _endIndex: Int

    @_versioned
    internal var _nextIndex: Int

    @_versioned
    internal var _buffer = _FixedArray16<Element>(allZeros: ())

    @_versioned
    internal var _bufferIndex: Int8 = 0

    @_versioned
    internal var _bufferCount: Int8 = 0

    @_inlineable
    @_versioned
    init(_ string: _UnmanagedOpaqueString, startingAt start: Int) {
      self._object = string.object
      self._endIndex = string.range.upperBound
      self._nextIndex = start
    }

    @_inlineable
    @_versioned
    @inline(__always)
    mutating func next() -> Element? {
      if _fastPath(_bufferIndex < _bufferCount) {
        let result = _buffer[Int(_bufferIndex)]
        _bufferIndex += 1
        return result
      }
      if _slowPath(_nextIndex == _endIndex) { return nil }
      return _nextOnSlowPath()
    }

    @_inlineable
    @_versioned
    mutating func _nextOnSlowPath() -> Element {
      // Fill buffer
      _sanityCheck(Element.self == UTF16.CodeUnit.self)
      _sanityCheck(_nextIndex < _endIndex)
      let capacity = _buffer.count
      let end = Swift.min(_nextIndex + capacity, _endIndex)
      unowned(unsafe) let object = _object
      withUnsafeMutableBytes(of: &_buffer.storage) { b in
        _sanityCheck(b.count == MemoryLayout<Element>.stride * capacity)
        _cocoaStringCopyCharacters(
          from: object,
          range: _nextIndex..<end,
          into: b.baseAddress!.assumingMemoryBound(to: UTF16.CodeUnit.self))
      }
      _bufferIndex = 1
      _bufferCount = Int8(end - _nextIndex)
      _nextIndex = end
      _fixLifetime(_object)
      return _buffer[0]
    }
  }
}

extension _UnmanagedOpaqueString : RandomAccessCollection {
  internal typealias IndexDistance = Int
  internal typealias Indices = CountableRange<Index>
  internal typealias SubSequence = _UnmanagedOpaqueString

  @_fixed_layout
  @_versioned
  struct Index : Strideable {
    @_versioned
    internal var _value: Int

    @_versioned
    @_inlineable
    @inline(__always)
    init(_ value: Int) {
      self._value = value
    }

    @_versioned
    @_inlineable
    @inline(__always)
    func distance(to other: Index) -> Int {
      return other._value - self._value
    }

    @_versioned
    @_inlineable
    @inline(__always)
    func advanced(by n: Int) -> Index {
      return Index(_value + n)
    }
  }

  @_versioned
  @_inlineable
  var startIndex: Index {
    return Index(range.lowerBound)
  }

  @_versioned
  @_inlineable
  var endIndex: Index {
    return Index(range.upperBound)
  }

  @_versioned
  @_inlineable // FIXME(sil-serialize-all)
  subscript(position: Index) -> UTF16.CodeUnit {
    _sanityCheck(position._value >= range.lowerBound)
    _sanityCheck(position._value < range.upperBound)
    return _cocoaStringSubscript(object, position._value)
  }

  @_versioned
  @_inlineable // FIXME(sil-serialize-all)
  subscript(bounds: Range<Index>) -> _UnmanagedOpaqueString {
    _sanityCheck(bounds.lowerBound._value >= range.lowerBound)
    _sanityCheck(bounds.upperBound._value <= range.upperBound)
    let b: Range<Int> = bounds.lowerBound._value ..< bounds.upperBound._value
    let newSlice = self.isSlice || b.count != range.count
    return _UnmanagedOpaqueString(object, range: b, isSlice: newSlice)
  }
}

extension _UnmanagedOpaqueString {
  @_versioned
  @inline(never)
  internal func cocoaSlice() -> _CocoaString {
    guard isSlice else { return object }
    // FIXME: This usually copies storage; maybe add an NSString subclass
    // for opaque slices?
    return _cocoaStringSlice(object, range)
  }

  @_versioned
  @_inlineable // FIXME(sil-serialize-all)
  subscript(offset: Int) -> UTF16.CodeUnit {
    _sanityCheck(offset >= 0 && offset < count)
    return _cocoaStringSubscript(object, range.lowerBound + offset)
  }

  @_versioned
  @_inlineable // FIXME(sil-serialize-all)
  subscript(offsetRange: Range<Int>) -> _UnmanagedOpaqueString {
    _sanityCheck(offsetRange.lowerBound >= 0)
    _sanityCheck(offsetRange.upperBound <= range.count)
    let b: Range<Int> =
      range.lowerBound + offsetRange.lowerBound ..<
      range.lowerBound + offsetRange.upperBound
    let newSlice = self.isSlice || b.count != range.count
    return _UnmanagedOpaqueString(object, range: b, isSlice: newSlice)
  }

  @_inlineable // FIXME(sil-serialize-all)
  @_versioned // FIXME(sil-serialize-all)
  internal func _copy(
    into dest: UnsafeMutableBufferPointer<UTF16.CodeUnit>
  ) {
    _sanityCheck(dest.count >= range.count)
    guard range.count > 0 else { return }
    _cocoaStringCopyCharacters(
      from: object,
      range: range,
      into: dest.baseAddress!)
  }

  @_inlineable // FIXME(sil-serialize-all)
  @_versioned // FIXME(sil-serialize-all)
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
    _cocoaStringCopyCharacters(
      from: object,
      range: range,
      into: d.assumingMemoryBound(to: UTF16.CodeUnit.self))
  }
}

extension _UnmanagedOpaqueString {
  @_versioned // FIXME(sil-serialize-all)
  @inline(never)
  func _unicodeScalarWidth(startingAt index: Int) -> Int {
    if _slowPath(UTF16.isLeadSurrogate(self[index])) {
      if index + 1 < self.count &&
      UTF16.isTrailSurrogate(self[index + 1]) {
        return 2
      }
    }
    return 1
  }

  @_versioned // FIXME(sil-serialize-all)
  @inline(never)
  func _unicodeScalarWidth(endingAt offset: Int) -> Int {
    _sanityCheck(offset >= 0 && offset < count)
    if _slowPath(UTF16.isTrailSurrogate(self[offset])) {
      if offset >= 1 && UTF16.isLeadSurrogate(self[offset - 1]) {
        return 2
      }
    }
    return 1
  }

  @_versioned // FIXME(sil-serialize-all)
  @inline(never)
  func _decodeUnicodeScalar(startingAt offset: Int) -> UnicodeDecodingResult {
    let u0 = self[offset]
    if _fastPath(UTF16._isScalar(u0)) {
      return .scalarValue(Unicode.Scalar(_unchecked: UInt32(u0)))
    }
    if UTF16.isLeadSurrogate(u0) && offset + 1 < count {
      let u1 = self[offset + 1]
      if UTF16.isTrailSurrogate(u1) {
        return .scalarValue(UTF16._decodeSurrogates(u0, u1))
      }
    }
    return .error
  }

  @_versioned // FIXME(sil-serialize-all)
  internal struct UnicodeScalarIterator : IteratorProtocol {
    var _base: _UnmanagedOpaqueString.Iterator
    var _peek: UTF16.CodeUnit?

    @_versioned // FIXME(sil-serialize-all)
    init(_ base: _UnmanagedOpaqueString) {
      self._base = base.makeIterator()
      self._peek = _base.next()
    }

    @_versioned // FIXME(sil-serialize-all)
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

  @_versioned // FIXME(sil-serialize-all)
  @inline(never)
  func makeUnicodeScalarIterator() -> UnicodeScalarIterator {
    return UnicodeScalarIterator(self)
  }
}
