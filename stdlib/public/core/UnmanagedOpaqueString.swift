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
  internal typealias Index = Int
  internal typealias IndexDistance = Int
  internal typealias Indices = CountableRange<Int>
  internal typealias SubSequence = _UnmanagedOpaqueString

  @_versioned
  @_inlineable
  var startIndex: Int {
    return range.lowerBound
  }

  @_versioned
  @_inlineable
  var endIndex: Int {
    return range.upperBound
  }

  @_versioned
  @_inlineable // FIXME(sil-serialize-all)
  subscript(position: Int) -> UTF16.CodeUnit {
    return _cocoaStringSubscript(object, range.lowerBound + position)
  }

  @_versioned
  @_inlineable // FIXME(sil-serialize-all)
  subscript(bounds: Range<Int>) -> _UnmanagedOpaqueString {
    _sanityCheck(bounds.lowerBound >= range.lowerBound)
    _sanityCheck(bounds.upperBound <= range.upperBound)
    let newSlice = self.isSlice || bounds != range
    return _UnmanagedOpaqueString(object, range: bounds, isSlice: newSlice)
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
