//===--- StringUTF8.swift - A UTF8 view of _StringCore --------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  _StringCore currently has three representations: Native ASCII,
//  Native UTF-16, and Opaque Cocoa.  Expose each of these as UTF-8 in a
//  way that will hopefully be efficient to traverse
//
//===----------------------------------------------------------------------===//

// FIXME(ABI): The UTF-8 string view should conform to
// `BidirectionalCollection`.

// FIXME(ABI): The UTF-8 string view should have a custom iterator type to
// allow performance optimizations of linear traversals.

extension _StringCore {
  /// An integral type that holds a sequence of UTF-8 code units, starting in
  /// its low byte.
  internal typealias _UTF8Chunk = UInt64

  /// Encode text starting at `i` as UTF-8.  Returns a pair whose first
  /// element is the index of the text following whatever got encoded,
  /// and the second element contains the encoded UTF-8 starting in its
  /// low byte.  Any unused high bytes in the result will be set to
  /// 0xFF.
  @warn_unused_result
  func _encodeSomeUTF8(from i: Int) -> (Int, _UTF8Chunk) {
    _sanityCheck(i <= count)

    if _fastPath(elementWidth == 1) {
      // How many UTF-16 code units might we use before we've filled up
      // our _UTF8Chunk with UTF-8 code units?
      let utf16Count = Swift.min(sizeof(_UTF8Chunk.self), count - i)

      var result: _UTF8Chunk = ~0 // Start with all bits set

      _memcpy(
        dest: UnsafeMutablePointer(Builtin.addressof(&result)),
        src: UnsafeMutablePointer(startASCII + i),
        size: numericCast(utf16Count))

      return (i + utf16Count, result)
    } else if _fastPath(_baseAddress != nil) {
      return _encodeSomeContiguousUTF16AsUTF8(from: i)
    } else {
#if _runtime(_ObjC)
      return _encodeSomeNonContiguousUTF16AsUTF8(from: i)
#else
      _sanityCheckFailure("_encodeSomeUTF8: Unexpected cocoa string")
#endif
    }
  }

  /// Helper for `_encodeSomeUTF8`, above.  Handles the case where the
  /// storage is contiguous UTF-16.
  @warn_unused_result
  func _encodeSomeContiguousUTF16AsUTF8(from i: Int) -> (Int, _UTF8Chunk) {
    _sanityCheck(elementWidth == 2)
    _sanityCheck(_baseAddress != nil)

    let storage = UnsafeBufferPointer(start: startUTF16, count: self.count)
    return _transcodeSomeUTF16AsUTF8(storage, i)
  }

#if _runtime(_ObjC)
  /// Helper for `_encodeSomeUTF8`, above.  Handles the case where the
  /// storage is non-contiguous UTF-16.
  @warn_unused_result
  func _encodeSomeNonContiguousUTF16AsUTF8(from i: Int) -> (Int, _UTF8Chunk) {
    _sanityCheck(elementWidth == 2)
    _sanityCheck(_baseAddress == nil)

    let storage = _CollectionOf<Int, UInt16>(
      _startIndex: 0, endIndex: self.count
    ) {
      (i: Int) -> UInt16 in
      return _cocoaStringSubscript(self, i)
    }
    return _transcodeSomeUTF16AsUTF8(storage, i)
  }
#endif
}

extension String {
  /// A collection of UTF-8 code units that encodes a `String` value.
  public struct UTF8View
    : Collection, 
      CustomStringConvertible, 
      CustomDebugStringConvertible {
    internal let _core: _StringCore
    internal let _startIndex: Index
    internal let _endIndex: Index

    init(_ _core: _StringCore) {
      self._core = _core
      self._endIndex = Index(_core, _core.endIndex, Index._emptyBuffer)
      if _fastPath(_core.count != 0) {
        let (_, buffer) = _core._encodeSomeUTF8(from: 0)
        self._startIndex = Index(_core, 0, buffer)
      } else {
        self._startIndex = self._endIndex
      }
    }

    init(_ _core: _StringCore, _ s: Index, _ e: Index) {
      self._core = _core
      self._startIndex = s
      self._endIndex = e
    }

    /// A position in a `String.UTF8View`.
    public struct Index : Comparable {
      internal typealias Buffer = _StringCore._UTF8Chunk

      init(_ _core: _StringCore, _ _coreIndex: Int,
           _ _buffer: Buffer) {
        self._core = _core
        self._coreIndex = _coreIndex
        self._buffer = _buffer
        _sanityCheck(_coreIndex >= 0)
        _sanityCheck(_coreIndex <= _core.count)
      }

      /// True iff the index is at the end of its view or if the next
      /// byte begins a new UnicodeScalar.
      internal var _isOnUnicodeScalarBoundary : Bool {
        let buffer = UInt32(truncatingBitPattern: _buffer)
        let (codePoint, _) = UTF8._decodeOne(buffer)
        return codePoint != nil || _isAtEnd
      }

      /// True iff the index is at the end of its view
      internal var _isAtEnd : Bool {
        return _buffer == Index._emptyBuffer
          && _coreIndex == _core.endIndex
      }

      /// The value of the buffer when it is empty
      internal static var _emptyBuffer: Buffer {
        return ~0
      }

      /// A Buffer value with the high byte set
      internal static var _bufferHiByte: Buffer {
        return 0xFF << numericCast((sizeof(Buffer.self) &- 1) &* 8)
      }

      /// Consume a byte of the given buffer: shift out the low byte
      /// and put FF in the high byte
      internal static func _nextBuffer(after thisBuffer: Buffer) -> Buffer {
        return (thisBuffer >> 8) | _bufferHiByte
      }

      /// The underlying buffer we're presenting as UTF-8
      internal let _core: _StringCore
      /// The position of `self`, rounded up to the nearest unicode
      /// scalar boundary, in the underlying UTF-16.
      internal let _coreIndex: Int
      /// If `self` is at the end of its `_core`, has the value `_endBuffer`.
      /// Otherwise, the low byte contains the value of
      internal let _buffer: Buffer
    }

    public typealias IndexDistance = Int

    /// The position of the first code unit if the `String` is
    /// non-empty; identical to `endIndex` otherwise.
    public var startIndex: Index {
      return self._startIndex
    }

    /// The "past the end" position.
    ///
    /// `endIndex` is not a valid argument to `subscript`, and is always
    /// reachable from `startIndex` by zero or more applications of
    /// `index(after:)`.
    public var endIndex: Index {
      return self._endIndex
    }

    /// Returns the next consecutive position after `i`.
    ///
    /// - Precondition: The next position is representable.
    @warn_unused_result
    public func index(after i: Index) -> Index {
      // FIXME: swift-3-indexing-model: range check i?
      let currentUnit = UTF8.CodeUnit(truncatingBitPattern: i._buffer)
      let hiNibble = currentUnit >> 4
      // Map the high nibble of the current code unit into the
      // amount by which to increment the UTF-16 index.  Only when
      // the high nibble is 1111 do we have a surrogate pair.
      let u16Increments = Int(bitPattern:
      // 1111 1110 1101 1100 1011 1010 1001 1000 0111 0110 0101 0100 0011 0010 0001 0000
         0b10___01___01___01___00___00___00___00___01___01___01___01___01___01___01___01)
      let increment = (u16Increments >> numericCast(hiNibble << 1)) & 0x3
      let nextCoreIndex = i._coreIndex &+ increment
      let nextBuffer = Index._nextBuffer(after: i._buffer)

      // if the nextBuffer is non-empty, we have all we need
      if _fastPath(nextBuffer != Index._emptyBuffer) {
        return Index(i._core, nextCoreIndex, nextBuffer)
      }
      // If the underlying UTF16 isn't exhausted, fill a new buffer
      else if _fastPath(nextCoreIndex < i._core.endIndex) {
        let (_, freshBuffer) = i._core._encodeSomeUTF8(from: nextCoreIndex)
        return Index(_core, nextCoreIndex, freshBuffer)
      }
      else {
        // Produce the endIndex
        _precondition(
          nextCoreIndex == i._core.endIndex,
          "Can't increment past endIndex of String.UTF8View")
        return Index(_core, nextCoreIndex, nextBuffer)
      }
    }

    /// Access the element at `position`.
    ///
    /// - Precondition: `position` is a valid position in `self` and
    ///   `position != endIndex`.
    public subscript(position: Index) -> UTF8.CodeUnit {
      let result = UTF8.CodeUnit(truncatingBitPattern: position._buffer & 0xFF)
      _precondition(result != 0xFF, "cannot subscript using endIndex")
      return result
    }

    /// Access the contiguous subrange of elements enclosed by `bounds`.
    ///
    /// - Complexity: O(1) unless bridging from Objective-C requires an
    ///   O(N) conversion.
    public subscript(bounds: Range<Index>) -> UTF8View {
      return UTF8View(_core, bounds.lowerBound, bounds.upperBound)
    }

    public var description: String {
      return String._fromCodeUnitSequenceWithRepair(UTF8.self, input: self).0
    }

    public var debugDescription: String {
      return "UTF8View(\(self.description.debugDescription))"
    }
  }

  /// A UTF-8 encoding of `self`.
  public var utf8: UTF8View {
    get {
      return UTF8View(self._core)
    }
    set {
      self = String(newValue)
    }
  }

  public var _contiguousUTF8: UnsafeMutablePointer<UTF8.CodeUnit>? {
    return _core.elementWidth == 1 ? _core.startASCII : nil
  }

  /// A contiguously-stored nul-terminated UTF-8 representation of
  /// `self`.
  ///
  /// To access the underlying memory, invoke
  /// `withUnsafeBufferPointer` on the `ContiguousArray`.
  public var nulTerminatedUTF8: ContiguousArray<UTF8.CodeUnit> {
    var result = ContiguousArray<UTF8.CodeUnit>()
    result.reserveCapacity(utf8.count + 1)
    result += utf8
    result.append(0)
    return result
  }

  internal func _withUnsafeBufferPointerToUTF8<R>(
    _ body: @noescape (UnsafeBufferPointer<UTF8.CodeUnit>) throws -> R
  ) rethrows -> R {
    let ptr = _contiguousUTF8
    if ptr != nil {
      return try body(UnsafeBufferPointer(start: ptr, count: _core.count))
    }
    return try nulTerminatedUTF8.withUnsafeBufferPointer(body)
  }

  /// Construct the `String` corresponding to the given sequence of
  /// UTF-8 code units.  If `utf8` contains unpaired surrogates, the
  /// result is `nil`.
  public init?(_ utf8: UTF8View) {
    let wholeString = String(utf8._core)

    if let start = utf8.startIndex.samePosition(in: wholeString),
       let end = utf8.endIndex.samePosition(in: wholeString) {
      self = wholeString[start..<end]
      return
    }
    return nil
  }

  /// The index type for subscripting a `String`'s `.utf8` view.
  public typealias UTF8Index = UTF8View.Index
}

// FIXME: swift-3-indexing-model: add complete set of forwards for Comparable 
//        assuming String.UTF8View.Index continues to exist
@warn_unused_result
public func == (
  lhs: String.UTF8View.Index,
  rhs: String.UTF8View.Index
) -> Bool {
  // If the underlying UTF16 index differs, they're unequal
  if lhs._coreIndex != rhs._coreIndex {
    return false
  }

  // Match up bytes in the buffer
  var buffer = (lhs._buffer, rhs._buffer)
  var isContinuation: Bool
  repeat {
    let unit = (
      UTF8.CodeUnit(truncatingBitPattern: buffer.0),
      UTF8.CodeUnit(truncatingBitPattern: buffer.1))

    isContinuation = UTF8.isContinuation(unit.0)
    if !isContinuation {
      // We don't check for unit equality in this case because one of
      // the units might be an 0xFF read from the end of the buffer.
      return !UTF8.isContinuation(unit.1)
    }
    // Continuation bytes must match exactly
    else if unit.0 != unit.1 {
      return false
    }

    // Move the buffers along.
    buffer = (
      String.UTF8Index._nextBuffer(after: buffer.0),
      String.UTF8Index._nextBuffer(after: buffer.1))
  }
  while true
}

@warn_unused_result
public func < (
  lhs: String.UTF8View.Index,
  rhs: String.UTF8View.Index
) -> Bool {
  // FIXME: swift-3-indexing-model: tests.
  // FIXME: swift-3-indexing-model: this implementation is wrong, it is just a
  // temporary HACK.
  return lhs._coreIndex < rhs._coreIndex
}

// Index conversions
extension String.UTF8View.Index {
  internal init(_ core: _StringCore, _utf16Offset: Int) {
      let (_, buffer) = core._encodeSomeUTF8(from: _utf16Offset)
      self.init(core, _utf16Offset, buffer)
  }

  /// Construct the position in `utf8` that corresponds exactly to
  /// `utf16Index`. If no such position exists, the result is `nil`.
  ///
  /// - Precondition: `utf8Index` is an element of
  ///   `String(utf16)!.utf8.indices`.
  public init?(_ utf16Index: String.UTF16Index, within utf8: String.UTF8View) {
    let utf16 = String.UTF16View(utf8._core)

    if utf16Index != utf16.startIndex
    && utf16Index != utf16.endIndex {
      _precondition(
        utf16Index >= utf16.startIndex
        && utf16Index <= utf16.endIndex,
        "Invalid String.UTF16Index for this UTF-8 view")

      // Detect positions that have no corresponding index.  Note that
      // we have to check before and after, because an unpaired
      // surrogate will be decoded as a single replacement character,
      // thus making the corresponding position valid in UTF8.
      if UTF16.isTrailSurrogate(utf16[utf16Index])
        && UTF16.isLeadSurrogate(utf16[utf16.index(before: utf16Index)]) {
        return nil
      }
    }
    self.init(utf8._core, _utf16Offset: utf16Index._offset)
  }

  /// Construct the position in `utf8` that corresponds exactly to
  /// `unicodeScalarIndex`.
  ///
  /// - Precondition: `unicodeScalarIndex` is an element of
  ///   `String(utf8)!.unicodeScalars.indices`.
  public init(
    _ unicodeScalarIndex: String.UnicodeScalarIndex,
    within utf8: String.UTF8View
  ) {
    self.init(utf8._core, _utf16Offset: unicodeScalarIndex._position)
  }

  /// Construct the position in `utf8` that corresponds exactly to
  /// `characterIndex`.
  ///
  /// - Precondition: `characterIndex` is an element of
  ///   `String(utf8)!.indices`.
  public init(_ characterIndex: String.Index, within utf8: String.UTF8View) {
    self.init(utf8._core, _utf16Offset: characterIndex._base._position)
  }

  /// Returns the position in `utf16` that corresponds exactly
  /// to `self`, or if no such position exists, `nil`.
  ///
  /// - Precondition: `self` is an element of `String(utf16)!.utf8.indices`.
  @warn_unused_result
  public func samePosition(
    in utf16: String.UTF16View
  ) -> String.UTF16View.Index? {
    return String.UTF16View.Index(self, within: utf16)
  }

  /// Returns the position in `unicodeScalars` that corresponds exactly
  /// to `self`, or if no such position exists, `nil`.
  ///
  /// - Precondition: `self` is an element of
  ///   `String(unicodeScalars).utf8.indices`.
  @warn_unused_result
  public func samePosition(
    in unicodeScalars: String.UnicodeScalarView
  ) -> String.UnicodeScalarIndex? {
    return String.UnicodeScalarIndex(self, within: unicodeScalars)
  }

  /// Returns the position in `characters` that corresponds exactly
  /// to `self`, or if no such position exists, `nil`.
  ///
  /// - Precondition: `self` is an element of `characters.utf8.indices`.
  @warn_unused_result
  public func samePosition(
    in characters: String
  ) -> String.Index? {
    return String.Index(self, within: characters)
  }
}

// Reflection
extension String.UTF8View : CustomReflectable {
  /// Returns a mirror that reflects `self`.
  public var customMirror: Mirror {
    return Mirror(self, unlabeledChildren: self)
  }
}

extension String.UTF8View : CustomPlaygroundQuickLookable {
  public var customPlaygroundQuickLook: PlaygroundQuickLook {
    return .text(description)
  }
}

