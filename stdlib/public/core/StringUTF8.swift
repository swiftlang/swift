//===--- StringUTF8.swift - A UTF8 view of _StringCore --------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
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


extension _StringCore {
  /// An integral type that holds a sequence of UTF-8 code units, starting in
  /// its low byte.
  public typealias UTF8Chunk = UInt64

  /// Encode text starting at `i` as UTF-8.  Returns a pair whose first
  /// element is the index of the text following whatever got encoded,
  /// and the second element contains the encoded UTF-8 starting in its
  /// low byte.  Any unused high bytes in the result will be set to
  /// 0xFF.
  func _encodeSomeUTF8(i: Int) -> (Int, UTF8Chunk) {
    _sanityCheck(i <= count)

    if _fastPath(elementWidth == 1) {
      // How many UTF-16 code units might we use before we've filled up
      // our UTF8Chunk with UTF-8 code units?
      let utf16Count = min(sizeof(UTF8Chunk.self), count - i)

      var result: UTF8Chunk = ~0 // start with all bits set

      _memcpy(
        dest: UnsafeMutablePointer(Builtin.addressof(&result)),
        src: UnsafeMutablePointer(startASCII + i),
        size: numericCast(utf16Count))

      return (i + utf16Count, result)
    } else if _fastPath(!_baseAddress._isNull) {
      return _encodeSomeContiguousUTF16AsUTF8(i)
    } else {
#if _runtime(_ObjC)
      return _encodeSomeNonContiguousUTF16AsUTF8(i)
#else
      _sanityCheckFailure("_encodeSomeUTF8: Unexpected cocoa string")
#endif
    }
  }

  /// Helper for `_encodeSomeUTF8`, above.  Handles the case where the
  /// storage is contiguous UTF-16.
  func _encodeSomeContiguousUTF16AsUTF8(i: Int) -> (Int, UTF8Chunk) {
    _sanityCheck(elementWidth == 2)
    _sanityCheck(!_baseAddress._isNull)

    let storage = UnsafeBufferPointer(start: startUTF16, count: self.count)
    return _transcodeSomeUTF16AsUTF8(storage, i)
  }

#if _runtime(_ObjC)
  /// Helper for `_encodeSomeUTF8`, above.  Handles the case where the
  /// storage is non-contiguous UTF-16.
  func _encodeSomeNonContiguousUTF16AsUTF8(i: Int) -> (Int, UTF8Chunk) {
    _sanityCheck(elementWidth == 2)
    _sanityCheck(_baseAddress._isNull)

    let storage = _CollectionOf<Int, UInt16>(
      startIndex: 0, endIndex: self.count) {
      (i: Int) -> UInt16 in
      return _cocoaStringSubscript(self, i)
    }
    return _transcodeSomeUTF16AsUTF8(storage, i)
  }
#endif
}

extension String {
  /// A collection of UTF-8 code units that encodes a `String` value.
  public struct UTF8View : CollectionType, Reflectable, CustomStringConvertible,
    CustomDebugStringConvertible {
    internal let _core: _StringCore
    internal let _startIndex: Index
    internal let _endIndex: Index

    init(_ _core: _StringCore) {
      self._core = _core
      self._endIndex = Index(_core, _core.endIndex, Index._emptyBuffer)
      if _fastPath(_core.count != 0) {
        let (_, buffer) = _core._encodeSomeUTF8(0)
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

    /// A position in a `String.UTF8View`
    public struct Index : ForwardIndexType {
      internal typealias Buffer = _StringCore.UTF8Chunk

      init(_ _core: _StringCore, _ _coreIndex: Int,
           _ _buffer: Buffer) {
        self._core = _core
        self._coreIndex = _coreIndex
        self._buffer = _buffer
        _sanityCheck(_coreIndex >= 0)
        _sanityCheck(_coreIndex <= _core.count)
      }

      /// Returns the next consecutive value after `self`.
      ///
      /// - Requires: The next value is representable.
      public func successor() -> Index {
        let currentUnit = UTF8.CodeUnit(truncatingBitPattern: _buffer)
        let hiNibble = currentUnit >> 4
        // Map the high nibble of the current code unit into the
        // amount by which to increment the utf16 index.  Only when
        // the high nibble is 1111 do we have a surrogate pair.
        let u16Increments = Int(bitPattern:
        // 1111 1110 1101 1100 1011 1010 1001 1000 0111 0110 0101 0100 0011 0010 0001 0000
           0b10___01___01___01___00___00___00___00___01___01___01___01___01___01___01___01)
        let increment = (u16Increments >> numericCast(hiNibble << 1)) & 0x3
        let nextCoreIndex = _coreIndex &+ increment
        let nextBuffer = Index._nextBuffer(_buffer)

        // if the nextBuffer is non-empty, we have all we need
        if _fastPath(nextBuffer != Index._emptyBuffer) {
          return Index(_core, nextCoreIndex, nextBuffer)
        }
        // If the underlying UTF16 isn't exhausted, fill a new buffer
        else if _fastPath(nextCoreIndex < _core.endIndex) {
          let (_, freshBuffer) = _core._encodeSomeUTF8(nextCoreIndex)
          return Index(_core, nextCoreIndex, freshBuffer)
        }
        else {
          // Produce the endIndex
          _precondition(
            nextCoreIndex == _core.endIndex,
            "Can't increment past endIndex of String.UTF8View")
          return Index(_core, nextCoreIndex, nextBuffer)
        }
      }

      /// True iff the index is at the end of its view or if the next
      /// byte begins a new UnicodeScalar.
      internal var _isOnUnicodeScalarBoundary : Bool {
        let next = UTF8.CodeUnit(truncatingBitPattern: _buffer)
        return UTF8._numTrailingBytes(next) != 4 || _isAtEnd
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
      internal static func _nextBuffer(thisBuffer: Buffer) -> Buffer {
        return (thisBuffer >> 8) | _bufferHiByte
      }

      /// The underlying buffer we're presenting as UTF8
      internal let _core: _StringCore
      /// The position of `self`, rounded up to the nearest unicode
      /// scalar boundary, in the underlying UTF16.
      internal let _coreIndex: Int
      /// If `self` is at the end of its `_core`, has the value `_endBuffer`.
      /// Otherwise, the low byte contains the value of
      internal let _buffer: Buffer
    }

    /// The position of the first code unit if the `String` is
    /// non-empty; identical to `endIndex` otherwise.
    public var startIndex: Index {
      return self._startIndex
    }

    /// The "past the end" position.
    ///
    /// `endIndex` is not a valid argument to `subscript`, and is always
    /// reachable from `startIndex` by zero or more applications of
    /// `successor()`.
    public var endIndex: Index {
      return self._endIndex
    }

    /// Access the element at `position`.
    ///
    /// - Requires: `position` is a valid position in `self` and
    ///   `position != endIndex`.
    public subscript(position: Index) -> UTF8.CodeUnit {
      let result: UTF8.CodeUnit = numericCast(position._buffer & 0xFF)
      _precondition(result != 0xFF, "can not subscript using endIndex")
      return result
    }

    /// Access the elements delimited by the given half-open range of
    /// indices.
    ///
    /// - Complexity: O(1) unless bridging from Objective-C requires an
    ///   O(N) conversion.
    public subscript(subRange: Range<Index>) -> UTF8View {
      return UTF8View(_core, subRange.startIndex, subRange.endIndex)
    }

    /// Return a *generator* over the code points that comprise this
    /// *sequence*.
    ///
    /// - Complexity: O(1)
    public func generate() -> IndexingGenerator<UTF8View> {
      return IndexingGenerator(self)
    }

    /// Returns a mirror that reflects `self`.
    public func getMirror() -> MirrorType {
      return _UTF8ViewMirror(self)
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
    return UTF8View(self._core)
  }

  public var _contiguousUTF8: UnsafeMutablePointer<UTF8.CodeUnit> {
    return _core.elementWidth == 1 ? _core.startASCII : nil
  }

  /// A contiguously-stored nul-terminated UTF-8 representation of
  /// `self`.
  ///
  /// To access the underlying memory, invoke
  /// `withUnsafeBufferPointer` on the `ContiguousArray`.
  public var nulTerminatedUTF8: ContiguousArray<UTF8.CodeUnit> {
    var result = ContiguousArray<UTF8.CodeUnit>()
    result.reserveCapacity(utf8.count() + 1)
    result += utf8
    result.append(0)
    return result
  }

  /// Construct the `String` corresponding to the given sequence of
  /// UTF-8 code units.  If `utf8` contains unpaired surrogates, the
  /// result is `nil`.
  public init?(_ utf8: UTF8View) {
    let wholeString = String(utf8._core)

    if let start = utf8.startIndex.samePositionIn(wholeString),
       let end = utf8.endIndex.samePositionIn(wholeString) {
      self = wholeString[start..<end]
      return
    }
    return nil
  }

  /// The index type for subscripting a `String`'s `.utf8` view.
  public typealias UTF8Index = UTF8View.Index
}

public
func == (lhs: String.UTF8View.Index, rhs: String.UTF8View.Index) -> Bool {
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
      String.UTF8Index._nextBuffer(buffer.0),
      String.UTF8Index._nextBuffer(buffer.1))
  }
  while true
}

// Index conversions
extension String.UTF8View.Index {
  internal init(_ core: _StringCore, _utf16Offset: Int) {
      let (_, buffer) = core._encodeSomeUTF8(_utf16Offset)
      self.init(core, _utf16Offset, buffer)
  }

  /// Construct the position in `utf8` that corresponds exactly to
  /// `utf16Index`. If no such position exists, the result is `nil`.
  ///
  /// - Requires: `utf8Index` is an element of
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
        && UTF16.isLeadSurrogate(utf16[utf16Index.predecessor()]) {
        return nil
      }
    }
    self.init(utf8._core, _utf16Offset: utf16Index._offset)
  }

  /// Construct the position in `utf8` that corresponds exactly to
  /// `unicodeScalarIndex`.
  ///
  /// - Requires: `unicodeScalarIndex` is an element of
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
  /// - Requires: `characterIndex` is an element of
  ///   `String(utf8)!.indices`.
  public init(_ characterIndex: String.Index, within utf8: String.UTF8View) {
    self.init(utf8._core, _utf16Offset: characterIndex._base._position)
  }

  /// Return the position in `utf16` that corresponds exactly
  /// to `self`, or if no such position exists, `nil`.
  ///
  /// - Requires: `self` is an element of `String(utf16)!.utf8.indices`.
  public func samePositionIn(
    utf16: String.UTF16View
  ) -> String.UTF16View.Index? {
    return String.UTF16View.Index(self, within: utf16)
  }

  /// Return the position in `unicodeScalars` that corresponds exactly
  /// to `self`, or if no such position exists, `nil`.
  ///
  /// - Requires: `self` is an element of
  ///   `String(unicodeScalars).utf8.indices`.
  public func samePositionIn(
    unicodeScalars: String.UnicodeScalarView
  ) -> String.UnicodeScalarIndex? {
    return String.UnicodeScalarIndex(self, within: unicodeScalars)
  }

  /// Return the position in `characters` that corresponds exactly
  /// to `self`, or if no such position exists, `nil`.
  ///
  /// - Requires: `self` is an element of `characters.utf8.indices`.
  public func samePositionIn(
    characters: String
  ) -> String.Index? {
    return String.Index(self, within: characters)
  }
}
