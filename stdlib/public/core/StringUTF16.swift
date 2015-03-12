//===--- StringUTF16.swift ------------------------------------------------===//
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

extension String {
  /// A collection of UTF-16 code units that encodes a `String` value.
  public struct UTF16View : Sliceable, Reflectable, Printable, DebugPrintable {
    public struct Index {
      // Foundation needs access to these fields so it can expose
      // random access
      public // SPI(Foundation)
      init(_offset: Int) { self._offset = _offset }
      
      public let _offset: Int
    }
    
    /// The position of the first code unit if the `String` is
    /// non-empty; identical to `endIndex` otherwise.
    public var startIndex: Index {
      return Index(_offset: 0)
    }
    
    /// The "past the end" position.
    ///
    /// `endIndex` is not a valid argument to `subscript`, and is always
    /// reachable from `startIndex` by zero or more applications of
    /// `successor()`.
    public var endIndex: Index {
      return Index(_offset: _length)
    }

    func _toInternalIndex(i: Int) -> Int {
      return _core.startIndex + _offset + i
    }

    public func generate() -> IndexingGenerator<UTF16View> {
      return IndexingGenerator(self)
    }

    /// Access the element at `position`.
    ///
    /// Requires: `position` is a valid position in `self` and
    /// `position != endIndex`.
    public subscript(i: Index) -> UTF16.CodeUnit {
      let position = i._offset
      _precondition(position >= 0 && position < _length,
          "out-of-range access on a UTF16View")

      var index = _toInternalIndex(position)
      let u = _core[index]
      if _fastPath((u >> 11) != 0b1101_1) {
        // Neither high-surrogate, nor low-surrogate -- well-formed sequence
        // of 1 code unit.
        return u
      }

      if (u >> 10) == 0b1101_10 {
        // `u` is a high-surrogate.  SequenceType is well-formed if it
        // is followed by a low-surrogate.
        if _fastPath(
               index + 1 < _core.count &&
               (_core[index + 1] >> 10) == 0b1101_11) {
          return u
        }
        return 0xfffd
      }

      // `u` is a low-surrogate.  SequenceType is well-formed if
      // previous code unit is a high-surrogate.
      if _fastPath(index != 0 && (_core[index - 1] >> 10) == 0b1101_10) {
        return u
      }
      return 0xfffd
    }

#if _runtime(_ObjC)
    // These may become less important once <rdar://problem/19255291> is addressed.

    @availability(
      *, unavailable,
      message="Indexing a String's UTF16View requires a String.UTF16View.Index, which can be constructed from Int when Foundation is imported")
    public subscript(i: Int) -> UTF16.CodeUnit {
      return self[Index(_offset: i)]
    }

    @availability(
      *, unavailable,
      message="Slicing a String's UTF16View requires a Range<String.UTF16View.Index>, String.UTF16View.Index can be constructed from Int when Foundation is imported")
    public subscript(subRange: Range<Int>) -> UTF16View {
      return self[Index(_offset: subRange.startIndex)..<Index(_offset: subRange.endIndex)]
    }
#endif
    
    /// Access the elements delimited by the given half-open range of
    /// indices.
    ///
    /// Complexity: O(1) unless bridging from Objective-C requires an
    /// O(N) conversion.
    public subscript(subRange: Range<Index>) -> UTF16View {
      return UTF16View(
        _core, offset: _toInternalIndex(subRange.startIndex._offset),
          length: subRange.endIndex._offset - subRange.startIndex._offset)
    }

    internal init(_ _core: _StringCore) {
      self._offset = 0
      self._length = _core.count
      self._core = _core
    }

    internal init(_ _core: _StringCore, offset: Int, length: Int) {
      self._offset = offset
      self._length = length
      self._core = _core
    }
    
    /// Returns a mirror that reflects `self`.
    public func getMirror() -> MirrorType {
      return _UTF16ViewMirror(self)
    }

    public var description: String {
      let start = _toInternalIndex(0)
      let end = _toInternalIndex(_length)
      return String(_core[start..<end])
    }

    public var debugDescription: String {
      return "StringUTF16(\(self.description.debugDescription))"
    }

    var _offset: Int
    var _length: Int
    let _core: _StringCore
  }

  /// A UTF-16 encoding of `self`.
  public var utf16: UTF16View {
    return UTF16View(_core)
  }

  /// Construct the `String` corresponding to the given sequence of
  /// UTF-16 code units.  If `utf16` contains unpaired surrogates, the
  /// result is `nil`.
  public init?(_ utf16: UTF16View) {
    let wholeString = String(utf16._core)
    
    if let start = UTF16Index(
      _offset: utf16._offset
    ).samePositionIn(wholeString) {
      if let end = UTF16Index(
        _offset: utf16._offset + utf16._length
      ).samePositionIn(wholeString) {
        self = wholeString[start..<end]
        return
      }
    }
    return nil
  }
  
  /// The index type for subscripting a `String`\ 's `utf16` view.
  public typealias UTF16Index = UTF16View.Index
}

// Conformance to RandomAccessIndexType intentionally only appears
// when Foundation is loaded
extension String.UTF16View.Index : BidirectionalIndexType {
  public typealias Distance = Int

  public func successor() -> String.UTF16View.Index {
    return String.UTF16View.Index(_offset: _offset.successor())
  }
  public func predecessor() -> String.UTF16View.Index {
    return String.UTF16View.Index(_offset: _offset.predecessor())
  }
}

public func == (
  lhs: String.UTF16View.Index, rhs: String.UTF16View.Index
) -> Bool {
  return lhs._offset == rhs._offset
}

extension String.UTF16View.Index : Comparable, Equatable {}

public func < (
  lhs: String.UTF16View.Index, rhs: String.UTF16View.Index
) -> Bool {
  return lhs._offset < rhs._offset
}

// We can do some things more efficiently, even if we don't promise to
// by conforming to RandomAccessIndexType.

/// Do not use this operator directly; call distance(start, end) instead
@inline(__always)
public func ~> (
  start: String.UTF16View.Index, rest:(_Distance, (String.UTF16View.Index))
) -> String.UTF16View.Index.Distance {
  let end = rest.1
  return start._offset.distanceTo(end._offset)
}

/// Do not use this operator directly; call advance(start, n) instead
@inline(__always)
public func ~> (
  start: String.UTF16View.Index,
  rest: (_Advance, (String.UTF16View.Index.Distance))
) -> String.UTF16View.Index {
  let n = rest.1
  return String.UTF16View.Index(_offset: start._offset.advancedBy(n))
}

/// Do not use this operator directly; call advance(start, n, end) instead
@inline(__always)
public func ~> (
  start: String.UTF16View.Index,
  rest: (_Advance, (String.UTF16View.Index.Distance, String.UTF16View.Index))
) -> String.UTF16View.Index {
  let n = rest.1.0
  let end = rest.1.1

  return String.UTF16View.Index(
    _offset: advance(start._offset, n, end._offset))
}

// Index conversions
extension String.UTF16View.Index {
  /// Construct the position in `utf16` that corresponds exactly to
  /// `utf8Index`. If no such position exists, the result is `nil`.
  ///
  /// Requires: `utf8Index` is an element of
  /// `indices(String(utf16)!.utf8)`.
  public init?(
    _ utf8Index: String.UTF8Index, within utf16: String.UTF16View
  ) {
    let core = utf16._core
    
    _precondition(
      utf8Index._coreIndex >= 0 && utf8Index._coreIndex <= core.endIndex,
      "Invalid String.UTF8Index for this UTF-16 view")

    // Detect positions that have no corresponding index.
    if !utf8Index._isOnUnicodeScalarBoundary {
      return nil
    }
    _offset = utf8Index._coreIndex
  }
  
  /// Construct the position in `utf16` that corresponds exactly to
  /// `unicodeScalarIndex`.
  ///
  /// Requires: `unicodeScalarIndex` is an element of
  /// `indices(String(utf16)!.unicodeScalars)`.
  public init(
    _ unicodeScalarIndex: String.UnicodeScalarIndex,
    within utf16: String.UTF16View) {
    _offset = unicodeScalarIndex._position
  }
  
  /// Construct the position in `utf16` that corresponds exactly to
  /// `characterIndex`.
  ///
  /// Requires: `characterIndex` is an element of
  /// `indices(String(utf16)!)`.
  public init(_ characterIndex: String.Index, within utf16: String.UTF16View) {
    _offset = characterIndex._utf16Index
  }

  /// Return the position in `utf8` that corresponds exactly
  /// to `self`, or if no such position exists, `nil`.
  ///
  /// Requires: `self` is an element of
  /// `indices(String(utf8)!.utf16)`.
  public func samePositionIn(
    utf8: String.UTF8View
  ) -> String.UTF8View.Index? {
    return String.UTF8View.Index(self, within: utf8)
  }

  /// Return the position in `unicodeScalars` that corresponds exactly
  /// to `self`, or if no such position exists, `nil`.
  ///
  /// Requires: `self` is an element of
  /// `indices(String(unicodeScalars).utf16)`.
  public func samePositionIn(
    unicodeScalars: String.UnicodeScalarView
  ) -> String.UnicodeScalarIndex? {
    return String.UnicodeScalarIndex(self, within: unicodeScalars)
  }
  
  /// Return the position in `characters` that corresponds exactly
  /// to `self`, or if no such position exists, `nil`.
  ///
  /// Requires: `self` is an element of `indices(characters.utf16)`.
  public func samePositionIn(
    characters: String
  ) -> String.Index? {
    return String.Index(self, within: characters)
  }
}
