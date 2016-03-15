//===----------------------------------------------------------------------===//
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

@warn_unused_result
public func == (
  lhs: String.UnicodeScalarView.Index,
  rhs: String.UnicodeScalarView.Index
) -> Bool {
  return lhs._position == rhs._position
}

@warn_unused_result
public func < (
  lhs: String.UnicodeScalarView.Index,
  rhs: String.UnicodeScalarView.Index
) -> Bool {
  return lhs._position < rhs._position
}

extension String {
  /// A collection of [Unicode scalar values](http://www.unicode.org/glossary/#unicode_scalar_value) that
  /// encodes a `String` value.
  public struct UnicodeScalarView :
    BidirectionalCollection,
    CustomStringConvertible,
    CustomDebugStringConvertible
  {
    internal init(_ _core: _StringCore) {
      self._core = _core
    }

    internal struct _ScratchIterator : IteratorProtocol {
      var core: _StringCore
      var idx: Int
      init(_ core: _StringCore, _ pos: Int) {
        self.idx = pos
        self.core = core
      }
      mutating func next() -> UTF16.CodeUnit? {
        if idx == core.endIndex {
          return nil
        }
        defer { idx += 1 }
        return self.core[idx]
      }
    }

    /// A position in a `String.UnicodeScalarView`.
    public struct Index : Comparable {
      public init(_ _position: Int) {
        self._position = _position
      }

      internal var _position: Int
    }

    /// The position of the first `UnicodeScalar` if the `String` is
    /// non-empty; identical to `endIndex` otherwise.
    public var startIndex: Index {
      return Index(_core.startIndex)
    }

    /// The "past the end" position.
    ///
    /// `endIndex` is not a valid argument to `subscript`, and is always
    /// reachable from `startIndex` by zero or more applications of
    /// `successor()`.
    public var endIndex: Index {
      return Index(_core.endIndex)
    }

    // TODO: swift-3-indexing-model - add docs
    @warn_unused_result
    public func next(i: Index) -> Index {
      // FIXME: swift-3-indexing-model: range checks?
      var scratch = _ScratchIterator(_core, i._position)
      var decoder = UTF16()
      let (_, length) = decoder._decodeOne(&scratch)
      return Index(i._position + length)
    }

    // TODO: swift-3-indexing-model - add docs
    @warn_unused_result
    public func previous(i: Index) -> Index {
      // FIXME: swift-3-indexing-model: range checks?
      var position = i._position-1
      if _slowPath((_core[position] >> 10) == 0b1101_11) {
        if position != 0 && (_core[position - 1] >> 10) == 0b1101_10 {
          position -= 1
        }
      }
      return Index(position)
    }

    /// Access the element at `position`.
    ///
    /// - Precondition: `position` is a valid position in `self` and
    ///   `position != endIndex`.
    public subscript(position: Index) -> UnicodeScalar {
      var scratch = _ScratchIterator(_core, position._position)
      var decoder = UTF16()
      switch decoder.decode(&scratch) {
      case .scalarValue(let us):
        return us
      case .emptyInput:
        _sanityCheckFailure("cannot subscript using an endIndex")
      case .error:
        return UnicodeScalar(0xfffd)
      }
    }

    /// Access the contiguous subrange of elements enclosed by `bounds`.
    ///
    /// - Complexity: O(1) unless bridging from Objective-C requires an
    ///   O(N) conversion.
    public subscript(r: Range<Index>) -> UnicodeScalarView {
      return UnicodeScalarView(
        _core[r.startIndex._position..<r.endIndex._position])
    }

    /// A type whose instances can produce the elements of this
    /// sequence, in order.
    public struct Iterator : IteratorProtocol {
      init(_ _base: _StringCore) {
        if _base.hasContiguousStorage {
            self._baseSet = true
          if _base.isASCII {
            self._ascii = true
            self._asciiBase = UnsafeBufferPointer<UInt8>(
              start: UnsafePointer(_base._baseAddress),
              count: _base.count).makeIterator()
          } else {
            self._ascii = false
            self._base = UnsafeBufferPointer<UInt16>(
              start: UnsafePointer(_base._baseAddress),
              count: _base.count).makeIterator()
          }
        } else {
          self._ascii = false
          self._baseSet = false
          self._iterator = _base.makeIterator()
        }
      }

      /// Advance to the next element and return it, or `nil` if no next
      /// element exists.
      ///
      /// - Precondition: No preceding call to `self.next()` has returned
      ///   `nil`.
      public mutating func next() -> UnicodeScalar? {
        var result: UnicodeDecodingResult
        if _baseSet {
          if _ascii {
            switch self._asciiBase.next() {
            case let x?:
              result = .scalarValue(UnicodeScalar(x))
            case nil:
              result = .emptyInput
            }
          } else {
            result = _decoder.decode(&(self._base!))
          }
        } else {
          result = _decoder.decode(&(self._iterator!))
        }
        switch result {
        case .scalarValue(let us):
          return us
        case .emptyInput:
          return nil
        case .error:
          return UnicodeScalar(0xfffd)
        }
      }

      internal var _decoder: UTF16 = UTF16()
      internal let _baseSet: Bool
      internal let _ascii: Bool
      internal var _asciiBase: UnsafeBufferPointerIterator<UInt8>!
      internal var _base: UnsafeBufferPointerIterator<UInt16>!
      internal var _iterator: IndexingIterator<_StringCore>!
    }

    /// Returns an iterator over the `UnicodeScalar`s that comprise
    /// this sequence.
    ///
    /// - Complexity: O(1).
    @warn_unused_result
    public func makeIterator() -> Iterator {
      return Iterator(_core)
    }

    public var description: String {
      return String(_core[startIndex._position..<endIndex._position])
    }

    public var debugDescription: String {
      return "StringUnicodeScalarView(\(self.description.debugDescription))"
    }

    internal var _core: _StringCore
  }

  /// Construct the `String` corresponding to the given sequence of
  /// Unicode scalars.
  public init(_ unicodeScalars: UnicodeScalarView) {
    self.init(unicodeScalars._core)
  }

  /// The index type for subscripting a `String`'s `.unicodeScalars`
  /// view.
  public typealias UnicodeScalarIndex = UnicodeScalarView.Index
}

extension String {
  /// The value of `self` as a collection of [Unicode scalar values](http://www.unicode.org/glossary/#unicode_scalar_value).
  public var unicodeScalars : UnicodeScalarView {
    get {
      return UnicodeScalarView(_core)
    }
    set {
      _core = newValue._core
    }
  }
}

extension String.UnicodeScalarView : RangeReplaceableCollection {
  /// Construct an empty instance.
  public init() {
    self = String.UnicodeScalarView(_StringCore())
  }
  /// Reserve enough space to store `n` ASCII characters.
  ///
  /// - Complexity: O(`n`).
  public mutating func reserveCapacity(n: Int) {
    _core.reserveCapacity(n)
  }
  /// Append `x` to `self`.
  ///
  /// - Complexity: Amortized O(1).
  public mutating func append(x: UnicodeScalar) {
    _core.append(x)
  }
  /// Append the elements of `newElements` to `self`.
  ///
  /// - Complexity: O(*length of result*).
  public mutating func append<
    S : Sequence where S.Iterator.Element == UnicodeScalar
  >(contentsOf newElements: S) {
    _core.append(contentsOf: newElements.lazy.flatMap { $0.utf16 })
  }
  /// Replace the elements within `bounds` with `newElements`.
  ///
  /// Invalidates all indices with respect to `self`.
  ///
  /// - Complexity: O(`bounds.count`) if `bounds.endIndex
  ///   == self.endIndex` and `newElements.isEmpty`, O(N) otherwise.
  public mutating func replaceSubrange<
    C: Collection where C.Iterator.Element == UnicodeScalar
  >(
    bounds: Range<Index>, with newElements: C
  ) {
    let rawSubRange: Range<Int> =
      bounds.startIndex._position
      ..< bounds.endIndex._position
    let lazyUTF16 = newElements.lazy.flatMap { $0.utf16 }
    _core.replaceSubrange(rawSubRange, with: lazyUTF16)
  }
}

// Index conversions
extension String.UnicodeScalarView {
  // TODO: swift-3-indexing-model - add docs
  @warn_unused_result
  public func index(
    equivalentTo utf8Index: String.UTF8Index
  ) -> String.UnicodeScalarIndex? {
    _precondition(
      utf8Index._coreIndex >= 0 && utf8Index._coreIndex <= _core.endIndex,
      "Invalid String.UTF8Index for this UnicodeScalar view")

    // Detect positions that have no corresponding index.
    if !utf8Index._isOnUnicodeScalarBoundary {
      return nil
    }

    return Index(utf8Index._coreIndex)
  }

  // TODO: swift-3-indexing-model - add docs
  @warn_unused_result
  public func index(
    equivalentTo utf16Index: String.UTF16Index
  ) -> String.UnicodeScalarIndex? {
    let utf16 = String.UTF16View(_core)

    if utf16Index != utf16.startIndex && utf16Index != utf16.endIndex {
      _precondition(
        utf16Index >= utf16.startIndex && utf16Index <= utf16.endIndex,
        "Invalid String.UTF16Index for this UnicodeScalar view")

      // Detect positions that have no corresponding index.  Note that
      // we have to check before and after, because an unpaired
      // surrogate will be decoded as a single replacement character,
      // thus making the corresponding position valid.
      if UTF16.isTrailSurrogate(utf16[utf16Index])
      && UTF16.isLeadSurrogate(utf16[utf16.previous(utf16Index)]) {
        return nil
      }
    }

    return Index(utf16Index._offset)
  }

  // TODO: swift-3-indexing-model - add docs
  @warn_unused_result
  public func index(
    equivalentTo stringIndex: String.Index
  ) -> String.UnicodeScalarIndex? {
    // FIXME: swift-3-indexing-model: range check?
    return Index(stringIndex._base._position)
  }
}

extension String.UnicodeScalarView {
  /// Returns the length of the first extended grapheme cluster in UTF-16
  /// code units.
  @warn_unused_result
  @inline(never)
  internal func _measureExtendedGraphemeClusterForward(
    from start: Index
  ) -> Int {
    let end = self.endIndex
    if start == end {
      return 0
    }

    let clusterBreakProperty = _UnicodeGraphemeClusterBreakPropertyTrie()
    let clusterSegmenter = _UnicodeExtendedGraphemeClusterSegmenter()
    var index = start

    var gcb0 = clusterBreakProperty.getPropertyRawValue(
      self[index].value
    )

    _nextInPlace(&index)

    while index != end {
      // FIXME(performance): consider removing this "fast path".  A branch
      // that is hard to predict could be worse for performance than a few
      // loads from cache to fetch the property 'gcb1'.
      if clusterSegmenter.isBoundaryAfter(gcb0) {
        break
      }
      let gcb1 = clusterBreakProperty.getPropertyRawValue(
        self[index].value
      )
      if clusterSegmenter.isBoundary(gcb0, gcb1) {
        break
      }
      gcb0 = gcb1
      _nextInPlace(&index)
    }

    return index._position - start._position
  }

  /// Returns the length of the previous extended grapheme cluster in UTF-16
  /// code units.
  @warn_unused_result
  @inline(never)
  internal func _measureExtendedGraphemeClusterBackward(
    from end: Index
  ) -> Int {
    let start = self.startIndex
    if start == end {
      return 0
    }

    let clusterBreakProperty = _UnicodeGraphemeClusterBreakPropertyTrie()
    let clusterSegmenter = _UnicodeExtendedGraphemeClusterSegmenter()
    var index = end

    _previousInPlace(&index)
    var gcb0 = clusterBreakProperty.getPropertyRawValue(
      self[index].value
    )

    while index != start {
      _previousInPlace(&index)
      let gcb1 = clusterBreakProperty.getPropertyRawValue(
        self[index].value
      )
      if clusterSegmenter.isBoundary(gcb1, gcb0) {
        break
      }
      gcb0 = gcb1
    }

    return end._position - index._position
  }

  internal func _isOnGraphemeClusterBoundary(
    index: String.UnicodeScalarIndex
  ) -> Bool {
    fatalError("FIXME: swift-3-indexing-model: implement")

    // FIXME: swift-3-indexing-model: rework the following to work
    //        against the supplied index
    /*
    let scalars = String.UnicodeScalarView(_core)
    if self == scalars.startIndex || self == scalars.endIndex {
    return true
    }
    let precedingScalar = scalars[self.predecessor()]

    let graphemeClusterBreakProperty =
    _UnicodeGraphemeClusterBreakPropertyTrie()
    let segmenter = _UnicodeExtendedGraphemeClusterSegmenter()

    let gcb0 = graphemeClusterBreakProperty.getPropertyRawValue(
    precedingScalar.value)

    if segmenter.isBoundaryAfter(gcb0) {
    return true
    }

    let gcb1 = graphemeClusterBreakProperty.getPropertyRawValue(
    scalars[self].value)

    return segmenter.isBoundary(gcb0, gcb1)
    */
  }
}

// Reflection
extension String.UnicodeScalarView : CustomReflectable {
  /// Returns a mirror that reflects `self`.
  public var customMirror: Mirror {
    return Mirror(self, unlabeledChildren: self)
  }
}

extension String.UnicodeScalarView : CustomPlaygroundQuickLookable {
  public var customPlaygroundQuickLook: PlaygroundQuickLook {
    return .text(description)
  }
}
