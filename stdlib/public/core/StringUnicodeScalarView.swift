//===----------------------------------------------------------------------===//
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

public func ==(
  lhs: String.UnicodeScalarView.Index,
  rhs: String.UnicodeScalarView.Index
) -> Bool {
  return lhs._position == rhs._position
}

public func <(
  lhs: String.UnicodeScalarView.Index,
  rhs: String.UnicodeScalarView.Index
) -> Bool {
  return lhs._position < rhs._position
}

extension String {
  /// A collection of [Unicode scalar values](http://www.unicode.org/glossary/#unicode_scalar_value) that
  /// encode a `String` .
  public struct UnicodeScalarView : Sliceable, SequenceType, Reflectable,
    CustomStringConvertible, CustomDebugStringConvertible, CollectionType {
    init(_ _core: _StringCore) {
      self._core = _core
    }

    struct _ScratchGenerator : GeneratorType {
      var core: _StringCore
      var idx: Int
      init(_ core: _StringCore, _ pos: Int) {
        self.idx = pos
        self.core = core
      }
      mutating func next() -> UTF16.CodeUnit? {
        if idx == core.endIndex {
          return .None
        }
        return self.core[idx++]
      }
    }

    /// A position in a `String.UnicodeScalarView`
    public struct Index : BidirectionalIndexType, Comparable {
      public init(_ _position: Int, _ _core: _StringCore) {
        self._position = _position
        self._core = _core
      }

      /// Returns the next consecutive value after `self`.
      ///
      /// Requires: the next value is representable.
      public func successor() -> Index {
        var scratch = _ScratchGenerator(_core, _position)
        var decoder = UTF16()
        let (_, length) = decoder._decodeOne(&scratch)
        return Index(_position + length, _core)
      }

      /// Returns the previous consecutive value before `self`.
      ///
      /// Requires: the previous value is representable.
      public func predecessor() -> Index {
        var i = _position
        let codeUnit = _core[--i]
        if _slowPath((codeUnit >> 10) == 0b1101_11) {
          if i != 0 && (_core[i - 1] >> 10) == 0b1101_10 {
            --i
          }
        }
        return Index(i, _core)
      }

      /// The end index that for this view.
      internal var _viewStartIndex: Index {
        return Index(_core.startIndex, _core)
      }

      /// The end index that for this view.
      internal var _viewEndIndex: Index {
        return Index(_core.endIndex, _core)
      }

      var _position: Int
      var _core: _StringCore
    }

    /// The position of the first `UnicodeScalar` if the `String` is
    /// non-empty; identical to `endIndex` otherwise.
    public var startIndex: Index {
      return Index(_core.startIndex, _core)
    }

    /// The "past the end" position.
    ///
    /// `endIndex` is not a valid argument to `subscript`, and is always
    /// reachable from `startIndex` by zero or more applications of
    /// `successor()`.
    public var endIndex: Index {
      return Index(_core.endIndex, _core)
    }

    /// Access the element at `position`.
    ///
    /// Requires: `position` is a valid position in `self` and
    /// `position != endIndex`.
    public subscript(position: Index) -> UnicodeScalar {
      var scratch = _ScratchGenerator(_core, position._position)
      var decoder = UTF16()
      switch decoder.decode(&scratch) {
      case .Result(let us):
        return us
      case .EmptyInput:
        _sanityCheckFailure("can not subscript using an endIndex")
      case .Error:
        return UnicodeScalar(0xfffd)
      }
    }

    /// Access the elements delimited by the given half-open range of
    /// indices.
    ///
    /// - Complexity: O(1) unless bridging from Objective-C requires an
    ///   O(N) conversion.
    public subscript(r: Range<Index>) -> UnicodeScalarView {
      return UnicodeScalarView(
        _core[r.startIndex._position..<r.endIndex._position])
    }

    /// A type whose instances can produce the elements of this
    /// sequence, in order.
    public struct Generator : GeneratorType {
      init(_ _base: _StringCore) {
        if _base.hasContiguousStorage {
            self._baseSet = true
          if _base.isASCII {
            self._ascii = true
            self._asciiBase = UnsafeBufferPointer<UInt8>(
              start: UnsafePointer(_base._baseAddress),
              count: _base.count).generate()
          } else {
            self._ascii = false
            self._base = UnsafeBufferPointer<UInt16>(
              start: UnsafePointer(_base._baseAddress),
              count: _base.count).generate()
          }
        } else {
          self._ascii = false
          self._baseSet = false
          self._generator = _base.generate()
        }
      }

      /// Advance to the next element and return it, or `nil` if no next
      /// element exists.
      ///
      /// Requires: no preceding call to `self.next()` has returned
      /// `nil`.
      public mutating func next() -> UnicodeScalar? {
        var result: UnicodeDecodingResult
        if _baseSet {
          if _ascii {
            switch self._asciiBase.next() {
            case let x?:
              result = .Result(UnicodeScalar(x))
            case .None:
              result = .EmptyInput
            }
          } else {
            result = _decoder.decode(&(self._base!))
          }
        } else {
          result = _decoder.decode(&(self._generator!))
        }
        switch result {
        case .Result(let us):
          return us
        case .EmptyInput:
          return .None
        case .Error:
          return UnicodeScalar(0xfffd)
        }
      }
      var _decoder: UTF16 = UTF16()
      let _baseSet: Bool
      let _ascii: Bool
      var _asciiBase: UnsafeBufferPointerGenerator<UInt8>!
      var _base: UnsafeBufferPointerGenerator<UInt16>!
      var _generator: IndexingGenerator<_StringCore>!
    }

    /// Return a *generator* over the `UnicodeScalar`s that comprise
    /// this *sequence*.
    ///
    /// - Complexity: O(1)
    public func generate() -> Generator {
      return Generator(_core)
    }

    /// Returns a mirror that reflects `self`.
    public func getMirror() -> MirrorType {
      return _UnicodeScalarViewMirror(self)
    }

    public var description: String {
      return String(_core[startIndex._position..<endIndex._position])
    }

    public var debugDescription: String {
      return "StringUnicodeScalarView(\(self.description.debugDescription))"
    }

    var _core: _StringCore
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

extension String.UnicodeScalarView : ExtensibleCollectionType {
  /// Construct an empty instance.
  public init() {
    self = String.UnicodeScalarView(_StringCore())
  }
  /// Reserve enough space to store `n` ASCII characters.
  ///
  /// - Complexity: O(`n`)
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
  /// - Complexity: O(*length of result*)
  public mutating func extend<
    S : SequenceType where S.Generator.Element == UnicodeScalar
  >(newElements: S) {
    _core.extend(
      _lazyConcatenate(lazy(newElements).map { $0.utf16 })
    )
  }
}

extension String.UnicodeScalarView : RangeReplaceableCollectionType {

  /// Replace the given `subRange` of elements with `newElements`.
  ///
  /// Invalidates all indices with respect to `self`.
  ///
  /// - Complexity: O(`subRange.count()`) if `subRange.endIndex
  ///   == self.endIndex` and `isEmpty(newElements)`, O(N) otherwise.
  public mutating func replaceRange<
    C: CollectionType where C.Generator.Element == UnicodeScalar
  >(
    subRange: Range<Index>, with newElements: C
  ) {
    let rawSubRange = subRange.startIndex._position
      ..< subRange.endIndex._position
    let lazyUTF16 = _lazyConcatenate(lazy(newElements).map { $0.utf16 })
    _core.replaceRange(rawSubRange, with: lazyUTF16)
  }

  /// Insert `newElement` at index `i`.
  ///
  /// Invalidates all indices with respect to `self`.
  ///
  /// - Complexity: O(`self.count()`).
  public mutating func insert(newElement: UnicodeScalar, atIndex i: Index) {
    Swift.insert(&self, newElement, atIndex: i)
  }

  /// Insert `newElements` at index `i`
  ///
  /// Invalidates all indices with respect to `self`.
  ///
  /// - Complexity: O(`self.count() + newElements.count()`).
  public mutating func splice<
    S : CollectionType where S.Generator.Element == UnicodeScalar
  >(newElements: S, atIndex i: Index) {
    Swift.splice(&self, newElements, atIndex: i)
  }

  /// Remove the and return element at index `i`
  ///
  /// Invalidates all indices with respect to `self`.
  ///
  /// - Complexity: O(`self.count()`).
  public mutating func removeAtIndex(i: Index) -> UnicodeScalar {
    return Swift.removeAtIndex(&self, i)
  }

  /// Remove the indicated `subRange` of elements
  ///
  /// Invalidates all indices with respect to `self`.
  ///
  /// - Complexity: O(`self.count()`).
  public mutating func removeRange(subRange: Range<Index>) {
    Swift.removeRange(&self, subRange)
  }

  /// Remove all elements.
  ///
  /// Invalidates all indices with respect to `self`.
  ///
  /// - parameter keepCapacity: if `true`, prevents the release of
  ///   allocated storage, which can be a useful optimization
  ///   when `self` is going to be grown again.
  public mutating func removeAll(keepCapacity keepCapacity: Bool = false) {
    Swift.removeAll(&self, keepCapacity: keepCapacity)
  }
}

// Index conversions
extension String.UnicodeScalarIndex {
  /// Construct the position in `unicodeScalars` that corresponds exactly to
  /// `utf16Index`. If no such position exists, the result is `nil`.
  ///
  /// Requires: `utf16Index` is an element of
  /// `String(unicodeScalars).utf16.indices`.
  public init?(
    _ utf16Index: String.UTF16Index,
    within unicodeScalars: String.UnicodeScalarView
  ) {
    let utf16 = String.UTF16View(unicodeScalars._core)

    if utf16Index != utf16.startIndex
    && utf16Index != utf16.endIndex {
      _precondition(
        utf16Index >= utf16.startIndex
        && utf16Index <= utf16.endIndex,
        "Invalid String.UTF16Index for this UnicodeScalar view")

      // Detect positions that have no corresponding index.  Note that
      // we have to check before and after, because an unpaired
      // surrogate will be decoded as a single replacement character,
      // thus making the corresponding position valid.
      if UTF16.isTrailSurrogate(utf16[utf16Index])
        && UTF16.isLeadSurrogate(utf16[utf16Index.predecessor()]) {
        return nil
      }
    }
    self.init(utf16Index._offset, unicodeScalars._core)
  }

  /// Construct the position in `unicodeScalars` that corresponds exactly to
  /// `utf8Index`. If no such position exists, the result is `nil`.
  ///
  /// Requires: `utf8Index` is an element of
  /// `String(unicodeScalars).utf8.indices`.
  public init?(
    _ utf8Index: String.UTF8Index,
    within unicodeScalars: String.UnicodeScalarView
  ) {
    let core = unicodeScalars._core

    _precondition(
      utf8Index._coreIndex >= 0 && utf8Index._coreIndex <= core.endIndex,
      "Invalid String.UTF8Index for this UnicodeScalar view")

    // Detect positions that have no corresponding index.
    if !utf8Index._isOnUnicodeScalarBoundary {
      return nil
    }
    self.init(utf8Index._coreIndex, core)
  }

  /// Construct the position in `unicodeScalars` that corresponds
  /// exactly to `characterIndex`.
  ///
  /// Requires: `characterIndex` is an element of
  /// `String(unicodeScalars).indices`.
  public init(
    _ characterIndex: String.Index,
    within unicodeScalars: String.UnicodeScalarView
  ) {
    self.init(characterIndex._base._position, unicodeScalars._core)
  }

  /// Return the position in `utf8` that corresponds exactly
  /// to `self`.
  ///
  /// Requires: `self` is an element of `String(utf8)!.indices`.
  public func samePositionIn(utf8: String.UTF8View) -> String.UTF8View.Index {
    return String.UTF8View.Index(self, within: utf8)
  }

  /// Return the position in `utf16` that corresponds exactly
  /// to `self`.
  ///
  /// Requires: `self` is an element of `String(utf16)!.indices`.
  public func samePositionIn(
    utf16: String.UTF16View
  ) -> String.UTF16View.Index {
    return String.UTF16View.Index(self, within: utf16)
  }

  /// Return the position in `characters` that corresponds exactly
  /// to `self`, or if no such position exists, `nil`.
  ///
  /// Requires: `self` is an element of `characters.unicodeScalars.indices`.
  public func samePositionIn(characters: String) -> String.Index? {
    return String.Index(self, within: characters)
  }

  internal var _isOnGraphemeClusterBoundary: Bool {
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
  }
}
