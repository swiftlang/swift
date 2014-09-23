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
  public struct UnicodeScalarView : Sliceable, SequenceType, Reflectable {
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
        let (result, length) = decoder._decodeOne(&scratch)
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

    public subscript(r: Range<Index>) -> UnicodeScalarView {
      return UnicodeScalarView(
        _core[r.startIndex._position..<r.endIndex._position])
    }

    public struct Generator : GeneratorType {
      init(_ _base: _StringCore.Generator) {
        self._base = _base
      }

      /// Advance to the next element and return it, or `nil` if no next
      /// element exists.
      ///
      /// Requires: no preceding call to `self.next()` has returned
      /// `nil`.
      public mutating func next() -> UnicodeScalar? {
        switch _decoder.decode(&self._base) {
        case .Result(let us):
          return us
        case .EmptyInput:
          return .None
        case .Error:
          return UnicodeScalar(0xfffd)
        }
      }
      var _decoder: UTF16 = UTF16()
      var _base: _StringCore.Generator
    }

    /// Return a *generator* over the `UnicodeScalar`\ s that comprise
    /// this *sequence*.
    ///
    /// Complexity: O(1)
    public func generate() -> Generator {
      return Generator(_core.generate())
    }

    public func getMirror() -> MirrorType {
      return _UnicodeScalarViewMirror(self)
    }

    var _core: _StringCore
  }
}

extension String {
  public init(_ view: UnicodeScalarView) {
    self = String(view._core)
  }

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
  public init() {
    self = String.UnicodeScalarView(_StringCore())
  }
  /// Reserve enough space to store `n` ASCII characters.
  ///
  /// Complexity: O(`n`)
  public mutating func reserveCapacity(n: Int) {
    _core.reserveCapacity(n)
  }
  /// Append `x` to `self`.
  ///
  /// Complexity: amortized O(1).
  public mutating func append(x: UnicodeScalar) {
    _core.append(x)
  }
  /// Append the elements of `newElements` to `self`.
  ///
  /// Complexity: O(*length of result*) 
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
  /// Complexity: O(\ `countElements(subRange)`\ ) if `subRange.endIndex
  /// == self.endIndex` and `isEmpty(newElements)`\ , O(N) otherwise.
  public mutating func replaceRange<
    C: CollectionType where C.Generator.Element == UnicodeScalar
  >(
    subRange: Range<Index>, with newElements: C
  ) {
    _core.replaceRange(
      subRange.startIndex._position
      ..< subRange.endIndex._position,
      with:
        _lazyConcatenate(lazy(newElements).map { $0.utf16 })
    )
  }

  /// Insert `newElement` at index `i`.
  ///
  /// Invalidates all indices with respect to `self`.
  ///
  /// Complexity: O(\ `countElements(self)`\ ).
  public mutating func insert(newElement: UnicodeScalar, atIndex i: Index) {
    Swift.insert(&self, newElement, atIndex: i)
  }
  
  /// Insert `newElements` at index `i`
  ///
  /// Invalidates all indices with respect to `self`.
  ///
  /// Complexity: O(\ `countElements(self) + countElements(newElements)`\ ).
  public mutating func splice<
    S : CollectionType where S.Generator.Element == UnicodeScalar
  >(newElements: S, atIndex i: Index) {
    Swift.splice(&self, newElements, atIndex: i)
  }

  /// Remove the and return element at index `i`
  ///
  /// Invalidates all indices with respect to `self`.
  ///
  /// Complexity: O(\ `countElements(self)`\ ).
  public mutating func removeAtIndex(i: Index) -> UnicodeScalar {
    return Swift.removeAtIndex(&self, i)
  }
  
  /// Remove the indicated `subRange` of elements
  ///
  /// Invalidates all indices with respect to `self`.
  ///
  /// Complexity: O(\ `countElements(self)`\ ).
  public mutating func removeRange(subRange: Range<Index>) {
    Swift.removeRange(&self, subRange)
  }

  /// Remove all elements.
  ///
  /// Invalidates all indices with respect to `self`.
  ///
  /// :param: `keepCapacity`, if `true`, prevents the release of
  ////   allocated storage, which can be a useful optimization
  ///    when `self` is going to be grown again.
  public mutating func removeAll(keepCapacity: Bool = false) {
    Swift.removeAll(&self, keepCapacity: keepCapacity)
  }
}
