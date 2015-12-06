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

extension StaticString {
  /// The value of `self` as a collection of [Unicode scalar values](http://www.unicode.org/glossary/#unicode_scalar_value).
  public var unicodeScalars: UnicodeScalarView {
    get {
      return UnicodeScalarView(self)
    }
  }

  /// Construct the `StaticString` corresponding to the given
  /// `UnicodeScalarView`.
  public init(_ unicodeScalars: UnicodeScalarView) {
    switch unicodeScalars._data {
    case let .Pointer(ptr, isASCII):
      self.init(start: ptr.baseAddress._rawValue, byteSize: ptr.count._builtinWordValue, isASCII: isASCII._value)
    case .Scalar(let scalar):
      self.init(unicodeScalar: unsafeBitCast(scalar.value, Int32.self)._value)
    }
  }

  /// Construct the `StaticString` corresponding to the given
  /// `UnicodeScalarView` slice.
  public init(_ unicodeScalars: Slice<UnicodeScalarView>) {
    UnicodeScalarView.Index._failEarlyRangeCheck2(
      unicodeScalars.startIndex, rangeEnd: unicodeScalars.endIndex,
      boundsStart: unicodeScalars._base.startIndex, boundsEnd: unicodeScalars._base.endIndex)
    switch unicodeScalars._base._data {
    case let .Pointer(ptr, isASCII):
      self.init(
        start: (ptr.baseAddress + unicodeScalars.startIndex._position)._rawValue,
        byteSize: (unicodeScalars.endIndex._position - unicodeScalars.startIndex._position)._builtinWordValue,
        isASCII: isASCII._value)
    case .Scalar(let scalar):
      if unicodeScalars.isEmpty {
        self.init()
      } else {
        self.init(unicodeScalar: unsafeBitCast(scalar.value, Int32.self)._value)
      }
    }
  }

  /// A collection of [Unicode scalar values](http://www.unicode.org/glossary/#unicode_scalar_value) that
  /// encode a `StaticString`.
  public struct UnicodeScalarView : CollectionType, _Reflectable,
    CustomStringConvertible, CustomDebugStringConvertible {
    enum Data {
      case Pointer(UnsafeBufferPointer<UInt8>, isASCII: Bool)
      case Scalar(UnicodeScalar)
    }

    let _data: Data

    init(_ _base: StaticString) {
      if _base.hasPointerRepresentation {
        let ptr = UnsafeBufferPointer(start: _base.utf8Start, count: Int(_base.byteSize))
        _data = .Pointer(ptr, isASCII: _base.isASCII)
      } else {
        _data = .Scalar(_base.unicodeScalar)
      }
    }

    /// A position in a `StaticString.UnicodeScalarView`.
    public struct Index : BidirectionalIndexType, Comparable {
      /// An index into the UTF-8 data of _base. If _base does not have a
      /// pointer representation, then a position of 0 is startIndex and 1 is
      /// endIndex.
      let _position: Int
      let _data: Data

      init(_ _position: Int, _ _data: Data) {
        self._position = _position
        self._data = _data
      }

      /// Returns the next consecutive value after `self`.
      ///
      /// - Requires: The next value is representable.
      @warn_unused_result
      public func successor() -> Index {
        switch _data {
        case .Pointer(let ptr, _):
          let count = Int(UTF8._numTrailingBytes(ptr[_position]))
          return Index(_position + count + 1, _data)
        case .Scalar:
          _precondition(_position == 0, "index points past StaticString end")
          return Index(1, _data)
        }
      }

      /// Returns the previous consecutive value before `self`.
      ///
      /// - Requires: The previous value is representable.
      @warn_unused_result
      public func predecessor() -> Index {
        _precondition(_position > 0, "index precedes StaticString start")
        var position = _position - 1
        if case .Pointer(let ptr, _) = _data {
          while UTF8.isContinuation(ptr[position]) {
            position -= 1
          }
        }
        return Index(position, _data)
      }
    }

    /// The position of the first `UnicodeScalar` if the `StaticString` is
    /// non-empty; identical to `endIndex` otherwise.
    public var startIndex: Index {
      return Index(0, _data)
    }

    /// The "past the end" position.
    ///
    /// `endIndex` is not a valid argument to `subscript`, and is always
    /// reachable from `startIndex` by zero or more applications of
    /// `successor()`.
    public var endIndex: Index {
      switch _data {
      case .Pointer(let ptr, _):
        return Index(ptr.endIndex, _data)
      case .Scalar:
        return Index(1, _data)
      }
    }

    /// Returns `true` iff `self` is empty.
    public var isEmpty: Bool {
      switch _data {
      case .Pointer(let ptr, _):
        return ptr.isEmpty
      case .Scalar:
        return false
      }
    }

    public subscript(position: Index) -> UnicodeScalar {
      switch _data {
      case let .Pointer(ptr, isASCII):
        _precondition(position._position < ptr.endIndex, "subscript: index cannot be endIndex")
        let start = ptr.baseAddress + position._position
        if isASCII {
          return UnicodeScalar(UInt32(start.memory))
        }
        let slice = UnsafeBufferPointer<UInt8>(start: start, count: ptr.endIndex - position._position)
        var gen = slice.generate()
        var decoder = UTF8()
        switch decoder.decode(&gen) {
        case .Result(let scalar): return scalar
        default:
          _sanityCheckFailure("StaticString UTF-8 decoding failed")
        }
      case .Scalar(let scalar):
        _precondition(position._position == 0, "subscript: index cannot be endIndex")
        return scalar
      }
    }

    /// A textual representation of `self`.
    public var description: String {
      return StaticString(self).stringValue
    }

    /// A textual representation of `self`, suitable for debugging.
    public var debugDescription: String {
      return "StaticString.UnicodeScalarView(\(StaticString(self).debugDescription))"
    }

    public func _getMirror() -> _MirrorType {
      return _reflect(StaticString(self).stringValue)
    }
  }
}

@warn_unused_result
public func ==(
  lhs: StaticString.UnicodeScalarView.Index,
  rhs: StaticString.UnicodeScalarView.Index
) -> Bool {
  return lhs._position == rhs._position
}

@warn_unused_result
public func <(
  lhs: StaticString.UnicodeScalarView.Index,
  rhs: StaticString.UnicodeScalarView.Index
) -> Bool {
  return lhs._position < rhs._position
}
