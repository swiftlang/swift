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
  public struct UnicodeScalarView : Sliceable, SequenceType {
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

      public func successor() -> Index {
        var scratch = _ScratchGenerator(_core, _position)
        var decoder = UTF16()
        let (result, length) = decoder._decodeOne(&scratch)
        return Index(_position + length, _core)
      }

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

    public var startIndex: Index {
      return Index(_core.startIndex, _core)
    }

    public var endIndex: Index {
      return Index(_core.endIndex, _core)
    }

    public subscript(i: Index) -> UnicodeScalar {
      var scratch = _ScratchGenerator(_core, i._position)
      var decoder = UTF16()
      switch decoder.decode(&scratch) {
      case .Result(let us):
        return us
      case .EmptyInput:
        _fatalError("can not subscript using an endIndex")
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

    public func generate() -> Generator {
      return Generator(_core.generate())
    }

    public func compare(other : UnicodeScalarView) -> Int {
      // Try to compare the string without decoding the UTF-16 string.
      var aIdx = self._core.startIndex
      var bIdx = other._core.startIndex
      var aEnd = self._core.endIndex
      var bEnd = other._core.endIndex

      // If this is not a contiguous UTF-16 then use the slow path.
      // TODO: when we fix rdar://16740011 we'll need to optimize the ascii
      // path.
      if ((self._core.elementShift != 1) |
         (other._core.elementShift != 1) |
         (!self._core.hasContiguousStorage) |
         (!other._core.hasContiguousStorage)) {
        // This is a cold path and inlining _compareUnicode may
        // prevent the inlining of the compare function.
        return _compareUnicode(other)
      }

      while true {
        if aIdx < aEnd {
          if bIdx < bEnd {
            let e1 = self._core._nthContiguous(aIdx)
            let e2 = other._core._nthContiguous(bIdx)

            // The range 0xD800 .. 0xDFFF is reserved for lead and trail
            // surrogates. In this code we are only comparing against the
            // lower bound because most interesting characters are in that
            // range. This is conservatively correct since the slow path is
            // handling the surrogates correctly.
            if _slowPath((e1 >= 0xD800) | (e2 >= 0xD800)) {
              // Use slow unicode comparator if
              // we found multi-byte scalar.
              return _compareUnicode(other)
            }

            if e1 < e2 {
              return -1
            }
            if e2 < e1 {
              return 1
            }
            aIdx++
            bIdx++
            continue // equivalent
          }
          return 1
        }
        if bIdx < bEnd {
          return -1
        }
        return 0
      }
    }

    @inline(never)
    func _compareUnicode(other : UnicodeScalarView) -> Int {
      var g1 = self.generate()
      var g2 = other.generate()
      while true {
        let e1_ = g1.next()
        let e2_ = g2.next()
        if let e1 = e1_ {
          if let e2 = e2_ {
            if e1 < e2 {
              return -1
            }
            if e2 < e1 {
              return 1
            }
            continue // equivalent
          }
          return 1
        }
        if e2_ != nil {
          return -1
        }
        return 0
      }
    }

    var _core: _StringCore
  }
}

extension String {
  public init(_ view: UnicodeScalarView) {
    self = String(view._core)
  }

  public var unicodeScalars : UnicodeScalarView {
    return UnicodeScalarView(_core)
  }
}
