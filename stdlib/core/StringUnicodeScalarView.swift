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

func ==(lhs: String.UnicodeScalarView.IndexType, rhs: String.UnicodeScalarView.IndexType) -> Bool {
  return lhs._position == rhs._position
}

extension String {
  struct UnicodeScalarView : Sliceable, Sequence {
    init(_ _base: _StringCore) {
      self._base = _base
    }

    struct ScratchGenerator : Generator {
      var base :_StringCore
      var idx : Int
      init(_ core : _StringCore, _ pos : Int) {
        idx = pos
        base = core
      }
      mutating func next() -> UTF16.CodeUnit? {
        return self.base[idx++]
      }
    }

    // FIXME: This index should probably become bidirectional, as UTF16
    // is traversable in either direction.
    struct IndexType : BidirectionalIndex {
      init(_ _position: Int, _ _base: _StringCore) {
        self._position = _position
        self._base = _base
      }

      func succ() -> IndexType {
        var scratch = ScratchGenerator(_base, _position)
        UTF16.decode(&scratch)
        return IndexType(scratch.idx, _base)
      }

      func pred() -> IndexType {
        var i = _position
        let codeUnit = self._base[--i]
        if codeUnit >= 0xD800 && codeUnit <= 0xE000 {
          --i
        }
        return IndexType(i, _base)
      }
      
      var _position: Int
      var _base: _StringCore
    }

    var startIndex: IndexType {
      return IndexType(_base.startIndex, _base)
    }
    
    var endIndex: IndexType {
      return IndexType(_base.endIndex, _base)
    }
    
    subscript(i: IndexType) -> UnicodeScalar {
      var scratch = ScratchGenerator(_base, i._position)
      return UTF16.decode(&scratch)!
    }

    func __slice__(start: IndexType, end: IndexType) -> UnicodeScalarView {
      return UnicodeScalarView(_base[start._position..end._position])
    }

    subscript(r: Range<IndexType>) -> UnicodeScalarView {
      return UnicodeScalarView(_base[r.startIndex._position..r.endIndex._position])
    }

    struct GeneratorType : Generator {
      init(_ _base: _StringCore.GeneratorType) {
        self._base = _base
      }

      mutating func next() -> UnicodeScalar? {
        return UTF16.decode(&self._base)
      }
      var _base: _StringCore.GeneratorType
    }
    
    func generate() -> GeneratorType {
      return GeneratorType(_base.generate())
    }

    @conversion
    func __conversion() -> String {
      return String(_base)
    }

    func compare(other : UnicodeScalarView) -> Int {
      // Try to compare the string without decoding
      // the UTF16 string.
      var aIdx = self._base.startIndex
      var bIdx = other._base.startIndex
      var aEnd = self._base.endIndex
      var bEnd = other._base.endIndex

      // If this is not a contiguous UTF-16 then use the slow path.
      // TODO: when we fix rdar://16740011 we'll need to optimize the ascii
      // path.
      if ((self._base.elementShift != 1) |
         (other._base.elementShift != 1) |
         (!self._base.hasContiguousStorage) |
         (!other._base.hasContiguousStorage)) {
        return _compareUnicode(other)
      }

      while true {
        if aIdx < aEnd {
          if bIdx < bEnd {
            let e1 = self._base._nthContiguous(aIdx)
            let e2 = other._base._nthContiguous(bIdx)

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
        if e2_ {
          return -1
        }
        return 0
      }
    }

    var _base: _StringCore
  }
}

extension String {
  func compare(other : String) -> Int {
    return(UnicodeScalarView(core).compare(UnicodeScalarView(other.core)))
  }
  var unicodeScalars : UnicodeScalarView {
    return UnicodeScalarView(core)
  }
}
