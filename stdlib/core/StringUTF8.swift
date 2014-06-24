//===--- StringUTF8.swift - A UTF8 view of _StringCore ---------------------===//
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
//  Native UTF16, and Opaque Cocoa.  Expose each of these as UTF8 in a
//  way that will hopefully be efficient to traverse
//
//===----------------------------------------------------------------------===//


extension _StringCore {
  // An integral type that holds a chunk of UTF8, starting in its low
  // byte
  typealias UTF8Chunk = UInt64
  
  /// Encode text starting at i as UTF8.  Returns a pair whose first
  /// element is the index of the text following whatever got encoded,
  /// and the second element contains the encoded UTF8 starting in its
  /// low byte.  Any unused high bytes in the result will be set to
  /// 0xFF.
  func _encodeSomeUTF8(i: Int) -> (Int, UTF8Chunk) {
    _sanityCheck(i <= count)
    
    if _fastPath(elementWidth == 1) {
      // How many UTF16 code units might we use before we've filled up
      // our UTF8Chunk with UTF8 code units?
      let utf16Count = min(sizeof(UTF8Chunk.self), count - i)
      
      var result: UTF8Chunk = ~0 // start with all bits set
      
      _memcpy(
        dest: UnsafePointer(Builtin.addressof(&result)), 
        src: UnsafePointer(startASCII + i), 
        size: numericCast(utf16Count))
      
      return (i + utf16Count, result)
    }
    else {
      return _encodeSomeUTF16AsUTF8(i)
    }
  }

  /// Helper for _encodeSomeUTF8, above.  Handles the case where we
  /// don't have contiguous ASCII storage.
  func _encodeSomeUTF16AsUTF8(i: Int) -> (Int, UTF8Chunk) {
    _sanityCheck(elementWidth == 2)

    if _fastPath(!_baseAddress._isNull) {
      let utf16Count = self.count
      let utf8Max = sizeof(UTF8Chunk.self)
      var result: UTF8Chunk = 0
      var utf8Count = 0
      var nextIndex = i
      while (nextIndex < utf16Count && utf8Count != utf8Max) {
        let u = UInt(startUTF16[nextIndex])
        let shift = UTF8Chunk(utf8Count * 8)
        var utf16Length = 1

        if _fastPath(u <= 0x7f) {
          result |= UTF8Chunk(u) << shift
          ++utf8Count
        }
        else {
          var scalarUtf8Length: Int
          var r: UInt
          if _fastPath((u >> 11) != 0b1101_1) {
            // Neither high-surrogate, nor low-surrogate -- sequence of 1 code
            // unit, decoding is trivial.
            if u < 0x800 {
              r = 0b10__00_0000__110__0_0000
              r |= u >> 6
              r |= (u & 0b11_1111) << 8
              scalarUtf8Length = 2
            }
            else {
              r = 0b10__00_0000__10__00_0000__1110__0000
              r |= u >> 12
              r |= ((u >> 6) & 0b11_1111) << 8
              r |= (u        & 0b11_1111) << 16
              scalarUtf8Length = 3
            }
          }
          else {
            var unit0 = u
            if _slowPath((unit0 >> 10) == 0b1101_11) {
              // `unit0` is a low-surrogate.  We have an ill-formed sequence.
              // Replace it with U+FFFD.
              r = 0xbdbfef
              scalarUtf8Length = 3
            } else if _slowPath(nextIndex + 1 == utf16Count) {
              // We have seen a high-surrogate and EOF, so we have an
              // ill-formed sequence.  Replace it with U+FFFD.
              r = 0xbdbfef
              scalarUtf8Length = 3
            } else {
              let unit1 = UInt(startUTF16[nextIndex + 1])
              if _fastPath((unit1 >> 10) == 0b1101_11) {
                // `unit1` is a low-surrogate.  We have a well-formed surrogate
                // pair.
                let v = 0x10000 + (((unit0 & 0x03ff) << 10) | (unit1 & 0x03ff))

                r = 0b10__00_0000__10__00_0000__10__00_0000__1111_0__000
                r |= v >> 18
                r |= ((v >> 12) & 0b11_1111) << 8
                r |= ((v >> 6) & 0b11_1111) << 16
                r |= (v        & 0b11_1111) << 24
                scalarUtf8Length = 4
                utf16Length = 2
              } else {
                // Otherwise, we have an ill-formed sequence.  Replace it with
                // U+FFFD.
                r = 0xbdbfef
                scalarUtf8Length = 3
              }
            }
          }
          // Don't overrun the buffer
          if utf8Count + scalarUtf8Length > utf8Max {
            break
          }
          result |= numericCast(r) << shift
          utf8Count += scalarUtf8Length
        }
        nextIndex += utf16Length
      }
      // FIXME: Annoying check, courtesy of <rdar://problem/16740169>
      if utf8Count < sizeofValue(result) {
        result |= ~0 << numericCast(utf8Count * 8)
      }
      return (nextIndex, result)
    }
    else {
      return _cocoaStringEncodeSomeUTF8(target: self, position: i)
    }
  }
}

extension String {
  @public struct UTF8View : Collection {
    let _core: _StringCore
    
    init(_ _core: _StringCore) {
      self._core = _core
    }

    @public struct Index : ForwardIndex {
      init(_ _core: _StringCore, _ _coreIndex: Int, 
           _ _buffer: _StringCore.UTF8Chunk) {
        self._core = _core
        self._coreIndex = _coreIndex
        self._buffer = _buffer
        _sanityCheck(_coreIndex >= 0)
        _sanityCheck(_coreIndex <= _core.count)
      }
      
      @public func successor() -> Index {
        let newBuffer0 = (_buffer >> 8) | (
          0xFF << numericCast((sizeofValue(_buffer) - 1) * 8)
        )
        if _fastPath(newBuffer0 != ~0) {
          return Index(_core, _coreIndex, newBuffer0)
        }
        if _fastPath(_coreIndex != _core.endIndex) {
          let (newCoreIndex, newBuffer1) = _core._encodeSomeUTF8(_coreIndex)
          _sanityCheck(newCoreIndex > _coreIndex)
          return Index(_core, newCoreIndex, newBuffer1)
        }
        return Index(_core, _coreIndex, ~0)
      }
      
      let _core: _StringCore
      let _coreIndex: Int
      let _buffer: _StringCore.UTF8Chunk
    }
  
    @public var startIndex: Index {
      if _fastPath(_core.count != 0) {
        let (coreIndex, buffer) = _core._encodeSomeUTF8(0)
        return Index(_core, coreIndex, buffer)
      }
      return endIndex
    }
    
    @public var endIndex: Index {
      return Index(_core, _core.endIndex, ~0)
    }

    @public subscript(i: Index) -> UTF8.CodeUnit {
      return numericCast(i._buffer & 0xFF)
    }

    @public func generate() -> IndexingGenerator<UTF8View> {
      return IndexingGenerator(self)
    }
  }

  @public var utf8: UTF8View {
    return UTF8View(self.core)
  }

  var _contiguousUTF8: UnsafePointer<UTF8.CodeUnit> {
    return core.elementWidth == 1 ? core.startASCII : nil
  }

  @public var nulTerminatedUTF8: ContiguousArray<UTF8.CodeUnit> {
    var result = ContiguousArray<UTF8.CodeUnit>()
    result.reserveCapacity(countElements(utf8) + 1)
    result += utf8
    result += 0
    return result
  }
}

@public
func == (lhs: String.UTF8View.Index, rhs: String.UTF8View.Index) -> Bool {
  return lhs._coreIndex == rhs._coreIndex && lhs._buffer == rhs._buffer
}
