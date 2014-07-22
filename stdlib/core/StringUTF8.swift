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
      return _encodeSomeNonContiguousUTF16AsUTF8(i)
    }
  }

  /// Helper for `_encodeSomeUTF8`, above.  Handles the case where the
  /// storage is contiguous UTF-16.
  func _encodeSomeContiguousUTF16AsUTF8(i: Int) -> (Int, UTF8Chunk) {
    _sanityCheck(elementWidth == 2)
    _sanityCheck(!_baseAddress._isNull)

    let storage = UnsafeArray(start: startUTF16, length: self.count)
    return _transcodeSomeUTF16AsUTF8(storage, i)
  }

  /// Helper for `_encodeSomeUTF8`, above.  Handles the case where the
  /// storage is non-contiguous UTF-16.
  func _encodeSomeNonContiguousUTF16AsUTF8(i: Int) -> (Int, UTF8Chunk) {
    _sanityCheck(elementWidth == 2)
    _sanityCheck(_baseAddress._isNull)

    let storage = _CollectionOf<Int, UInt16>(
      startIndex: 0, endIndex: self.count) {
      (i: Int) -> UInt16 in
      return _cocoaStringSubscript(target: self, position: i)
    }
    return _transcodeSomeUTF16AsUTF8(storage, i)
  }
}

extension String {
  public struct UTF8View : CollectionType, Reflectable {
    let _core: _StringCore
    
    init(_ _core: _StringCore) {
      self._core = _core
    }

    public struct Index : ForwardIndexType {
      init(_ _core: _StringCore, _ _coreIndex: Int, 
           _ _buffer: _StringCore.UTF8Chunk) {
        self._core = _core
        self._coreIndex = _coreIndex
        self._buffer = _buffer
        _sanityCheck(_coreIndex >= 0)
        _sanityCheck(_coreIndex <= _core.count)
      }
      
      public func successor() -> Index {
        let newBuffer0 = (_buffer >> 8) | (
          0xFF << numericCast((sizeofValue(_buffer) &- 1) &* 8)
        )
        if _fastPath(newBuffer0 != ~0) {
          return Index(_core, _coreIndex, newBuffer0)
        }
        if _fastPath(_coreIndex != _core.endIndex) {
          let (newCoreIndex, newBuffer1) = _core._encodeSomeUTF8(_coreIndex)
          _sanityCheck(newCoreIndex > _coreIndex)
          return Index(_core, newCoreIndex, newBuffer1)
        }
        _precondition(_buffer & 0xFF != 0xFE, "can not increment endIndex")
        return Index(_core, _coreIndex, ~1)
      }
      
      let _core: _StringCore
      let _coreIndex: Int
      let _buffer: _StringCore.UTF8Chunk
    }
  
    public var startIndex: Index {
      if _fastPath(_core.count != 0) {
        let (coreIndex, buffer) = _core._encodeSomeUTF8(0)
        return Index(_core, coreIndex, buffer)
      }
      return endIndex
    }
    
    public var endIndex: Index {
      return Index(_core, _core.endIndex, ~1)
    }

    public subscript(i: Index) -> UTF8.CodeUnit {
      let result: UTF8.CodeUnit = numericCast(i._buffer & 0xFF)
      _precondition(result != 0xFE, "can not subscript using endIndex")
      return result
    }

    public func generate() -> IndexingGenerator<UTF8View> {
      return IndexingGenerator(self)
    }
    
    public func getMirror() -> MirrorType {
      return _UTF8ViewMirror(self)
    }
  }

  public var utf8: UTF8View {
    return UTF8View(self.core)
  }

  public var _contiguousUTF8: UnsafeMutablePointer<UTF8.CodeUnit> {
    return core.elementWidth == 1 ? core.startASCII : nil
  }

  public var nulTerminatedUTF8: ContiguousArray<UTF8.CodeUnit> {
    var result = ContiguousArray<UTF8.CodeUnit>()
    result.reserveCapacity(countElements(utf8) + 1)
    result += utf8
    result.append(0)
    return result
  }
}

public
func == (lhs: String.UTF8View.Index, rhs: String.UTF8View.Index) -> Bool {
  return lhs._coreIndex == rhs._coreIndex && lhs._buffer == rhs._buffer
}
