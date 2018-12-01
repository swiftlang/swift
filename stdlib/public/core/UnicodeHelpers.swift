//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

//
// Low-level helper functions and utilities for interpreting Unicode
//

internal let _leadingSurrogateBias: UInt16 = 0xd800
internal let _trailingSurrogateBias: UInt16 = 0xdc00
internal let _surrogateMask: UInt16 = 0xfc00

@inline(__always)
internal func _isTrailingSurrogate(_ cu: UInt16) -> Bool {
  return cu & _surrogateMask == _trailingSurrogateBias
}
@inline(__always)
@usableFromInline
internal func _isLeadingSurrogate(_ cu: UInt16) -> Bool {
  return cu & _surrogateMask == _leadingSurrogateBias
}
@inline(__always)
internal func _isSurrogate(_ cu: UInt16) -> Bool {
  // TODO(String micro-performance): check codegen
  return _isLeadingSurrogate(cu) || _isTrailingSurrogate(cu)
}

@inlinable @inline(__always)
internal func _isASCII(_ x: UInt8) -> Bool {
  return x & 0b1000_0000 == 0
}

@inlinable
@inline(__always)
internal func _decodeUTF8(_ x: UInt8) -> Unicode.Scalar {
  _internalInvariant(_isASCII(x))
  return Unicode.Scalar(_unchecked: UInt32(x))
}

@inlinable
@inline(__always)
internal func _decodeUTF8(_ x: UInt8, _ y: UInt8) -> Unicode.Scalar {
  _internalInvariant(_utf8ScalarLength(x) == 2)
  _internalInvariant(_isContinuation(y))
  let x = UInt32(x)
  let value = ((x & 0b0001_1111) &<< 6) | _continuationPayload(y)
  return Unicode.Scalar(_unchecked: value)
}

@inlinable
@inline(__always)
internal func _decodeUTF8(
  _ x: UInt8, _ y: UInt8, _ z: UInt8
) -> Unicode.Scalar {
  _internalInvariant(_utf8ScalarLength(x) == 3)
  _internalInvariant(_isContinuation(y) && _isContinuation(z))
  let x = UInt32(x)
  let value = ((x & 0b0000_1111) &<< 12)
            | (_continuationPayload(y) &<< 6)
            | _continuationPayload(z)
  return Unicode.Scalar(_unchecked: value)
}

@inlinable
@inline(__always)
internal func _decodeUTF8(
  _ x: UInt8, _ y: UInt8, _ z: UInt8, _ w: UInt8
) -> Unicode.Scalar {
  _internalInvariant(_utf8ScalarLength(x) == 4)
  _internalInvariant(
    _isContinuation(y) && _isContinuation(z) && _isContinuation(w))
  let x = UInt32(x)
  let value = ((x & 0b0000_1111) &<< 18)
            | (_continuationPayload(y) &<< 12)
            | (_continuationPayload(z) &<< 6)
            | _continuationPayload(w)
  return Unicode.Scalar(_unchecked: value)
}

@inlinable
internal func _decodeScalar(
  _ utf16: UnsafeBufferPointer<UInt16>, startingAt i: Int
) -> (Unicode.Scalar, scalarLength: Int) {
  let high = utf16[i]
  if i + 1 >= utf16.count {
    return (Unicode.Scalar(_unchecked: UInt32(high)), 1)
  }
  
  if !_isLeadingSurrogate(high) {
    return (Unicode.Scalar(_unchecked: UInt32(high)), 1)
  }
  
  let low = utf16[i+1]
  return (Unicode.Scalar(_unchecked: _decodeSurrogatePair(leading: high, trailing: low)), 2)
}

@inlinable
internal func _decodeScalar(
  _ utf8: UnsafeBufferPointer<UInt8>, startingAt i: Int
) -> (Unicode.Scalar, scalarLength: Int) {
  let cu0 = utf8[_unchecked: i]
  let len = _utf8ScalarLength(cu0)
  switch  len {
  case 1: return (_decodeUTF8(cu0), len)
  case 2: return (_decodeUTF8(cu0, utf8[_unchecked: i &+ 1]), len)
  case 3: return (_decodeUTF8(
    cu0, utf8[_unchecked: i &+ 1], utf8[_unchecked: i &+ 2]), len)
  case 4:
    return (_decodeUTF8(
      cu0,
      utf8[_unchecked: i &+ 1],
      utf8[_unchecked: i &+ 2],
      utf8[_unchecked: i &+ 3]),
    len)
  default: Builtin.unreachable()
  }
}

@inlinable
internal func _decodeScalar(
  _ utf8: UnsafeBufferPointer<UInt8>, endingAt i: Int
) -> (Unicode.Scalar, scalarLength: Int) {
  let len = _utf8ScalarLength(utf8, endingAt: i)
  let (scalar, scalarLen) = _decodeScalar(utf8, startingAt: i &- len)
  _internalInvariant(len == scalarLen)
  return (scalar, len)
}

@inlinable @inline(__always)
internal func _utf8ScalarLength(_ x: UInt8) -> Int {
  _internalInvariant(!_isContinuation(x))
  if _isASCII(x) { return 1 }
  // TODO(String micro-performance): check codegen
  return (~x).leadingZeroBitCount
}

@inlinable @inline(__always)
internal func _utf8ScalarLength(
  _ utf8: UnsafeBufferPointer<UInt8>, endingAt i: Int
  ) -> Int {
  var len = 1
  while _isContinuation(utf8[_unchecked: i &- len]) {
    len &+= 1
  }
  _internalInvariant(len == _utf8ScalarLength(utf8[i &- len]))
  return len
}

@inlinable @inline(__always)
internal func _isContinuation(_ x: UInt8) -> Bool {
  return x & 0b1100_0000 == 0b1000_0000
}

@inlinable
@inline(__always)
internal func _continuationPayload(_ x: UInt8) -> UInt32 {
  return UInt32(x & 0x3F)
}

@inline(__always)
@usableFromInline
internal func _decodeSurrogatePair(
  leading high: UInt16, trailing low: UInt16
) -> UInt32 {
  _internalInvariant(_isLeadingSurrogate(high) && _isTrailingSurrogate(low))
  let hi10: UInt32 = UInt32(high) &- UInt32(_leadingSurrogateBias)
  _internalInvariant(hi10 < 1<<10, "I said high 10. Not high, like, 20 or something")
  let lo10: UInt32 = UInt32(low) &- UInt32(_trailingSurrogateBias)
  _internalInvariant(lo10 < 1<<10, "I said low 10. Not low, like, 20 or something")

  return ((hi10 &<< 10) | lo10) &+ 0x1_00_00
}

@inline(__always)
internal func _numUTF8CodeUnits(_ scalar: Unicode.Scalar) -> Int {
  switch scalar.value {
    case 0..<0x80: return 1
    case 0x80..<0x0800: return 2
    case 0x0800..<0x1_0000: return 3
    default: return 4
  }
}
@inline(__always)
internal func _numUTF16CodeUnits(_ scalar: Unicode.Scalar) -> Int {
  return scalar.value <= UInt16.max ? 1 : 2
}

@inlinable @inline(__always)
internal func _scalarAlign(
  _ utf8: UnsafeBufferPointer<UInt8>, _ idx: Int
) -> Int {
  var i = idx
  while _slowPath(_isContinuation(utf8[_unchecked: i])) {
    i &-= 1
    _internalInvariant(i >= 0,
      "Malformed contents: starts with continuation byte")
  }
  return i
}

//
// Scalar helpers
//
extension _StringGuts {
  @inlinable
  @inline(__always) // fast-path: fold common fastUTF8 check
  internal func scalarAlign(_ idx: Index) -> Index {
    // TODO(String performance): isASCII check

    if _slowPath(idx.transcodedOffset != 0 || idx.encodedOffset == 0) {
      // Transcoded indices are already scalar aligned
      return String.Index(encodedOffset: idx.encodedOffset)
    }
    if _slowPath(self.isForeign) {
      return foreignScalarAlign(idx)
    }

    return self.withFastUTF8 { utf8 in
      let i = _scalarAlign(utf8, idx.encodedOffset)

      // If no alignment is performed, keep grapheme cache
      if i == idx.encodedOffset {
        return idx
      }

      return Index(encodedOffset: i)
    }
  }

  @inlinable
  internal func fastUTF8ScalarLength(startingAt i: Int) -> Int {
    _internalInvariant(isFastUTF8)
    let len = _utf8ScalarLength(self.withFastUTF8 { $0[i] })
    _internalInvariant((1...4) ~= len)
    return len
  }

  @inlinable
  internal func fastUTF8ScalarLength(endingAt i: Int) -> Int {
    _internalInvariant(isFastUTF8)

    return self.withFastUTF8 { utf8 in
      _internalInvariant(i == utf8.count || !_isContinuation(utf8[i]))
      var len = 1
      while _isContinuation(utf8[i &- len]) {
        _internalInvariant(i &- len > 0)
        len += 1
      }
      _internalInvariant(len <= 4)
      return len
    }
  }

  @inlinable
  internal func fastUTF8Scalar(startingAt i: Int) -> Unicode.Scalar {
    _internalInvariant(isFastUTF8)
    return self.withFastUTF8 { _decodeScalar($0, startingAt: i).0 }
  }

  @usableFromInline
  @_effects(releasenone)
  internal func isOnUnicodeScalarBoundary(_ i: String.Index) -> Bool {
    // TODO(String micro-performance): check isASCII

    // Beginning and end are always scalar aligned; mid-scalar never is
    guard i.transcodedOffset == 0 else { return false }
    if i == self.startIndex || i == self.endIndex { return true }

    if _fastPath(isFastUTF8) {
      return self.withFastUTF8 { return !_isContinuation($0[i.encodedOffset]) }
    }

    return i == foreignScalarAlign(i)
  }
}

//
// Error-correcting helpers (U+FFFD for unpaired surrogates) for accessing
// contents of foreign strings
//
extension _StringGuts {
  @_effects(releasenone)
  private func _getForeignCodeUnit(at i: Int) -> UInt16 {
#if _runtime(_ObjC)
    // Currently, foreign  means NSString
    return _cocoaStringSubscript(_object.cocoaObject, i)
#else
  fatalError("No foreign strings on Linux in this version of Swift")
#endif
  }

  @usableFromInline
  @_effects(releasenone)
  internal func foreignErrorCorrectedScalar(
    startingAt idx: String.Index
  ) -> (Unicode.Scalar, scalarLength: Int) {
    _internalInvariant(idx.transcodedOffset == 0)
    _internalInvariant(idx.encodedOffset < self.count)

    let start = idx.encodedOffset
    let leading = _getForeignCodeUnit(at: start)

    if _fastPath(!_isSurrogate(leading)) {
      return (Unicode.Scalar(_unchecked: UInt32(leading)), 1)
    }

    // Validate foreign strings on-read: trailing surrogates are invalid,
    // leading need following trailing
    //
    // TODO(String performance): Consider having a valid performance flag
    // available to check, and assert it's not set in the condition here.
    let nextOffset = start &+ 1
    if _slowPath(_isTrailingSurrogate(leading) || nextOffset == self.count) {
      return (Unicode.Scalar._replacementCharacter, 1)
    }
    let trailing = _getForeignCodeUnit(at: nextOffset)
    if _slowPath(!_isTrailingSurrogate(trailing)) {
      return (Unicode.Scalar._replacementCharacter, 1)
    }

    return (Unicode.Scalar(
      _unchecked: _decodeSurrogatePair(leading: leading, trailing: trailing)),
      2)
  }

  @_effects(releasenone)
  internal func foreignErrorCorrectedScalar(
    endingAt idx: String.Index
  ) -> (Unicode.Scalar, scalarLength: Int) {
    _internalInvariant(idx.transcodedOffset == 0)
    _internalInvariant(idx.encodedOffset <= self.count)
    _internalInvariant(idx.encodedOffset > 0)

    let end = idx.encodedOffset
    let trailing = _getForeignCodeUnit(at: end &- 1)
    if _fastPath(!_isSurrogate(trailing)) {
      return (Unicode.Scalar(_unchecked: UInt32(trailing)), 1)
    }

    // Validate foreign strings on-read: trailing surrogates are invalid,
    // leading need following trailing
    //
    // TODO(String performance): Consider having a valid performance flag
    // available to check, and assert it's not set in the condition here.
    let priorOffset = end &- 2
    if _slowPath(_isLeadingSurrogate(trailing) || priorOffset < 0) {
      return (Unicode.Scalar._replacementCharacter, 1)
    }
    let leading = _getForeignCodeUnit(at: priorOffset)
    if _slowPath(!_isLeadingSurrogate(leading)) {
      return (Unicode.Scalar._replacementCharacter, 1)
    }

    return (Unicode.Scalar(
      _unchecked: _decodeSurrogatePair(leading: leading, trailing: trailing)),
      2)
  }

  @_effects(releasenone)
  internal func foreignErrorCorrectedUTF16CodeUnit(
    at idx: String.Index
  ) -> UInt16 {
    _internalInvariant(idx.transcodedOffset == 0)
    _internalInvariant(idx.encodedOffset < self.count)

    let start = idx.encodedOffset
    let cu = _getForeignCodeUnit(at: start)
    if _fastPath(!_isSurrogate(cu)) {
      return cu
    }

    // Validate foreign strings on-read: trailing surrogates are invalid,
    // leading need following trailing
    //
    // TODO(String performance): Consider having a valid performance flag
    // available to check, and assert it's not set in the condition here.
    if _isLeadingSurrogate(cu) {
      let nextOffset = start &+ 1
      guard nextOffset < self.count,
            _isTrailingSurrogate(_getForeignCodeUnit(at: nextOffset))
      else { return UTF16._replacementCodeUnit }
    } else {
      let priorOffset = start &- 1
      guard priorOffset >= 0,
            _isLeadingSurrogate(_getForeignCodeUnit(at: priorOffset))
      else { return UTF16._replacementCodeUnit }
    }

    return cu
  }

  @usableFromInline @inline(never) // slow-path
  @_effects(releasenone)
  internal func foreignScalarAlign(_ idx: Index) -> Index {
    _internalInvariant(idx.encodedOffset < self.count)

    let ecCU = foreignErrorCorrectedUTF16CodeUnit(at: idx)
    if _fastPath(!_isTrailingSurrogate(ecCU)) {
      return idx
    }
    _internalInvariant(idx.encodedOffset > 0,
      "Error-correction shouldn't give trailing surrogate at position zero")
    return String.Index(encodedOffset: idx.encodedOffset &- 1)
  }

  @usableFromInline @inline(never)
  @_effects(releasenone)
  internal func foreignErrorCorrectedGrapheme(
    startingAt start: Int, endingAt end: Int
  ) -> Character {
#if _runtime(_ObjC)
    _internalInvariant(self.isForeign)

    // Both a fast-path for single-code-unit graphemes and validation:
    //   ICU treats isolated surrogates as isolated graphemes
    let count = end &- start
    if start &- end == 1 {
      return Character(String(self.foreignErrorCorrectedScalar(
        startingAt: String.Index(encodedOffset: start)
      ).0))
    }

    // TODO(String performance): Stack buffer if small enough
    var cus = Array<UInt16>(repeating: 0, count: count)
    cus.withUnsafeMutableBufferPointer {
      _cocoaStringCopyCharacters(
        from: self._object.cocoaObject,
        range: start..<end,
        into: $0.baseAddress._unsafelyUnwrappedUnchecked)
    }
    return cus.withUnsafeBufferPointer {
      return Character(String._uncheckedFromUTF16($0))
    }
#else
    fatalError("No foreign strings on Linux in this version of Swift")
#endif
  }
}

// Higher level aggregate operations. These should only be called when the
// result is the sole operation done by a caller, otherwise it's always more
// efficient to use `withFastUTF8` in the caller.
extension _StringGuts {
  @inlinable @inline(__always)
  internal func errorCorrectedScalar(
    startingAt i: Int
  ) -> (Unicode.Scalar, scalarLength: Int) {
    if _fastPath(isFastUTF8) {
      return withFastUTF8 { _decodeScalar($0, startingAt: i) }
    }
    return foreignErrorCorrectedScalar(
      startingAt: String.Index(encodedOffset: i))
  }
  @inlinable @inline(__always)
  internal func errorCorrectedCharacter(
    startingAt start: Int, endingAt end: Int
  ) -> Character {
    if _fastPath(isFastUTF8) {
      return withFastUTF8(range: start..<end) { utf8 in
        return Character(unchecked: String._uncheckedFromUTF8(utf8))
      }
    }

    return foreignErrorCorrectedGrapheme(startingAt: start, endingAt: end)
  }
}
