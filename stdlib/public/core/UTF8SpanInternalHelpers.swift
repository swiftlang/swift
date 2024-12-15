/*

 Additional helpers build on stdlibDuplicates.swift

 */

// TODO: Should we update our unicode helpers file to call these instead?

// import Builtin

extension UnsafeRawPointer {
  // @_alwaysEmitIntoClient
  internal func _loadByte(_ i: Int) -> UInt8 {
    _internalInvariant(i >= 0)
    return (self+i).loadUnaligned(as: UInt8.self)
  }

  // @_alwaysEmitIntoClient
  internal func _isUTF8Continuation(_ i: Int) -> Bool {
    UTF8.isContinuation(_loadByte(i))
  }

  // @_alwaysEmitIntoClient
  internal func _isScalarAligned(_ i: Int) -> Bool {
    _internalInvariant(i >= 0)
    return !_isUTF8Continuation(i)
  }

  // @_alwaysEmitIntoClient
  internal func _scalarLength(startingAt i: Int) -> Int {
    _utf8ScalarLength(_loadByte(i))
  }

  // NOTE: Adaptation of `_decodeScalar` to work on URP
//  @_alwaysEmitIntoClient
  internal func _decodeScalar(
    startingAt i: Int
  ) -> (Unicode.Scalar, nextScalarStart: Int) {
    let cu0 = _loadByte(i)
    let len = _utf8ScalarLength(cu0)
    let next = len &+ i
    switch  len {
    case 1: return (_decodeUTF8(cu0), next)
    case 2: return (_decodeUTF8(cu0, _loadByte(i &+ 1)), next)
    case 3: return (
      _decodeUTF8(cu0, _loadByte(i &+ 1), _loadByte(i &+ 2)), next
    )
    case 4:
      return (
        _decodeUTF8(
          cu0, _loadByte(i &+ 1), _loadByte(i &+ 2), _loadByte(i &+ 3)
        ),
        next
      )
    default: Builtin.unreachable()
    }
  }

  // @_alwaysEmitIntoClient
  internal func _decodeScalar(
    endingAt i: Int
  ) -> (Unicode.Scalar, previousScalarStart: Int) {
    // TODO: no need to double load the bytes...
    let start = _previousScalarStart(i)
    return (_decodeScalar(startingAt: start).0, start)
  }

  // @_alwaysEmitIntoClient
  internal func _previousScalarStart(_ i: Int) -> Int {
    var prev = i &- 1
    _internalInvariant(prev >= 0)
    while _isUTF8Continuation(prev) {
      prev &-= 1
      _internalInvariant(prev >= 0)
    }
    _internalInvariant(i == prev + _utf8ScalarLength(_loadByte(prev)))
    return prev
  }

  // @_alwaysEmitIntoClient
  internal func _scalarAlign(_ i: Int) -> Int {
    var i = i
    while _slowPath(!_isScalarAligned(i)) {
      i &-= 1
    }
    return i
  }
}

extension UnsafeRawPointer {
  // TODO: ASCII fast path wrappers around ufi functions

  // TODO: hook up to real grapheme breaking
  internal func _urbp(_ range: Range<Int>) -> UnsafeRawBufferPointer {
    .init(start: self + range.lowerBound, count: range.count)
  }

  @_alwaysEmitIntoClient
  internal func _ubp(_ range: Range<Int>) -> UnsafeBufferPointer<UInt8> {
    UnsafeBufferPointer<UInt8>(
      start: UnsafePointer((self+range.lowerBound)._rawValue),
      count: range.count)
  }

  internal func _str(_ range: Range<Int>) -> String {
    String(decoding: _urbp(range) , as: UTF8.self)
  }

  // // @usableFromInline
  // internal func _isCharacterAligned(
  //   _ i: Int,
  //   limitedBy end: Int
  // ) -> Bool {
  //   print(i)
  //   print(end)
  //   _internalInvariant(i >= 0 && i <= end)
  //   if i == 0 || i == end {
  //     return true
  //   }

  //   // TODO: call internals instead
  //   let str = _str(0..<end)
  //   let idx = str.utf8.index(str.utf8.startIndex, offsetBy: i)
  //   return idx == str.index(after: str.index(before: idx))
  // }

  // @usableFromInline
  internal func _nextCharacterStart(
    _ i: Int, limitedBy end: Int
  ) -> Int {
    _internalInvariant((0..<end).contains(i))
    _internalInvariant(_isScalarAligned(i))

    return _nextGraphemeClusterBoundary(startingAt: i) { idx in
      guard idx < end else { return nil }
      let (scalar, end) = _decodeScalar(startingAt: idx)
      return (scalar, end)
    }
  }

  // @usableFromInline
  internal func _previousCharacterStart(
    _ i: Int,
    limitedBy end: Int
  ) -> Int {
    _internalInvariant(_isScalarAligned(i))

    return _previousGraphemeClusterBoundary(endingAt: i) { idx in
      guard idx > 0 else { return nil }
      let (scalar, prior) = _decodeScalar(endingAt: idx)
      return (scalar, prior)
    }
  }

  // @usableFromInline
  internal func _decodeCharacter(
    startingAt i: Int, limitedBy end: Int
  ) -> (Character, nextCharacterStart: Int) {
    let nextStart = _nextCharacterStart(i, limitedBy: end)
    return (Character(_str(i..<nextStart)), nextStart)
  }

  // @usableFromInline
  internal func _decodeCharacter(
    endingAt i: Int,
    limitedBy end: Int
  ) -> (Character, nextCharacterStart: Int) {
    let start = _previousCharacterStart(i, limitedBy: end)
    _internalInvariant(start >= 0)

    return (Character(_str(start..<i)), start)
  }

}

@available(SwiftStdlib 6.1, *)
extension UnsafeRawPointer {
  internal enum _UTF8ValidationResult {
    case success(isASCII: Bool)
    case error(_: Range<Int>)
  }

  // Returns isASCII
  // TODO: return more values
  internal func _validateUTF8(
    limitedBy end: Int
  ) throws(UTF8.ValidationError) -> Bool {
    switch validateUTF8(_ubp(0..<end)) {
    case .success(let info):
      return info.isASCII
    case .error(let kind, let range):
      throw UTF8.ValidationError(kind._publicKind, range)
    }
  }

}
