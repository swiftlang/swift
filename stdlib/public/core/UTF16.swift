//===--- UTF16.swift ------------------------------------------------------===//
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
extension Unicode {
  @frozen
  public enum UTF16 {
  case _swift3Buffer(Unicode.UTF16.ForwardParser)
  }
}

extension Unicode.UTF16 {
  /// Returns the number of code units required to encode the given Unicode
  /// scalar.
  ///
  /// Because a Unicode scalar value can require up to 21 bits to store its
  /// value, some Unicode scalars are represented in UTF-16 by a pair of
  /// 16-bit code units. The first and second code units of the pair,
  /// designated *leading* and *trailing* surrogates, make up a *surrogate
  /// pair*.
  ///
  ///     let anA: Unicode.Scalar = "A"
  ///     print(anA.value)
  ///     // Prints "65"
  ///     print(UTF16.width(anA))
  ///     // Prints "1"
  ///
  ///     let anApple: Unicode.Scalar = "🍎"
  ///     print(anApple.value)
  ///     // Prints "127822"
  ///     print(UTF16.width(anApple))
  ///     // Prints "2"
  ///
  /// - Parameter x: A Unicode scalar value.
  /// - Returns: The width of `x` when encoded in UTF-16, either `1` or `2`.
  @inlinable
  public static func width(_ x: Unicode.Scalar) -> Int {
    return x.value <= UInt16.max ? 1 : 2
  }

  /// Returns the high-surrogate code unit of the surrogate pair representing
  /// the specified Unicode scalar.
  ///
  /// Because a Unicode scalar value can require up to 21 bits to store its
  /// value, some Unicode scalars are represented in UTF-16 by a pair of
  /// 16-bit code units. The first and second code units of the pair,
  /// designated *leading* and *trailing* surrogates, make up a *surrogate
  /// pair*.
  ///
  ///     let apple: Unicode.Scalar = "🍎"
  ///     print(UTF16.leadSurrogate(apple)
  ///     // Prints "55356"
  ///
  /// - Parameter x: A Unicode scalar value. `x` must be represented by a
  ///   surrogate pair when encoded in UTF-16. To check whether `x` is
  ///   represented by a surrogate pair, use `UTF16.width(x) == 2`.
  /// - Returns: The leading surrogate code unit of `x` when encoded in UTF-16.
  @inlinable
  public static func leadSurrogate(_ x: Unicode.Scalar) -> UTF16.CodeUnit {
    _precondition(width(x) == 2)
    return 0xD800 + UTF16.CodeUnit(truncatingIfNeeded:
      (x.value - 0x1_0000) &>> (10 as UInt32))
  }

  /// Returns the low-surrogate code unit of the surrogate pair representing
  /// the specified Unicode scalar.
  ///
  /// Because a Unicode scalar value can require up to 21 bits to store its
  /// value, some Unicode scalars are represented in UTF-16 by a pair of
  /// 16-bit code units. The first and second code units of the pair,
  /// designated *leading* and *trailing* surrogates, make up a *surrogate
  /// pair*.
  ///
  ///     let apple: Unicode.Scalar = "🍎"
  ///     print(UTF16.trailSurrogate(apple)
  ///     // Prints "57166"
  ///
  /// - Parameter x: A Unicode scalar value. `x` must be represented by a
  ///   surrogate pair when encoded in UTF-16. To check whether `x` is
  ///   represented by a surrogate pair, use `UTF16.width(x) == 2`.
  /// - Returns: The trailing surrogate code unit of `x` when encoded in UTF-16.
  @inlinable
  public static func trailSurrogate(_ x: Unicode.Scalar) -> UTF16.CodeUnit {
    _precondition(width(x) == 2)
    return 0xDC00 + UTF16.CodeUnit(truncatingIfNeeded:
      (x.value - 0x1_0000) & (((1 as UInt32) &<< 10) - 1))
  }

  /// Returns a Boolean value indicating whether the specified code unit is a
  /// high-surrogate code unit.
  ///
  /// Here's an example of checking whether each code unit in a string's
  /// `utf16` view is a lead surrogate. The `apple` string contains a single
  /// emoji character made up of a surrogate pair when encoded in UTF-16.
  ///
  ///     let apple = "🍎"
  ///     for unit in apple.utf16 {
  ///         print(UTF16.isLeadSurrogate(unit))
  ///     }
  ///     // Prints "true"
  ///     // Prints "false"
  ///
  /// This method does not validate the encoding of a UTF-16 sequence beyond
  /// the specified code unit. Specifically, it does not validate that a
  /// low-surrogate code unit follows `x`.
  ///
  /// - Parameter x: A UTF-16 code unit.
  /// - Returns: `true` if `x` is a high-surrogate code unit; otherwise,
  ///   `false`.
  @inlinable
  public static func isLeadSurrogate(_ x: CodeUnit) -> Bool {
    return (x & 0xFC00) == 0xD800
  }

  /// Returns a Boolean value indicating whether the specified code unit is a
  /// low-surrogate code unit.
  ///
  /// Here's an example of checking whether each code unit in a string's
  /// `utf16` view is a trailing surrogate. The `apple` string contains a
  /// single emoji character made up of a surrogate pair when encoded in
  /// UTF-16.
  ///
  ///     let apple = "🍎"
  ///     for unit in apple.utf16 {
  ///         print(UTF16.isTrailSurrogate(unit))
  ///     }
  ///     // Prints "false"
  ///     // Prints "true"
  ///
  /// This method does not validate the encoding of a UTF-16 sequence beyond
  /// the specified code unit. Specifically, it does not validate that a
  /// high-surrogate code unit precedes `x`.
  ///
  /// - Parameter x: A UTF-16 code unit.
  /// - Returns: `true` if `x` is a low-surrogate code unit; otherwise,
  ///   `false`.
  @inlinable
  public static func isTrailSurrogate(_ x: CodeUnit) -> Bool {
    return (x & 0xFC00) == 0xDC00
  }

  /// Returns a Boolean value indicating whether the specified code unit is a
  /// high or low surrogate code unit.
  @_alwaysEmitIntoClient
  public static func isSurrogate(_ x: CodeUnit) -> Bool {
    return isLeadSurrogate(x) || isTrailSurrogate(x)
  }

  @inlinable
  public // @testable
  static func _copy<T: _StringElement, U: _StringElement>(
    source: UnsafeMutablePointer<T>,
    destination: UnsafeMutablePointer<U>,
    count: Int
  ) {
    if MemoryLayout<T>.stride == MemoryLayout<U>.stride {
      _memcpy(
        dest: UnsafeMutablePointer(destination),
        src: UnsafeMutablePointer(source),
        size: UInt(count) * UInt(MemoryLayout<U>.stride))
    }
    else {
      for i in 0..<count {
        let u16 = T._toUTF16CodeUnit((source + i).pointee)
        (destination + i).pointee = U._fromUTF16CodeUnit(u16)
      }
    }
  }

  /// Returns the number of UTF-16 code units required for the given code unit
  /// sequence when transcoded to UTF-16, and a Boolean value indicating
  /// whether the sequence was found to contain only ASCII characters.
  ///
  /// The following example finds the length of the UTF-16 encoding of the
  /// string `"Fermata 𝄐"`, starting with its UTF-8 representation.
  ///
  ///     let fermata = "Fermata 𝄐"
  ///     let bytes = fermata.utf8
  ///     print(Array(bytes))
  ///     // Prints "[70, 101, 114, 109, 97, 116, 97, 32, 240, 157, 132, 144]"
  ///
  ///     let result = transcodedLength(of: bytes.makeIterator(),
  ///                                   decodedAs: UTF8.self,
  ///                                   repairingIllFormedSequences: false)
  ///     print(result)
  ///     // Prints "Optional((10, false))"
  ///
  /// - Parameters:
  ///   - input: An iterator of code units to be translated, encoded as
  ///     `sourceEncoding`. If `repairingIllFormedSequences` is `true`, the
  ///     entire iterator will be exhausted. Otherwise, iteration will stop if
  ///     an ill-formed sequence is detected.
  ///   - sourceEncoding: The Unicode encoding of `input`.
  ///   - repairingIllFormedSequences: Pass `true` to measure the length of
  ///     `input` even when `input` contains ill-formed sequences. Each
  ///     ill-formed sequence is replaced with a Unicode replacement character
  ///     (`"\u{FFFD}"`) and is measured as such. Pass `false` to immediately
  ///     stop measuring `input` when an ill-formed sequence is encountered.
  /// - Returns: A tuple containing the number of UTF-16 code units required to
  ///   encode `input` and a Boolean value that indicates whether the `input`
  ///   contained only ASCII characters. If `repairingIllFormedSequences` is
  ///   `false` and an ill-formed sequence is detected, this method returns
  ///   `nil`.
  @inlinable
  public static func transcodedLength<
    Input: IteratorProtocol,
    Encoding: Unicode.Encoding
  >(
    of input: Input,
    decodedAs sourceEncoding: Encoding.Type,
    repairingIllFormedSequences: Bool
  ) -> (count: Int, isASCII: Bool)?
    where Encoding.CodeUnit == Input.Element {

    var utf16Count = 0
    var i = input
    var d = Encoding.ForwardParser()

    // Fast path for ASCII in a UTF8 buffer
    if sourceEncoding == Unicode.UTF8.self {
      var peek: Encoding.CodeUnit = 0
      while let u = i.next() {
        peek = u
        guard _fastPath(peek < 0x80) else { break }
        utf16Count = utf16Count + 1
      }
      if _fastPath(peek < 0x80) { return (utf16Count, true) }

      var d1 = UTF8.ForwardParser()
      d1._buffer.append(numericCast(peek))
      d = _identityCast(d1, to: Encoding.ForwardParser.self)
    }

    var utf16BitUnion: CodeUnit = 0
    while true {
      let s = d.parseScalar(from: &i)
      if _fastPath(s._valid != nil), let scalarContent = s._valid {
        let utf16 = transcode(scalarContent, from: sourceEncoding)
          ._unsafelyUnwrappedUnchecked
        utf16Count += utf16.count
        for x in utf16 { utf16BitUnion |= x }
      }
      else if let _ = s._error {
        guard _fastPath(repairingIllFormedSequences) else { return nil }
        utf16Count += 1
        utf16BitUnion |= UTF16._replacementCodeUnit
      }
      else {
        return (utf16Count, utf16BitUnion < 0x80)
      }
    }
  }
}

extension Unicode.UTF16: Unicode.Encoding {
  public typealias CodeUnit = UInt16
  public typealias EncodedScalar = _UIntBuffer<UInt16>

  @inlinable
  internal static var _replacementCodeUnit: CodeUnit {
    @inline(__always) get { return 0xfffd }
  }
  
  @inlinable
  public static var encodedReplacementCharacter: EncodedScalar {
    return EncodedScalar(_storage: 0xFFFD, _bitCount: 16)
  }

  /// Returns whether the given code unit represents an ASCII scalar
  @_alwaysEmitIntoClient
  public static func isASCII(_ x: CodeUnit) -> Bool {
    return x <= 0x7f
  }

  @inlinable
  public static func _isScalar(_ x: CodeUnit) -> Bool {
    return x & 0xf800 != 0xd800
  }

  @inlinable
  @inline(__always)
  internal static func _decodeSurrogates(
    _ lead: CodeUnit,
    _ trail: CodeUnit
  ) -> Unicode.Scalar {
    _internalInvariant(isLeadSurrogate(lead))
    _internalInvariant(isTrailSurrogate(trail))
    return Unicode.Scalar(
      _unchecked: 0x10000 +
        (UInt32(lead & 0x03ff) &<< 10 | UInt32(trail & 0x03ff)))
  }

  @inlinable
  public static func decode(_ source: EncodedScalar) -> Unicode.Scalar {
    let bits = source._storage
    if _fastPath(source._bitCount == 16) {
      return Unicode.Scalar(_unchecked: bits & 0xffff)
    }
    _internalInvariant(source._bitCount == 32)
    let lower: UInt32 = bits >> 16 & 0x03ff
    let upper: UInt32 = (bits & 0x03ff) << 10
    let value = 0x10000 + (lower | upper)
    return Unicode.Scalar(_unchecked: value)
  }

  @inlinable
  public static func encode(
    _ source: Unicode.Scalar
  ) -> EncodedScalar? {
    let x = source.value
    if _fastPath(x < ((1 as UInt32) << 16)) {
      return EncodedScalar(_storage: x, _bitCount: 16)
    }
    let x1 = x - ((1 as UInt32) << 16)
    var r = (0xdc00 + (x1 & 0x3ff))
    r &<<= 16
    r |= (0xd800 + (x1 &>> 10 & 0x3ff))
    return EncodedScalar(_storage: r, _bitCount: 32)
  }

  @inlinable
  @inline(__always)
  public static func transcode<FromEncoding: Unicode.Encoding>(
    _ content: FromEncoding.EncodedScalar, from _: FromEncoding.Type
  ) -> EncodedScalar? {
    if _fastPath(FromEncoding.self == UTF8.self) {
      let c = _identityCast(content, to: UTF8.EncodedScalar.self)
      var b = c.count
      b = b &- 1
      if _fastPath(b == 0) {
        return EncodedScalar(
          _storage: (c._biasedBits &- 0x1) & 0b0__111_1111, _bitCount: 16)
      }
      var s = c._biasedBits &- 0x01010101
      var r = s
      r &<<= 6
      s &>>= 8
      r |= s & 0b0__11_1111
      b = b &- 1
      
      if _fastPath(b == 0) {
        return EncodedScalar(_storage: r & 0b0__111_1111_1111, _bitCount: 16)
      }
      r &<<= 6
      s &>>= 8
      r |= s & 0b0__11_1111
      b = b &- 1
      
      if _fastPath(b == 0) {
        return EncodedScalar(_storage: r & 0xFFFF, _bitCount: 16)
      }
      
      r &<<= 6
      s &>>= 8
      r |= s & 0b0__11_1111
      r &= (1 &<< 21) - 1
      return encode(Unicode.Scalar(_unchecked: r))
    }
    else if _fastPath(FromEncoding.self == UTF16.self) {
      return unsafeBitCast(content, to: UTF16.EncodedScalar.self)
    }
    return encode(FromEncoding.decode(content))
  }
  
  @frozen
  public struct ForwardParser {
    public typealias _Buffer = _UIntBuffer<UInt16>
    @inlinable
    public init() { _buffer = _Buffer() }
    public var _buffer: _Buffer
  }
  
  @frozen
  public struct ReverseParser {
    public typealias _Buffer = _UIntBuffer<UInt16>
    @inlinable
    public init() { _buffer = _Buffer() }
    public var _buffer: _Buffer
  }
}

extension UTF16.ReverseParser: Unicode.Parser, _UTFParser {
  public typealias Encoding = Unicode.UTF16

  @inlinable
  public func _parseMultipleCodeUnits() -> (isValid: Bool, bitCount: UInt8) {
    _internalInvariant(  // this case handled elsewhere
      !Encoding._isScalar(UInt16(truncatingIfNeeded: _buffer._storage)))
    if _fastPath(_buffer._storage & 0xFC00_FC00 == 0xD800_DC00) {
      return (true, 2*16)
    }
    return (false, 1*16)
  }
  
  @inlinable
  public func _bufferedScalar(bitCount: UInt8) -> Encoding.EncodedScalar {
    return Encoding.EncodedScalar(
      _storage:
        (_buffer._storage &<< 16 | _buffer._storage &>> 16) &>> (32 - bitCount),
      _bitCount: bitCount
    )
  }
}

extension Unicode.UTF16.ForwardParser: Unicode.Parser, _UTFParser {
  public typealias Encoding = Unicode.UTF16
  
  @inlinable
  public func _parseMultipleCodeUnits() -> (isValid: Bool, bitCount: UInt8) {
    _internalInvariant(  // this case handled elsewhere
      !Encoding._isScalar(UInt16(truncatingIfNeeded: _buffer._storage)))
    if _fastPath(_buffer._storage & 0xFC00_FC00 == 0xDC00_D800) {
      return (true, 2*16)
    }
    return (false, 1*16)
  }
  
  @inlinable
  public func _bufferedScalar(bitCount: UInt8) -> Encoding.EncodedScalar {
    var r = _buffer
    r._bitCount = bitCount
    return r
  }
}
