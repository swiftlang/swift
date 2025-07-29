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
  public enum UTF16: Sendable {
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
  ///     print(UTF16.leadSurrogate(apple))
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
  ///     print(UTF16.trailSurrogate(apple))
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
      unsafe _memcpy(
        dest: UnsafeMutablePointer(destination),
        src: UnsafeMutablePointer(source),
        size: UInt(count) * UInt(MemoryLayout<U>.stride))
    }
    else {
      for i in 0..<count {
        let u16 = unsafe T._toUTF16CodeUnit((source + i).pointee)
        unsafe (destination + i).pointee = U._fromUTF16CodeUnit(u16)
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
  ///     let result = UTF16.transcodedLength(of: bytes.makeIterator(),
  ///                                         decodedAs: UTF8.self,
  ///                                         repairingIllFormedSequences: false)
  ///     print(result)
  ///     // Prints "Optional((count: 10, isASCII: false))"
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
    fatalError()
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
      return unsafe unsafeBitCast(content, to: UTF16.EncodedScalar.self)
    }
    return encode(FromEncoding.decode(content))
  }

  @frozen
  public struct ForwardParser: Sendable {
    public typealias _Buffer = _UIntBuffer<UInt16>

    public var _buffer: _Buffer

    @inlinable
    public init() { _buffer = _Buffer() }
  }

  @frozen
  public struct ReverseParser: Sendable {
    public typealias _Buffer = _UIntBuffer<UInt16>

    public var _buffer: _Buffer

    @inlinable
    public init() { _buffer = _Buffer() }
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

private enum ScalarFallbackResult: UInt8 {
  case invalid
  case singleByte
  case multiByte
}

#if arch(arm64_32)
typealias Word = UInt64
#else
typealias Word = UInt
#endif
//Blocks containing only ASCII will not have any of these bits set
@_transparent var asciiMask:Word {
  Word(truncatingIfNeeded: 0xFF80FF80_FF80FF80 as UInt64)
}

/*
 Shifting a UInt16 value by 7 bits gets us:
 <0x80: 0 (ASCII, 1 byte UTF8)
 0x80-0x7FF: 1-15 (2 byte UTF8)
 0x800-0x3FFF: 16-127 (3 byte UTF8)
 Which we can use as an index into this table.
 Values >= 0x4000 are handled via the scalar path to keep the table size down
 */
@_transparent var lengthLUT: InlineArray<128, UInt8> {
  [1, //0
   2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, //1-15
   3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
   3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
   3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
   3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
   3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
   3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
   3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3] //16 - 128
}
//Blocks with values we can use the LUT for don't have any of these bits set
@_transparent var largeValueMask: Word {
  Word(truncatingIfNeeded: 0xE000E000_E000E000 as UInt64)
}

@_transparent var utf8TwoByteMax: UInt32 { 0x7FF }
@_transparent var utf16LeadSurrogateMin: UInt32 { 0xD800 }
@_transparent var utf16TrailSurrogateMin: UInt32 { 0xDC00 }
@_transparent var utf16ReplacementCharacter: UInt32 { 0xFFFD }
@_transparent var utf16ScalarMax: UInt32 { 0x10FFFF }
@_transparent var utf16BasicMultilingualPlaneMax: UInt32 { 0xFFFF }
@_transparent var utf16AstralPlaneMin: UInt32 { 0x10000 }

typealias Block = (Word, Word, Word, Word)

#if SWIFT_STDLIB_ENABLE_VECTOR_TYPES
#if _pointerBitWidth(_32) && !arch(arm64_32)
@_transparent var blockSize:Int { 8 }
@_transparent
func allASCIIBlock(at pointer: UnsafePointer<UInt16>) -> SIMD8<UInt8>? {
  let block = unsafe UnsafeRawPointer(pointer).loadUnaligned(as: Block.self)
  return unsafe ((block.0 | block.1 | block.2 | block.3) & asciiMask == 0)
    ? unsafeBitCast(block, to: SIMD16<UInt8>.self).evenHalf : nil
}
@_transparent
func _utf8CountOfBlock(at pointer: UnsafePointer<UInt16>) -> Int? {
  let block = unsafe UnsafeRawPointer(pointer).loadUnaligned(as: Block.self)
  if (block.0 | block.1 | block.2 | block.3) & asciiMask == 0 {
    return 8
  }
  if (block.0 | block.1 | block.2 | block.3) & largeValueMask != 0 {
    return nil
  }
  let lut = lengthLUT
  return unsafe withUnsafeBytes(of: lut) { lutBuffer in
    let simdBlock = unsafe unsafeBitCast(block, to: SIMD16<UInt16>.self) &>> 7
    var count = 0
    for i in 0 ..< 8 {
      count &+= unsafe Int(lutBuffer[Int(simdBlock[i])])
    }
    return count
  }
}
#else
@_transparent var blockSize:Int { 16 }
@_transparent
func allASCIIBlock(at pointer: UnsafePointer<UInt16>) -> SIMD16<UInt8>? {
  let block = unsafe UnsafeRawPointer(pointer).loadUnaligned(as: Block.self)
  return unsafe ((block.0 | block.1 | block.2 | block.3) & asciiMask == 0)
    ? unsafeBitCast(block, to: SIMD32<UInt8>.self).evenHalf : nil
}
@_transparent
func _utf8CountOfBlock(at pointer: UnsafePointer<UInt16>) -> Int? {
  let block = unsafe UnsafeRawPointer(pointer).loadUnaligned(as: Block.self)
  if (block.0 | block.1 | block.2 | block.3) & asciiMask == 0 {
    return 16
  }
  if (block.0 | block.1 | block.2 | block.3) & largeValueMask != 0 {
    return nil
  }
  let lut = lengthLUT
  return unsafe withUnsafeBytes(of: lut) { lutBuffer in
    let simdBlock = unsafe unsafeBitCast(block, to: SIMD16<UInt16>.self) &>> 7
    var count = 0
    for i in 0 ..< 16 {
      count &+= unsafe Int(lutBuffer[Int(simdBlock[i])])
    }
    return count
  }
}
#endif
#else
@_transparent var blockSize:Int { 1 }
@_transparent
func allASCIIBlock(at pointer: UnsafePointer<UInt16>) -> CollectionOfOne<UInt8>? {
  let value = unsafe pointer.pointee
  if value & 0xFF80 == 0 {
    return CollectionOfOne(UInt8(truncatingIfNeeded: value))
  }
  return nil
}
@_transparent
func _utf8CountOfBlock(at pointer: UnsafePointer<UInt16>) -> Int? {
  return switch unsafe UInt32(pointer.pointee) {
    case 0 ..< 0x80: 1
    case 0x80 ..< 0x800 : 2
    case 0x800 ..< 0x10000: 3
    default: nil
  }
}
#endif

/*
 This is expressible in a more concise way using the other transcoding
 primitives in the stdlib, but at least as of July 2025 doing that makes
 processing runs of non-ASCII several times slower.
 */
@inline(__always)
private func encodeScalarAsUTF8(
  _ scalar: UInt32,
  output: inout UnsafeMutablePointer<Unicode.UTF8.CodeUnit>,
  outputEnd: UnsafePointer<Unicode.UTF8.CodeUnit>,
) -> ScalarFallbackResult {
  _debugPrecondition(scalar >= 0x80)
  _debugPrecondition(scalar <= utf16ScalarMax)
  if scalar <= utf8TwoByteMax {
    if unsafe output + 2 > outputEnd { return .invalid }
    // Scalar fits in 11 bits
    // 2 byte UTF8 is 0b110[top 5 bits] 0b10[bottom 6 bits]
    unsafe output.pointee = 0b1100_0000 | UInt8((scalar >> 6) & 0b01_1111)
    unsafe (output + 1).pointee = 0b1000_0000 | UInt8(scalar & 0b11_1111)
    unsafe output += 2
  } else if scalar <= utf16BasicMultilingualPlaneMax {
    // Scalar fits in 16 bits
    // 3 byte UTF8 is 0b1110[top 4 bits] 0b10[middle 6 bits] 0b10[bottom 6 bits]
    if unsafe output + 3 > outputEnd { return .invalid }
    unsafe output.pointee = 0b1110_0000 | UInt8((scalar >> 12) & 0b1111)
    unsafe (output + 1).pointee = 0b1000_0000 | UInt8((scalar >> 6) & 0b11_1111)
    unsafe (output + 2).pointee = 0b1000_0000 | UInt8(scalar & 0b11_1111)
    unsafe output += 3
  } else if scalar <= utf16ScalarMax {
    // Scalar fits in 21 bits.
    // 0b11110[top 3] 0b10[upper middle 6] 0b10[lower middle 6] 0b10[bottom 6]
    if unsafe output + 4 > outputEnd { return .invalid }
    unsafe output.pointee = 0b1111_0000 | UInt8((scalar >> 18) & 0b0111)
    unsafe (output + 1).pointee = 0b1000_0000 | UInt8((scalar >> 12) & 0b11_1111)
    unsafe (output + 2).pointee = 0b1000_0000 | UInt8((scalar >> 6) & 0b11_1111)
    unsafe (output + 3).pointee = 0b1000_0000 | UInt8(scalar & 0b11_1111)
    unsafe output += 4
  } else {
    Builtin.unreachable()
  }
  return .multiByte
}

@inline(__always)
private func processNonASCIIScalarFallback(
  _ cu: UInt16,
  input: inout UnsafePointer<UInt16>,
  inputEnd: UnsafePointer<UInt16>,
  output: inout UnsafeMutablePointer<Unicode.UTF8.CodeUnit>,
  outputEnd: UnsafePointer<Unicode.UTF8.CodeUnit>,
  repairing: Bool
) -> (ScalarFallbackResult, repairsMade: Bool) {
  var scalar: UInt32 = 0
  var invalid = false
  if UTF16.isLeadSurrogate(cu) {
    if unsafe input + 1 >= inputEnd {
      //Leading with no room for trailing
      invalid = true
      unsafe input += 1
    } else {
      let next = unsafe (input + 1).pointee
      if !UTF16.isTrailSurrogate(next) {
        //Leading followed by non-trailing
        invalid = true
        unsafe input += 1
      } else {
        /*
         Code points outside the BMP are encoded as:
         value -= smallest non-BMP code point
         lead = smallest leading surrogate + high 10 bits of value
         trail = smallest trailing surrogate + low 10 bits of value
         */
        scalar = utf16AstralPlaneMin
          + ((UInt32(cu) - utf16LeadSurrogateMin) << 10)
          + (UInt32(next) - utf16TrailSurrogateMin)
        unsafe input += 2
      }
    }
  } else if UTF16.isTrailSurrogate(cu) {
    //Trailing with no leading
    invalid = true
    unsafe input += 1
  } else {
    scalar = UInt32(cu)
    unsafe input += 1
  }
  if _slowPath(invalid || scalar > utf16ScalarMax) {
    guard repairing else { return (.invalid, repairsMade: false) }
    return (
      unsafe encodeScalarAsUTF8(utf16ReplacementCharacter,
                                output: &output,
                                outputEnd: outputEnd),
      repairsMade: true
    )
  }
  return (
    unsafe encodeScalarAsUTF8(scalar, output: &output, outputEnd: outputEnd),
    repairsMade: false
  )
}

@inline(__always)
private func processScalarFallback(
  input: inout UnsafePointer<Unicode.UTF16.CodeUnit>,
  inputEnd: UnsafePointer<Unicode.UTF16.CodeUnit>,
  output: inout UnsafeMutablePointer<Unicode.UTF8.CodeUnit>,
  outputEnd: UnsafePointer<Unicode.UTF8.CodeUnit>,
  repairing: Bool
) -> (ScalarFallbackResult, repairsMade: Bool) {
  let cu = unsafe input.pointee
  if Unicode.UTF16.isASCII(cu) {
    if unsafe output < outputEnd {
      unsafe output.initialize(to: UInt8(truncatingIfNeeded: cu))
      unsafe input += 1
      unsafe output += 1
    } else {
      Builtin.unreachable()
    }
  } else {
    // Scalar fallback for this code unit
    return unsafe processNonASCIIScalarFallback(
      cu,
      input: &input,
      inputEnd: inputEnd,
      output: &output,
      outputEnd: outputEnd,
      repairing: repairing
    )
  }
  return (.singleByte, repairsMade: false)
}

func processNonASCIIChunk(
  input: inout UnsafePointer<UInt16>,
  inputEnd: UnsafePointer<UInt16>,
  output: inout UnsafeMutablePointer<UInt8>,
  outputEnd: UnsafePointer<UInt8>,
  repairing: Bool
) -> (Bool, repairsMade: Bool) {
  for _ in 0 ..< blockSize {
    switch unsafe processScalarFallback(
      input: &input,
      inputEnd: inputEnd,
      output: &output,
      outputEnd: outputEnd,
      repairing: repairing
    ) {
    case (.invalid, let repairsMade):
      return (false, repairsMade: repairsMade)
    case (.multiByte, let repairsMade):
      return (true, repairsMade: repairsMade) //found the non-ASCII, try starting a new SIMD batch
    case (.singleByte, _):
      continue
    }
  }
  Builtin.unreachable()
}

internal func transcodeUTF16ToUTF8(
  UTF16CodeUnits: UnsafeBufferPointer<Unicode.UTF16.CodeUnit>,
  into outputBuffer: UnsafeMutableBufferPointer<Unicode.UTF8.CodeUnit>,
  repairing: Bool = true
) -> (Int, repairsMade: Bool) {
  let inCount = UTF16CodeUnits.count
  let outCount = outputBuffer.count
  guard inCount > 0, outCount > 0 else { return (0, repairsMade: false) }
  var input = unsafe UTF16CodeUnits.baseAddress.unsafelyUnwrapped
  let inputEnd = unsafe input + inCount
  var output = unsafe outputBuffer.baseAddress.unsafelyUnwrapped
  let outputStart = unsafe output
  let outputEnd = unsafe output + outCount
  var repairsMade = false
  
  while unsafe input <= (inputEnd - blockSize) && output <= (outputEnd - (blockSize / 2)) {
    if let asciiBlock = unsafe allASCIIBlock(at: input) {
      // All ASCII: transcode directly
      for i in 0 ..< blockSize {
        unsafe (output + i).initialize(to: asciiBlock[i])
      }
      unsafe input += blockSize
      unsafe output += blockSize
    } else {
      let (success, tmpRepairsMade) = unsafe processNonASCIIChunk(
        input: &input,
        inputEnd: inputEnd,
        output: &output,
        outputEnd: outputEnd,
        repairing: repairing
      )
      repairsMade = repairsMade && tmpRepairsMade
      if !success {
        return unsafe (output - outputStart, repairsMade: repairsMade)
      }
    }
  }
  // Finish any remaining code units using fallback scalar loop
  while unsafe input < inputEnd && output < outputEnd {
    switch unsafe processScalarFallback(
      input: &input,
      inputEnd: inputEnd,
      output: &output,
      outputEnd: outputEnd,
      repairing: repairing
    ) {
    case (.invalid, let tmpRepairsMade):
      return unsafe (output - outputStart, repairsMade: repairsMade && tmpRepairsMade)
    case (_, let tmpRepairsMade):
      repairsMade = repairsMade && tmpRepairsMade
    }
  }
  return unsafe (output - outputStart, repairsMade: repairsMade)
}

internal func utf8Length(
  of UTF16CodeUnits: UnsafeBufferPointer<Unicode.UTF16.CodeUnit>,
  repairing: Bool = true
) -> (Int, isASCII: Bool)? {
  let inCount = UTF16CodeUnits.count
  guard inCount > 0 else { return (0, isASCII: true) }
  var input = unsafe UTF16CodeUnits.baseAddress.unsafelyUnwrapped
  let inputEnd = unsafe input + inCount
  var count = 0
  while unsafe input < (inputEnd - blockSize) {
    if let blockCount = unsafe _utf8CountOfBlock(at: input) {
      unsafe input += blockSize
      count &+= blockCount
    } else {
      let chunkCount = unsafe withUnsafeTemporaryAllocation(
        of: UInt8.self, capacity: blockSize * 4 /*max 4 bytes per UTF8 element*/
      ) { outputBuf -> Int? in
        var output = unsafe outputBuf.baseAddress.unsafelyUnwrapped
        let outputStart = unsafe output
        let outputEnd = unsafe output + outputBuf.count
        if unsafe !processNonASCIIChunk(
          input: &input,
          inputEnd: inputEnd,
          output: &output,
          outputEnd: outputEnd,
          repairing: repairing
        ).0 {
          return nil
        }
        return unsafe output - outputStart
      }
      if let chunkCount {
        count += chunkCount
      } else {
        return nil
      }
    }
  }
  // Finish any remaining input that didn't fit in a SIMD chunk
  while unsafe input < inputEnd {
    let trailingCount = unsafe withUnsafeTemporaryAllocation(
      of: UInt8.self, capacity: 4 /*max 4 bytes per UTF8 element*/
    ) { outputBuf -> Int? in
      var output = unsafe outputBuf.baseAddress.unsafelyUnwrapped
      let outputStart = unsafe output
      let outputEnd = unsafe output + outputBuf.count
      let (scalarFallBackResult, _) = unsafe processScalarFallback(
        input: &input,
        inputEnd: inputEnd,
        output: &output,
        outputEnd: outputEnd,
        repairing: repairing
      )
      if scalarFallBackResult ~= .invalid {
        return nil
      }
      return unsafe output - outputStart
    }
    if let trailingCount {
      count += trailingCount
    } else {
      return nil
    }
  }
  return (count, isASCII: count == inCount)
}
