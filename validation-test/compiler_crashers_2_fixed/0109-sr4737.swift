// RUN: not %target-swift-frontend %s -typecheck

//===----------------------------------------------------------------------===//
extension UnicodeScalar {
  // Hack providing an efficient API that is available to the standard library
  @usableFromInline
  @inline(__always)
  init(_unchecked x: UInt32) { self = unsafeBitCast(x, to: UnicodeScalar.self) }

  static var replacementCharacter: UnicodeScalar {
    return UnicodeScalar(_unchecked: 0xfffd)
  }
}
//===----------------------------------------------------------------------===//
@_fixed_layout
public struct _UIntBuffer<
  Storage: UnsignedInteger & FixedWidthInteger, 
  Element: UnsignedInteger & FixedWidthInteger
> {
  @usableFromInline
  var _storage: Storage
  @usableFromInline
  var _bitCount: UInt8

  @inline(__always)
  @usableFromInline
  internal init(_storage: Storage, _bitCount: UInt8) {
    self._storage = _storage
    self._bitCount = _bitCount
  }
  
  @inline(__always)
  public init(containing e: Element) {
    _storage = Storage(truncatingIfNeeded: e)
    _bitCount = UInt8(truncatingIfNeeded: Element.bitWidth)
  }
}

extension _UIntBuffer : Sequence {
  @_fixed_layout
  public struct Iterator : IteratorProtocol, Sequence {
    @inline(__always)
    public init(_ x: _UIntBuffer) { _impl = x }
    
    @inline(__always)
    public mutating func next() -> Element? {
      if _impl._bitCount == 0 { return nil }
      defer {
        _impl._storage = _impl._storage &>> Element.bitWidth
        _impl._bitCount = _impl._bitCount &- _impl._elementWidth
      }
      return Element(truncatingIfNeeded: _impl._storage)
    }
    @usableFromInline
    var _impl: _UIntBuffer
  }
  
  @inline(__always)
  public func makeIterator() -> Iterator {
    return Iterator(self)
  }

  @inline(__always)
  public func reversed() -> _UIntBuffer {
    if Element.bitWidth == 8 {
      return _UIntBuffer(
        _storage:
        storage.byteSwapped &>> (Storage.bitWidth &- numericCast(_bitCount)),
        _bitCount: _bitCount)
    }
    else {
      var s: Storage = 0
      for x in self {
        s <<= Element.bitWidth
        s |= Storage(truncatingIfNeeded: x)
      }
      return Self(_storage: s, _bitCount: _bitCount)
    }
  }
}

extension _UIntBuffer : Collection {
  public typealias _Element = Element
  
  public struct Index : Comparable {
    @usableFromInline
    var bitOffset: UInt8
    
    @usableFromInline
    init(bitOffset: UInt8) { self.bitOffset = bitOffset }
    
    public static func == (lhs: Index, rhs: Index) -> Bool {
      return lhs.bitOffset == rhs.bitOffset
    }
    public static func < (lhs: Index, rhs: Index) -> Bool {
      return lhs.bitOffset < rhs.bitOffset
    }
  }

  public var startIndex : Index {
    @inline(__always)
    get { return Index(bitOffset: 0) }
  }
  
  public var endIndex : Index {
    @inline(__always)
    get { return Index(bitOffset: _bitCount) }
  }
  
  @inline(__always)
  public func index(after i: Index) -> Index {
    return Index(bitOffset: i.bitOffset &+ _elementWidth)
  }

  @usableFromInline
  internal var _elementWidth : UInt8 {
    return UInt8(truncatingIfNeeded: Element.bitWidth)
  }
  
  public subscript(i: Index) -> Element {
    @inline(__always)
    get {
      return Element(truncatingIfNeeded: _storage &>> i.bitOffset)
    }
  }
}

extension _UIntBuffer : BidirectionalCollection {
  @inline(__always)
  public func index(before i: Index) -> Index {
    return Index(bitOffset: i.bitOffset &- _elementWidth)
  }
}

extension _UIntBuffer : RandomAccessCollection {
  public typealias Indices = DefaultIndices<_UIntBuffer>
  
  @inline(__always)
  public func index(_ i: Index, offsetBy n: Int) -> Index {
    let x = Int(i.bitOffset) &+ n &* Element.bitWidth
    return Index(bitOffset: UInt8(truncatingIfNeeded: x))
  }

  @inline(__always)
  public func distance(from i: Index, to j: Index) -> Int {
    return (Int(j.bitOffset) &- Int(i.bitOffset)) / Element.bitWidth
  }
}

extension FixedWidthInteger {
  @inline(__always)
  @usableFromInline
  func _fullShiftLeft<N: FixedWidthInteger>(_ n: N) -> Self {
    return (self &<< ((n &+ 1) &>> 1)) &<< (n &>> 1)
  }
  @inline(__always)
  @usableFromInline
  func _fullShiftRight<N: FixedWidthInteger>(_ n: N) -> Self {
    return (self &>> ((n &+ 1) &>> 1)) &>> (n &>> 1)
  }
  @inline(__always)
  @usableFromInline
  static func _lowBits<N: FixedWidthInteger>(_ n: N) -> Self {
    return ~((~0 as Self)._fullShiftLeft(n))
  }
}

extension Range {
  @inline(__always)
  @usableFromInline
  func _contains_(_ other: Range) -> Bool {
    return other.clamped(to: self) == other
  }
}

extension _UIntBuffer : RangeReplaceableCollection {
  @inline(__always)
  public init() {
    _storage = 0
    _bitCount = 0
  }

  public var capacity: Int {
    return Storage.bitWidth / Element.bitWidth
  }

  @inline(__always)
  public mutating func append(_ newElement: Element) {
    _debugPrecondition(count < capacity)
    _storage |= Storage(newElement) &<< _bitCount
    _bitCount = _bitCount &+ _elementWidth
  }
  
  @inline(__always)
  public mutating func replaceSubrange<C: Collection>(
    _ target: Range<Index>, with replacement: C
  ) where C._Element == Element {
    _debugPrecondition(
      (0..<_bitCount)._contains_(
        target.lowerBound.bitOffset..<target.upperBound.bitOffset))
    
    let replacement1 = _UIntBuffer(replacement)

    let targetCount = distance(
      from: target.lowerBound, to: target.upperBound)
    let growth = replacement1.count &- targetCount
    _debugPrecondition(count + growth <= capacity)

    let headCount = distance(from: startIndex, to: target.lowerBound)
    let tailOffset = distance(from: startIndex, to: target.upperBound)

    let w = Element.bitWidth
    let headBits = _storage & ._lowBits(headCount &* w)
    let tailBits = _storage._fullShiftRight(tailOffset &* w)

    _storage = headBits
    _storage |= replacement1._storage &<< (headCount &* w)
    _storage |= tailBits &<< ((tailOffset &+ growth) &* w)
    _bitCount = UInt8(
      truncatingIfNeeded: Int(_bitCount) &+ growth &* w)
  }
}
//===----------------------------------------------------------------------===//

public enum Unicode {
  public typealias UTF8 = Swift.UTF8
  public typealias UTF16 = Swift.UTF16
  public typealias UTF32 = Swift.UTF32
}

extension Unicode {
  public typealias ParseResult<T> = (T, consumedCodeUnits: UInt8, isValid: Bool)
}

public protocol UnicodeDecoder {
  associatedtype CodeUnit : UnsignedInteger, FixedWidthInteger
  associatedtype EncodedScalar : BidirectionalCollection
    where EncodedScalar.Iterator.Element == CodeUnit

  init()

  mutating func parseOne<I : IteratorProtocol>(
    _ input: inout I
  ) -> Unicode.ParseResult<EncodedScalar> where I.Element == CodeUnit
}

extension UnicodeDecoder {
  @inline(__always)
  @discardableResult
  public static func decode<I: IteratorProtocol>(
    _ input: inout I,
    repairingIllFormedSequences makeRepairs: Bool,
    into output: (UnicodeScalar)->Void
  ) -> Int
  where I.Element == CodeUnit
  {
    var errors = 0
    var d = Self()
    while true {
      switch d.parseOne(&input) {
      case let .valid(scalarContent):
        output(decodeOne(scalarContent))
      case .invalid:
        if !makeRepairs { return 1 }
        errors += 1
        output(UnicodeScalar(_unchecked: 0xFFFD))
      case .emptyInput:
        return errors
      }
    }
  }
}


extension Unicode {
  struct ParsingIterator<
    CodeUnitIterator : IteratorProtocol, 
    Encoding: UnicodeEncoding,
    Decoder: UnicodeDecoder
  > where Decoder.CodeUnit == CodeUnitIterator.Element,
    Encoding.EncodedScalar == Decoder.EncodedScalar {
    var codeUnits: CodeUnitIterator
    var decoder: Decoder
  }
}
extension Unicode.ParsingIterator : IteratorProtocol, Sequence {
  mutating func next() -> Decoder.EncodedScalar? {
    switch decoder.parseOne(&codeUnits) {
    case let .valid(scalarContent): return scalarContent
    case .invalid: return Encoding.encodedReplacementScalar
    case .emptyInput: return nil
    }
  }
}

extension Unicode {
  struct DefaultScalarView<
    CodeUnits: BidirectionalCollection,
    Encoding: UnicodeEncoding
  > where CodeUnits.Iterator.Element == Encoding.CodeUnit {
    var codeUnits: CodeUnits
    init(
      _ codeUnits: CodeUnits,
      fromEncoding _: Encoding.Type = Encoding.self) {
      self.codeUnits = codeUnits
    }
  }
}

extension Unicode.DefaultScalarView : Sequence {
  struct Iterator {
    var parsing: Unicode.ParsingIterator<
      CodeUnits.Iterator, Encoding, Encoding.ForwardDecoder
    >
  }
  
  func makeIterator() -> Iterator {
    return Iterator(
      parsing: Unicode.ParsingIterator(
        codeUnits: codeUnits.makeIterator(),
        decoder: Encoding.ForwardDecoder()
      ))
  }
}

extension Unicode.DefaultScalarView.Iterator : IteratorProtocol, Sequence {
  mutating func next() -> UnicodeScalar? {
    return parsing.next().map {
      Encoding.ForwardDecoder.decodeOne($0)
    }
  }
}

extension Unicode.DefaultScalarView {
  struct Index {
    var codeUnitIndex: CodeUnits.Index
    var scalar: UnicodeScalar
    var stride: UInt8
  }
}

extension Unicode.DefaultScalarView.Index : Comparable {
  @inline(__always)
  public static func < (
    lhs: Unicode.DefaultScalarView<CodeUnits,Encoding>.Index,
    rhs: Unicode.DefaultScalarView<CodeUnits,Encoding>.Index
  ) -> Bool {
    return lhs.codeUnitIndex < rhs.codeUnitIndex
  }
  
  @inline(__always)
  public static func == (
    lhs: Unicode.DefaultScalarView<CodeUnits,Encoding>.Index,
    rhs: Unicode.DefaultScalarView<CodeUnits,Encoding>.Index
  ) -> Bool {
    return lhs.codeUnitIndex == rhs.codeUnitIndex
  }
}

extension Unicode.DefaultScalarView : Collection {
  public var startIndex: Index {
    @inline(__always)
    get {
      return index(
        after: Index(
          codeUnitIndex: codeUnits.startIndex,
          scalar: UnicodeScalar(_unchecked: 0),
          stride: 0)
      )
    }
  }

  public var endIndex: Index {
    @inline(__always)
    get {
      return Index(
        codeUnitIndex: codeUnits.endIndex,
        scalar: UnicodeScalar(_unchecked: 0),
        stride: 0)
    }
  }

  public subscript(i: Index) -> UnicodeScalar {
    @inline(__always) get { return i.scalar }
  }

  @inline(__always)
  public func index(after i: Index) -> Index {
    let nextPosition = codeUnits.index(
      i.codeUnitIndex, offsetBy: numericCast(i.stride))
    var i = IndexingIterator(
      _elements: codeUnits, _position: nextPosition
    )
    var d = Encoding.ForwardDecoder()
    switch d.parseOne(&i) {
    case .valid(let scalarContent):
      return Index(
        codeUnitIndex: nextPosition,
        scalar: Encoding.ForwardDecoder.decodeOne(scalarContent),
        stride: numericCast(scalarContent.count))
    case .invalid(let stride):
      return Index(
        codeUnitIndex: nextPosition,
        scalar: UnicodeScalar(_unchecked: 0xfffd),
        stride: numericCast(stride))
    case .emptyInput:
      return endIndex
    }
  }
}

// This should go in the standard library; see
// https://github.com/apple/swift/pull/9074 and
// https://bugs.swift.org/browse/SR-4721
@_fixed_layout
public struct ReverseIndexingIterator<
  Elements : BidirectionalCollection
> : IteratorProtocol, Sequence {

  @inlinable
  @inline(__always)
  /// Creates an iterator over the given collection.
  public /// @testable
  init(_elements: Elements, _position: Elements.Index) {
    self._elements = _elements
    self._position = _position
  }
  
  @inlinable
  @inline(__always)
  public mutating func next() -> Elements._Element? {
    guard _fastPath(_position != _elements.startIndex) else { return nil }
    _position = _elements.index(before: _position)
    return _elements[_position]
  }
  
  @usableFromInline
  internal let _elements: Elements
  @usableFromInline
  internal var _position: Elements.Index
}

extension Unicode.DefaultScalarView : BidirectionalCollection {
  @inline(__always)
  public func index(before i: Index) -> Index {
    var d = Encoding.ReverseDecoder()
    
    var more = ReverseIndexingIterator(
      _elements: codeUnits, _position: i.codeUnitIndex)
    
    switch d.parseOne(&more) {
    case .valid(let scalarContent):
      let d: Int = -numericCast(scalarContent.count)
      return Index(
        codeUnitIndex: codeUnits.index(i.codeUnitIndex, offsetBy: d),
        scalar: Encoding.ReverseDecoder.decodeOne(scalarContent),
        stride: numericCast(scalarContent.count))
    case .invalid(let stride):
      let d: Int = -numericCast(stride)
      return Index(
        codeUnitIndex: codeUnits.index(i.codeUnitIndex, offsetBy: d) ,
        scalar: UnicodeScalar(_unchecked: 0xfffd),
        stride: numericCast(stride))
    case .emptyInput: fatalError("index out of bounds.")
    }
  }
}

public protocol UnicodeEncoding {
  associatedtype CodeUnit
  
  associatedtype EncodedScalar
    where CodeUnit == EncodedScalar.Iterator.Element

  static var encodedReplacementScalar : EncodedScalar { get }
  static func decode(_ content: EncodedScalar) -> UnicodeScalar
  
  associatedtype ForwardDecoder : UnicodeDecoder
  where EncodedScalar == ForwardDecoder.EncodedScalar
  
  associatedtype ReverseDecoder : UnicodeDecoder
  where EncodedScalar == ReverseDecoder.EncodedScalar
}

internal protocol _UTFEncoding : UnicodeEncoding {
  static func _isScalar(_: CodeUnit) -> Bool
}

public protocol _UTFDecoderBase : UnicodeDecoder {

  associatedtype Buffer : RangeReplaceableCollection = EncodedScalar
  var buffer: Buffer { get set }

  associatedtype BufferStorage : UnsignedInteger, FixedWidthInteger = UInt32
}

public protocol _UTFDecoder : _UTFDecoderBase
where Buffer == _UIntBuffer<BufferStorage, CodeUnit>, Buffer == EncodedScalar {
  static func _isScalar(_: CodeUnit) -> Bool
  func _parseMultipleCodeUnits() -> Unicode.ParseResult<EncodedScalar>
}

extension _UTFEncoding {
  public mutating func parseScalar<I : IteratorProtocol, Decoder: _UTFDecoder>(
    from input: inout I, with decoder: inout Decoder
  ) -> Unicode.ParseResult<EncodedScalar>
    where I.Element == CodeUnit {

    // Bufferless single-scalar fastpath.
    if _fastPath(buffer.isEmpty) {
      guard let codeUnit = input.next() else { return .emptyInput }
      // ASCII, return immediately.
      if Self._isScalar(codeUnit) {
        return (
          EncodedScalar(containing: codeUnit),
          consumedCodeUnits: 1, isValid: true)
      }
      // Non-ASCII, proceed to buffering mode.
      buffer.append(codeUnit)
    } else if Self._isScalar(CodeUnit(truncatingIfNeeded: buffer._storage)) {
      // ASCII in buffer.  We don't refill the buffer so we can return
      // to bufferless mode once we've exhausted it.
      let codeUnit = CodeUnit(truncatingIfNeeded: buffer._storage)
      buffer.remove(at: buffer.startIndex)
      return (
          EncodedScalar(containing: codeUnit),
          consumedCodeUnits: 1, isValid: true)
    }
    // Buffering mode.
    // Fill buffer back to 4 bytes (or as many as are left in the iterator).
    _sanityCheck(buffer._bitCount < BufferStorage.bitWidth)
    repeat {
      if let codeUnit = input.next() {
        buffer.append(codeUnit)
      } else {
        if buffer.isEmpty { return .emptyInput }
        break // We still have some bytes left in our buffer.
      }
    } while buffer._bitCount < BufferStorage.bitWidth

    // Find one unicode scalar.
    return _parseMultipleCodeUnits()
  }
}

//===----------------------------------------------------------------------===//
//===--- UTF8 Decoders ----------------------------------------------------===//
//===----------------------------------------------------------------------===//

public protocol _UTF8Decoder : _UTFDecoder {}

extension _UTF8Decoder {
  public static func _isScalar(_ x: CodeUnit) -> Bool { return x & 0x80 == 0 }
}

extension Unicode.UTF8 : UnicodeEncoding {
  public typealias EncodedScalar = _UIntBuffer<UInt32, UInt8>
  public static var encodedReplacementScalar : EncodedScalar {
    return EncodedScalar(_storage: 0xbdbfef, _bitCount: 24)    
  }
  
  public struct ForwardDecoder {
    public typealias Buffer = _UIntBuffer<UInt32, UInt8>
    public typealias EncodedScalar = _UIntBuffer<UInt32, UInt8>
    public init() {  }
    public var buffer = Buffer()
  }
  
  public struct ReverseDecoder {
    public typealias Buffer = _UIntBuffer<UInt32, UInt8>
    public typealias EncodedScalar = _UIntBuffer<UInt32, UInt8>
    public init() {  }
    public var buffer = Buffer()
  }

  public static func decode(_ source: EncodedScalar) -> UnicodeScalar {
    let bits = source._storage
    switch source._bitCount {
    case 8:
      return UnicodeScalar(_unchecked: bits)
    case 16:
      var value = (bits & 0b0_______________________11_1111__0000_0000) &>> 8
      value    |= (bits & 0b0________________________________0001_1111) &<< 6
      return UnicodeScalar(_unchecked: value)
    case 24:
      var value = (bits & 0b0____________11_1111__0000_0000__0000_0000) &>> 16
      value    |= (bits & 0b0_______________________11_1111__0000_0000) &>> 2
      value    |= (bits & 0b0________________________________0000_1111) &<< 12
      return UnicodeScalar(_unchecked: value)
    default:
      _sanityCheck(source.count == 4)
      var value = (bits & 0b0_11_1111__0000_0000__0000_0000__0000_0000) &>> 24
      value    |= (bits & 0b0____________11_1111__0000_0000__0000_0000) &>> 10
      value    |= (bits & 0b0_______________________11_1111__0000_0000) &<< 4
      value    |= (bits & 0b0________________________________0000_0111) &<< 18
      return UnicodeScalar(_unchecked: value)
    }
  }
}

extension Unicode.UTF8.ReverseDecoder : _UTF8Decoder {
  public typealias CodeUnit = UInt8

  @inline(__always)
  @usableFromInline
  internal mutating func _consumeCodeUnits(_ n: UInt8) -> EncodedScalar {
    let s = buffer._storage
    let bitCount = n &* UInt8(CodeUnit.bitWidth)
    buffer._storage >>= bitCount
    buffer._bitCount -= bitCount
    return EncodedScalar(
      _storage: s.byteSwapped >> (type(of: s).bitWidth - bitCount),
      _bitCount: bitCount)
  }

  @inline(__always)
  @usableFromInline
  internal mutating func _consumeValidCodeUnits(
    _ n: UInt8
  ) -> Unicode.ParseResult<EncodedScalar> {
    return ParseResult(
      _consumeCodeUnits(n), consumedCodeUnits: n, isValid: true)
  }

  @inline(__always)
  @usableFromInline
  internal mutating func _consumeInvalidCodeUnits(
    _ n: UInt8
  ) -> Unicode.ParseResult<EncodedScalar> {
    _ = _consumeCodeUnits(n)
    return ParseResult(
      UTF8.encodedReplacementScalar, consumedCodeUnits: n, isValid: false)
  }
  
  public // @testable
  func _parseMultipleCodeUnits() -> Unicode.ParseResult<EncodedScalar> {
    _sanityCheck(buffer._storage & 0x80 != 0) // this case handled elsewhere

    if buffer._storage                & 0b0__1110_0000__1100_0000
                                     == 0b0__1100_0000__1000_0000 {
      // 2-byte sequence.  Top 4 bits of decoded result must be nonzero
      let top4Bits =  buffer._storage & 0b0__0001_1110__0000_0000
      if _fastPath(top4Bits != 0) {
        return _consumeValidCodeUnits(2)
      }
    }
    else if buffer._storage     & 0b0__1111_0000__1100_0000__1100_0000
                               == 0b0__1110_0000__1000_0000__1000_0000 {
      // 3-byte sequence. The top 5 bits of the decoded result must be nonzero
      // and not a surrogate
      let top5Bits = buffer._storage & 0b0__1111__0010_0000__0000_0000
      if _fastPath(
        top5Bits != 0 &&   top5Bits != 0b0__1101__0010_0000__0000_0000) {
        return _consumeValidCodeUnits(3)
      }
    }
    else if buffer._storage  & 0b0__1111_1000__1100_0000__1100_0000__1100_0000
                            == 0b0__1111_0000__1000_0000__1000_0000__1000_0000 {
      // Make sure the top 5 bits of the decoded result would be in range
      let top5bits = buffer._storage
                                  & 0b0__0111__0011_0000__0000_0000__0000_0000
      if _fastPath(
        top5bits != 0
        && top5bits <=              0b0__0100__0000_0000__0000_0000__0000_0000
      ) {
        return _consumeValidCodeUnits(4)
      }
    }
    return _parseInvalid()
  }

  @inline(never)
  mutating func _parseInvalid() -> Unicode.ParseResult<EncodedScalar> {
    if buffer._storage                 & 0b0__1111_0000__1100_0000
                                      == 0b0__1110_0000__1000_0000 {
      // 2-byte prefix of 3-byte sequence. The top 5 bits of the decoded result
      // must be nonzero and not a surrogate
      let top5Bits = buffer._storage        & 0b0__1111__0010_0000
      if top5Bits != 0 &&         top5Bits != 0b0__1101__0010_0000 {
        return invalid(codeUnitCount: 2)
      }
    }
    else if buffer._storage            & 0b0__1111_1000__1100_0000
                                      == 0b0__1111_0000__1000_0000
    {
      // 2-byte prefix of 4-byte sequence
      // Make sure the top 5 bits of the decoded result would be in range
      let top5bits =        buffer._storage & 0b0__0111__0011_0000
      if top5bits != 0 &&         top5bits <= 0b0__0100__0000_0000 {
        return invalid(codeUnitCount: 2)
      }
    }
    else if buffer._storage & 0b0__1111_1000__1100_0000__1100_0000
                           == 0b0__1111_0000__1000_0000__1000_0000 {
      // 3-byte prefix of 4-byte sequence
      // Make sure the top 5 bits of the decoded result would be in range
      let top5bits =        buffer._storage & 0b0__0111__0011_0000__0000_0000
      if top5bits != 0 &&         top5bits <= 0b0__0100__0000_0000__0000_0000 {
        return invalid(codeUnitCount: 3)
      }
    }
    return invalid(codeUnitCount: 1)
  }
}

extension Unicode.UTF8.ForwardDecoder : _UTF8Decoder {
  public typealias CodeUnit = UInt8
  
  @inline(__always)
  @usableFromInline
  internal mutating func _consumeCodeUnits(_ n: UInt8) -> EncodedScalar {
    let s = buffer._storage
    let bitCount = n &* UInt8(CodeUnit.bitWidth)
    buffer._storage >>= bitCount
    buffer._bitCount -= bitCount
    return EncodedScalar(_storage: s, _bitCount: bitCount)
  }
  
  @inline(__always)
  @usableFromInline
  internal mutating func _consumeValidCodeUnits(
    _ n: UInt8
  ) -> Unicode.ParseResult<EncodedScalar> {
    return ParseResult(
      _consumeCodeUnits(codeUnitCount, consumedCodeUnits: n, isValid: true))
  }
  
  @inline(__always)
  @usableFromInline
  internal func _consumeInvalidCodeUnits(
    codeUnitCount n: UInt8
  ) -> Unicode.ParseResult<EncodedScalar> {
    _ = _consumeCodeUnits(n)
    return ParseResult(
        UTF8.encodedReplacementScalar, consumedCodeUnits: n, isValid: false)
    }
  
  public // @testable
  func _parseMultipleCodeUnits() -> Unicode.ParseResult<EncodedScalar> {
    _sanityCheck(buffer._storage & 0x80 != 0) // this case handled elsewhere
    
    if buffer._storage & 0b0__1100_0000__1110_0000
                      == 0b0__1000_0000__1100_0000 {
      // 2-byte sequence. At least one of the top 4 bits of the decoded result
      // must be nonzero.
      if _fastPath(buffer._storage & 0b0_0001_1110 != 0) {
        return _consumeValidCodeUnits(2)
      }
    }
    else if buffer._storage         & 0b0__1100_0000__1100_0000__1111_0000
                                   == 0b0__1000_0000__1000_0000__1110_0000 {
      // 3-byte sequence. The top 5 bits of the decoded result must be nonzero
      // and not a surrogate
      let top5Bits =          buffer._storage & 0b0___0010_0000__0000_1111
      if _fastPath(top5Bits != 0 && top5Bits != 0b0___0010_0000__0000_1101) {
        return _consumeValidCodeUnits(3)
      }
    }
    else if buffer._storage & 0b0__1100_0000__1100_0000__1100_0000__1111_1000
                           == 0b0__1000_0000__1000_0000__1000_0000__1111_0000 {
      // 4-byte sequence.  The top 5 bits of the decoded result must be nonzero
      // and no greater than 0b0__0100_0000
      let top5bits = UInt16(buffer._storage       & 0b0__0011_0000__0000_0111)
      if _fastPath(
        top5bits != 0 && top5bits.byteSwapped    <= 0b0__0000_0100__0000_0000
      ) {
        return _consumeValidCodeUnits(4)
      }
    }
    return _parseInvalid()
  }

  @inline(never)
  mutating func _parseInvalid() -> Unicode.ParseResult<EncodedScalar> {
    
    if buffer._storage               & 0b0__1100_0000__1111_0000
                                    == 0b0__1000_0000__1110_0000 {
      // 2-byte prefix of 3-byte sequence. The top 5 bits of the decoded result
      // must be nonzero and not a surrogate
      let top5Bits = buffer._storage & 0b0__0010_0000__0000_1111
      if top5Bits != 0 && top5Bits  != 0b0__0010_0000__0000_1101 {
        return _consumeInvalidCodeUnits(2)
      }
    }
    else if buffer._storage          & 0b0__1100_0000__1111_1000
                                    == 0b0__1000_0000__1111_0000
    {
      // Prefix of 4-byte sequence. The top 5 bits of the decoded result
      // must be nonzero and no greater than 0b0__0100_0000
      let top5bits = UInt16(buffer._storage & 0b0__0011_0000__0000_0111)
      if top5bits != 0 && top5bits.byteSwapped <= 0b0__0000_0100__0000_0000 {
        return _consumeInvalidCodeUnits(
          buffer._storage   & 0b0__1100_0000__0000_0000__0000_0000
                           == 0b0__1000_0000__0000_0000__0000_0000 ? 3 : 2)
      }
    }
    return 1
  }  
}

//===----------------------------------------------------------------------===//
//===--- UTF-16 Decoders --------------------------------------------------===//
//===----------------------------------------------------------------------===//

public protocol _UTF16Decoder : _UTFDecoder where CodeUnit == UTF16.CodeUnit {
  var buffer: Buffer { get set }
  static var _surrogatePattern : UInt32 { get }
}

extension _UTF16Decoder {
  public static func _isScalar(_ x: CodeUnit) -> Bool {
    return x & 0xf800 != 0xd800
  }
  
  internal mutating func _consume(bitCount: UInt8) -> EncodedScalar {
    _sanityCheck(bitCount == 16)
    let s = buffer._storage
    buffer._storage = 0
    buffer._bitCount = 0
    return EncodedScalar(_storage: s, _bitCount: bitCount)
  }
  
  public // @testable
  func _parseMultipleCodeUnits() -> (isValid: Bool, bitCount: UInt8) {
    _sanityCheck(  // this case handled elsewhere
      !Self._isScalar(UInt16(truncatingIfNeeded: buffer._storage)))
    
    if _fastPath(buffer._storage & 0xFC00_FC00 == Self._surrogatePattern) {
      return (true, 2*16)
    }
    return (false, 1*16)
  }
}

extension Unicode.UTF16 : UnicodeEncoding {
  public typealias EncodedScalar = _UIntBuffer<UInt32, UInt16>
  public static var encodedReplacementScalar : EncodedScalar {
    return EncodedScalar(_storage: 0xFFFD, _bitCount: 16)
  }
  
  public struct ForwardDecoder {
    public typealias Buffer = _UIntBuffer<UInt32, UInt16>
    public init() { buffer = Buffer() }
    public var buffer: Buffer
  }
  
  public struct ReverseDecoder {
    public typealias Buffer = _UIntBuffer<UInt32, UInt16>
    public init() { buffer = Buffer() }
    public var buffer: Buffer
  }

  public static func decode(_ source: EncodedScalar) -> UnicodeScalar {
    let bits = source._storage
    if _fastPath(source._bitCount == 16) {
      return UnicodeScalar(_unchecked: bits & 0xffff)
    }
    _sanityCheck(source._bitCount == 32)
    let value = 0x10000 + (bits >> 16 & 0x03ff | (bits & 0x03ff) << 10)
    return UnicodeScalar(_unchecked: value)
  }  
}

extension UTF16.ReverseDecoder : _UTF16Decoder {
  public typealias CodeUnit = UInt16
  public typealias EncodedScalar = Buffer

  public static var _surrogatePattern : UInt32 { return 0xD800_DC00 }  
}

extension Unicode.UTF16.ForwardDecoder : _UTF16Decoder {
  public typealias CodeUnit = UInt16
  public typealias EncodedScalar = Buffer
  
  public static var _surrogatePattern : UInt32 { return 0xDC00_D800 }  
}

#if !BENCHMARK
//===--- testing ----------------------------------------------------------===//
import StdlibUnittest
import SwiftPrivate

func checkDecodeUTF<Codec : UnicodeCodec & UnicodeEncoding>(
    _ codec: Codec.Type, _ expectedHead: [UInt32],
    _ expectedRepairedTail: [UInt32], _ utfStr: [Codec.CodeUnit]
) -> AssertionResult {
  var decoded = [UInt32]()
  var expected = expectedHead
  func output(_ scalar: UInt32) { decoded.append(scalar) }
  func output1(_ scalar: UnicodeScalar) { decoded.append(scalar.value) }
  
  var result = assertionSuccess()
  
  func check<C: Collection>(_ expected: C, _ description: String)
  where C.Iterator.Element == UInt32
  {
    if !expected.elementsEqual(decoded) {
      if result.description == "" { result = assertionFailure()  }
      result = result.withDescription(" [\(description)]\n")
        .withDescription("expected: \(asHex(expectedHead))\n")
        .withDescription("actual:       \(asHex(decoded))")
    }
    decoded.removeAll(keepingCapacity: true)
  }

  //===--- Tests without repairs ------------------------------------------===//
  do {
    let iterator = utfStr.makeIterator()
    _ = transcode(
      iterator, from: codec, to: UTF32.self,
      stoppingOnError: true, into: output)
  }
  check(expected, "legacy, repairing: false")

  do {
    var iterator = utfStr.makeIterator()
    let errorCount = Codec.ForwardDecoder.decode(
      &iterator, repairingIllFormedSequences: false, into: output1)
    expectEqual(expectedRepairedTail.isEmpty ? 0 : 1, errorCount)
  }
  check(expected, "forward, repairing: false")

  do {
    var iterator = utfStr.reversed().makeIterator()
    let errorCount = Codec.ReverseDecoder.decode(
      &iterator, repairingIllFormedSequences: false, into: output1)
    if expectedRepairedTail.isEmpty {
      expectEqual(0, errorCount)
      check(expected.reversed(), "reverse, repairing: false")
    }
    else {
      expectEqual(1, errorCount)
      let x = (expected + expectedRepairedTail).reversed()
      expectTrue(
        x.starts(with: decoded),
        "reverse, repairing: false\n\t\(Array(x)) does not start with \(decoded)")
      decoded.removeAll(keepingCapacity: true)
    }
  }

  //===--- Tests with repairs ------------------------------------------===//
  expected += expectedRepairedTail
  do {
    let iterator = utfStr.makeIterator()
    _ = transcode(iterator, from: codec, to: UTF32.self,
      stoppingOnError: false, into: output)
  }
  check(expected, "legacy, repairing: true")
  do {
    var iterator = utfStr.makeIterator()
    let errorCount = Codec.ForwardDecoder.decode(
      &iterator, repairingIllFormedSequences: true, into: output1)
    
    if expectedRepairedTail.isEmpty { expectEqual(0, errorCount) }
    else { expectNotEqual(0, errorCount) }
  }
  check(expected, "forward, repairing: true")
  do {
    var iterator = utfStr.reversed().makeIterator()
    let errorCount = Codec.ReverseDecoder.decode(
      &iterator, repairingIllFormedSequences: true, into: output1)
    if expectedRepairedTail.isEmpty { expectEqual(0, errorCount) }
    else { expectNotEqual(0, errorCount) }
  }
  check(expected.reversed(), "reverse, repairing: true")

  let scalars = Unicode.DefaultScalarView(utfStr, fromEncoding: Codec.self)
  expectEqualSequence(expected, scalars.map { $0.value })
  expectEqualSequence(
    expected.reversed(),
    scalars.reversed().map { $0.value })

  do {
    var x = scalars.makeIterator()
    var j = scalars.startIndex
    while (j != scalars.endIndex) {
      expectEqual(x.next()!, scalars[j])
      j = scalars.index(after: j)
    }
    expectNil(x.next())
  }
  return result
}

func checkDecodeUTF8(
    _ expectedHead: [UInt32],
    _ expectedRepairedTail: [UInt32], _ utf8Str: [UInt8]
) -> AssertionResult {
  return checkDecodeUTF(UTF8.self, expectedHead, expectedRepairedTail, utf8Str)
}

func checkDecodeUTF16(
    _ expectedHead: [UInt32],
    _ expectedRepairedTail: [UInt32], _ utf16Str: [UInt16]
) -> AssertionResult {
  return checkDecodeUTF(UTF16.self, expectedHead, expectedRepairedTail,
      utf16Str)
}

/*
func checkDecodeUTF32(
    _ expectedHead: [UInt32],
    _ expectedRepairedTail: [UInt32], _ utf32Str: [UInt32]
) -> AssertionResult {
  return checkDecodeUTF(UTF32.self, expectedHead, expectedRepairedTail,
      utf32Str)
}
*/

func checkEncodeUTF8(_ expected: [UInt8],
                     _ scalars: [UInt32]) -> AssertionResult {
  var encoded = [UInt8]()
  let output: (UInt8) -> Void = { encoded.append($0) }
  let iterator = scalars.makeIterator()
  let hadError = transcode(
    iterator,
    from: UTF32.self,
    to: UTF8.self,
    stoppingOnError: true,
    into: output)
  expectFalse(hadError)
  if expected != encoded {
    return assertionFailure()
        .withDescription("\n")
        .withDescription("expected: \(asHex(expected))\n")
        .withDescription("actual:   \(asHex(encoded))")
  }

  return assertionSuccess()
}

var UTF8Decoder = TestSuite("UTF8Decoder")

//===----------------------------------------------------------------------===//
public struct UTFTest {
  public struct Flags : OptionSet {
    public let rawValue: Int

    public init(rawValue: Int) {
      self.rawValue = rawValue
    }

    public static let utf8IsInvalid = Flags(rawValue: 1 << 0)
    public static let utf16IsInvalid = Flags(rawValue: 1 << 1)
  }

  public let string: String
  public let utf8: [UInt8]
  public let utf16: [UInt16]
  public let unicodeScalars: [UnicodeScalar]
  public let unicodeScalarsRepairedTail: [UnicodeScalar]
  public let flags: Flags
  public let loc: SourceLoc

  public var utf32: [UInt32] {
    return unicodeScalars.map(UInt32.init)
  }

  public var utf32RepairedTail: [UInt32] {
    return unicodeScalarsRepairedTail.map(UInt32.init)
  }

  public init(
    string: String,
    utf8: [UInt8],
    utf16: [UInt16],
    scalars: [UInt32],
    scalarsRepairedTail: [UInt32] = [],
    flags: Flags = [],
    file: String = #file, line: UInt = #line
  ) {
    self.string = string
    self.utf8 = utf8
    self.utf16 = utf16
    self.unicodeScalars = scalars.map { UnicodeScalar($0)! }
    self.unicodeScalarsRepairedTail =
      scalarsRepairedTail.map { UnicodeScalar($0)! }
    self.flags = flags
    self.loc = SourceLoc(file, line, comment: "test data")
  }
}

public var utfTests: [UTFTest] = []
  //
  // Empty sequence.
  //

utfTests.append(
  UTFTest(
    string: "",
    utf8: [],
    utf16: [],
    scalars: []))

  //
  // 1-byte sequences.
  //

  // U+0000 NULL
utfTests.append(
  UTFTest(
    string: "\u{0000}",
    utf8: [ 0x00 ],
    utf16: [ 0x00 ],
    scalars: [ 0x00 ]))

  // U+0041 LATIN CAPITAL LETTER A
utfTests.append(
  UTFTest(
    string: "A",
    utf8: [ 0x41 ],
    utf16: [ 0x41 ],
    scalars: [ 0x41 ]))

  // U+0041 LATIN CAPITAL LETTER A
  // U+0042 LATIN CAPITAL LETTER B
utfTests.append(
  UTFTest(
    string: "AB",
    utf8: [ 0x41, 0x42 ],
    utf16: [ 0x41, 0x42 ],
    scalars: [ 0x41, 0x42 ]))

  // U+0061 LATIN SMALL LETTER A
  // U+0062 LATIN SMALL LETTER B
  // U+0063 LATIN SMALL LETTER C
utfTests.append(
  UTFTest(
    string: "ABC",
    utf8: [ 0x41, 0x42, 0x43 ],
    utf16: [ 0x41, 0x42, 0x43 ],
    scalars: [ 0x41, 0x42, 0x43 ]))

  // U+0000 NULL
  // U+0041 LATIN CAPITAL LETTER A
  // U+0042 LATIN CAPITAL LETTER B
  // U+0000 NULL
utfTests.append(
  UTFTest(
    string: "\u{0000}AB\u{0000}",
    utf8: [ 0x00, 0x41, 0x42, 0x00 ],
    utf16: [ 0x00, 0x41, 0x42, 0x00 ],
    scalars: [ 0x00, 0x41, 0x42, 0x00 ]))

  // U+007F DELETE
utfTests.append(
  UTFTest(
    string: "\u{007F}",
    utf8: [ 0x7F ],
    utf16: [ 0x7F ],
    scalars: [ 0x7F ]))

  //
  // 2-byte sequences.
  //

  // U+0283 LATIN SMALL LETTER ESH
utfTests.append(
  UTFTest(
    string: "\u{0283}",
    utf8: [ 0xCA, 0x83 ],
    utf16: [ 0x0283 ],
    scalars: [ 0x0283 ]))

  // U+03BA GREEK SMALL LETTER KAPPA
  // U+1F79 GREEK SMALL LETTER OMICRON WITH OXIA
  // U+03C3 GREEK SMALL LETTER SIGMA
  // U+03BC GREEK SMALL LETTER MU
  // U+03B5 GREEK SMALL LETTER EPSILON
utfTests.append(
  UTFTest(
    string: "\u{03BA}\u{1F79}\u{03C3}\u{03BC}\u{03B5}",
    utf8: [ 0xCE, 0xBA, 0xE1, 0xBD, 0xB9, 0xCF, 0x83, 0xCE, 0xBC, 0xCE, 0xB5 ],
    utf16: [ 0x03BA, 0x1F79, 0x03C3, 0x03BC, 0x03B5 ],
    scalars: [ 0x03BA, 0x1F79, 0x03C3, 0x03BC, 0x03B5 ]))

  // U+0430 CYRILLIC SMALL LETTER A
  // U+0431 CYRILLIC SMALL LETTER BE
  // U+0432 CYRILLIC SMALL LETTER VE
utfTests.append(
  UTFTest(
    string: "\u{0430}\u{0431}\u{0432}",
    utf8: [ 0xD0, 0xB0, 0xD0, 0xB1, 0xD0, 0xB2 ],
    utf16: [ 0x0430, 0x0431, 0x0432 ],
    scalars: [ 0x0430, 0x0431, 0x0432 ]))

  //
  // 3-byte sequences.
  //

  // U+4F8B CJK UNIFIED IDEOGRAPH-4F8B
  // U+6587 CJK UNIFIED IDEOGRAPH-6587
utfTests.append(
  UTFTest(
    string: "\u{4F8b}\u{6587}",
    utf8: [ 0xE4, 0xBE, 0x8B, 0xE6, 0x96, 0x87 ],
    utf16: [ 0x4F8B, 0x6587 ],
    scalars: [ 0x4F8B, 0x6587 ]))

  // U+D55C HANGUL SYLLABLE HAN
  // U+AE00 HANGUL SYLLABLE GEUL
utfTests.append(
  UTFTest(
    string: "\u{d55c}\u{ae00}",
    utf8: [ 0xED, 0x95, 0x9C, 0xEA, 0xB8, 0x80 ],
    utf16: [ 0xD55C, 0xAE00 ],
    scalars: [ 0xD55C, 0xAE00 ]))

  // U+1112 HANGUL CHOSEONG HIEUH
  // U+1161 HANGUL JUNGSEONG A
  // U+11AB HANGUL JONGSEONG NIEUN
  // U+1100 HANGUL CHOSEONG KIYEOK
  // U+1173 HANGUL JUNGSEONG EU
  // U+11AF HANGUL JONGSEONG RIEUL
utfTests.append(
  UTFTest(
    string: "\u{1112}\u{1161}\u{11ab}\u{1100}\u{1173}\u{11af}",
    utf8:
      [ 0xE1, 0x84, 0x92, 0xE1, 0x85, 0xA1, 0xE1, 0x86, 0xAB,
        0xE1, 0x84, 0x80, 0xE1, 0x85, 0xB3, 0xE1, 0x86, 0xAF ],
    utf16: [ 0x1112, 0x1161, 0x11AB, 0x1100, 0x1173, 0x11AF ],
    scalars: [ 0x1112, 0x1161, 0x11AB, 0x1100, 0x1173, 0x11AF ]))

  // U+3042 HIRAGANA LETTER A
  // U+3044 HIRAGANA LETTER I
  // U+3046 HIRAGANA LETTER U
  // U+3048 HIRAGANA LETTER E
  // U+304A HIRAGANA LETTER O
utfTests.append(
  UTFTest(
    string: "\u{3042}\u{3044}\u{3046}\u{3048}\u{304a}",
    utf8:
      [ 0xE3, 0x81, 0x82, 0xE3, 0x81, 0x84, 0xE3, 0x81, 0x86,
        0xE3, 0x81, 0x88, 0xE3, 0x81, 0x8A ],
    utf16: [ 0x3042, 0x3044, 0x3046, 0x3048, 0x304A ],
    scalars: [ 0x3042, 0x3044, 0x3046, 0x3048, 0x304A ]))

  // U+D7FF (unassigned)
utfTests.append(
  UTFTest(
    string: "\u{D7FF}",
    utf8: [ 0xED, 0x9F, 0xBF ],
    utf16: [ 0xD7FF ],
    scalars: [ 0xD7FF ]))

  // U+E000 (private use)
utfTests.append(
  UTFTest(
    string: "\u{E000}",
    utf8: [ 0xEE, 0x80, 0x80 ],
    utf16: [ 0xE000 ],
    scalars: [ 0xE000 ]))

  // U+FFFD REPLACEMENT CHARACTER
utfTests.append(
  UTFTest(
    string: "\u{FFFD}",
    utf8: [ 0xEF, 0xBF, 0xBD ],
    utf16: [ 0xFFFD ],
    scalars: [ 0xFFFD ]))

  // U+FFFF (noncharacter)
utfTests.append(
  UTFTest(
    string: "\u{FFFF}",
    utf8: [ 0xEF, 0xBF, 0xBF ],
    utf16: [ 0xFFFF ],
    scalars: [ 0xFFFF ]))

  //
  // 4-byte sequences.
  //

  // U+1F425 FRONT-FACING BABY CHICK
utfTests.append(
  UTFTest(
    string: "\u{1F425}",
    utf8: [ 0xF0, 0x9F, 0x90, 0xA5 ],
    utf16: [ 0xD83D, 0xDC25 ],
    scalars: [ 0x0001_F425 ]))

  // U+0041 LATIN CAPITAL LETTER A
  // U+1F425 FRONT-FACING BABY CHICK
utfTests.append(
  UTFTest(
    string: "A\u{1F425}",
    utf8: [ 0x41, 0xF0, 0x9F, 0x90, 0xA5 ],
    utf16: [ 0x41, 0xD83D, 0xDC25 ],
    scalars: [ 0x41, 0x0001_F425 ]))

  // U+0041 LATIN CAPITAL LETTER A
  // U+0042 LATIN CAPITAL LETTER B
  // U+1F425 FRONT-FACING BABY CHICK
utfTests.append(
  UTFTest(
    string: "AB\u{1F425}",
    utf8: [ 0x41, 0x42, 0xF0, 0x9F, 0x90, 0xA5 ],
    utf16: [ 0x41, 0x42, 0xD83D, 0xDC25 ],
    scalars: [ 0x41, 0x42, 0x0001_F425 ]))

  // U+0041 LATIN CAPITAL LETTER A
  // U+0042 LATIN CAPITAL LETTER B
  // U+0043 LATIN CAPITAL LETTER C
  // U+1F425 FRONT-FACING BABY CHICK
utfTests.append(
  UTFTest(
    string: "ABC\u{1F425}",
    utf8: [ 0x41, 0x42, 0x43, 0xF0, 0x9F, 0x90, 0xA5 ],
    utf16: [ 0x41, 0x42, 0x43, 0xD83D, 0xDC25 ],
    scalars: [ 0x41, 0x42, 0x43, 0x0001_F425 ]))

  // U+0041 LATIN CAPITAL LETTER A
  // U+0042 LATIN CAPITAL LETTER B
  // U+0043 LATIN CAPITAL LETTER C
  // U+0044 LATIN CAPITAL LETTER D
  // U+1F425 FRONT-FACING BABY CHICK
utfTests.append(
  UTFTest(
    string: "ABCD\u{1F425}",
    utf8: [ 0x41, 0x42, 0x43, 0x44, 0xF0, 0x9F, 0x90, 0xA5 ],
    utf16: [ 0x41, 0x42, 0x43, 0x44, 0xD83D, 0xDC25 ],
    scalars: [ 0x41, 0x42, 0x43, 0x44, 0x0001_F425 ]))

  // U+0041 LATIN CAPITAL LETTER A
  // U+0042 LATIN CAPITAL LETTER B
  // U+0043 LATIN CAPITAL LETTER C
  // U+0044 LATIN CAPITAL LETTER D
  // U+0045 LATIN CAPITAL LETTER E
  // U+1F425 FRONT-FACING BABY CHICK
utfTests.append(
  UTFTest(
    string: "ABCDE\u{1F425}",
    utf8: [ 0x41, 0x42, 0x43, 0x44, 0x45, 0xF0, 0x9F, 0x90, 0xA5 ],
    utf16: [ 0x41, 0x42, 0x43, 0x44, 0x45, 0xD83D, 0xDC25 ],
    scalars: [ 0x41, 0x42, 0x43, 0x44, 0x45, 0x0001_F425 ]))

  // U+0041 LATIN CAPITAL LETTER A
  // U+0042 LATIN CAPITAL LETTER B
  // U+0043 LATIN CAPITAL LETTER C
  // U+0044 LATIN CAPITAL LETTER D
  // U+0045 LATIN CAPITAL LETTER E
  // U+0046 LATIN CAPITAL LETTER F
  // U+1F425 FRONT-FACING BABY CHICK
utfTests.append(
  UTFTest(
    string: "ABCDEF\u{1F425}",
    utf8: [ 0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0xF0, 0x9F, 0x90, 0xA5 ],
    utf16: [ 0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0xD83D, 0xDC25 ],
    scalars: [ 0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x0001_F425 ]))

  // U+0041 LATIN CAPITAL LETTER A
  // U+0042 LATIN CAPITAL LETTER B
  // U+0043 LATIN CAPITAL LETTER C
  // U+0044 LATIN CAPITAL LETTER D
  // U+0045 LATIN CAPITAL LETTER E
  // U+0046 LATIN CAPITAL LETTER F
  // U+0047 LATIN CAPITAL LETTER G
  // U+1F425 FRONT-FACING BABY CHICK
utfTests.append(
  UTFTest(
    string: "ABCDEFG\u{1F425}",
    utf8: [ 0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47, 0xF0, 0x9F, 0x90, 0xA5 ],
    utf16: [ 0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47, 0xD83D, 0xDC25 ],
    scalars: [ 0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47, 0x0001_F425 ]))

  // U+0041 LATIN CAPITAL LETTER A
  // U+0042 LATIN CAPITAL LETTER B
  // U+0043 LATIN CAPITAL LETTER C
  // U+0044 LATIN CAPITAL LETTER D
  // U+0045 LATIN CAPITAL LETTER E
  // U+0046 LATIN CAPITAL LETTER F
  // U+0047 LATIN CAPITAL LETTER G
  // U+0048 LATIN CAPITAL LETTER H
  // U+1F425 FRONT-FACING BABY CHICK
utfTests.append(
  UTFTest(
    string: "ABCDEFGH\u{1F425}",
    utf8:
      [ 0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47, 0x48,
        0xF0, 0x9F, 0x90, 0xA5 ],
    utf16:
      [ 0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47, 0x48,
        0xD83D, 0xDC25 ],
    scalars:
      [ 0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47, 0x48, 0x0001_F425 ]))

  // U+0041 LATIN CAPITAL LETTER A
  // U+0042 LATIN CAPITAL LETTER B
  // U+0043 LATIN CAPITAL LETTER C
  // U+0044 LATIN CAPITAL LETTER D
  // U+0045 LATIN CAPITAL LETTER E
  // U+0046 LATIN CAPITAL LETTER F
  // U+0047 LATIN CAPITAL LETTER G
  // U+0048 LATIN CAPITAL LETTER H
  // U+0049 LATIN CAPITAL LETTER I
  // U+1F425 FRONT-FACING BABY CHICK
utfTests.append(
  UTFTest(
    string: "ABCDEFGHI\u{1F425}",
    utf8:
      [ 0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47, 0x48, 0x49,
        0xF0, 0x9F, 0x90, 0xA5 ],
    utf16:
      [ 0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47, 0x48, 0x49,
        0xD83D, 0xDC25 ],
    scalars:
      [ 0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47, 0x48, 0x49, 0x0001_F425 ]))

  // U+10000 LINEAR B SYLLABLE B008 A
utfTests.append(
  UTFTest(
    string: "\u{10000}",
    utf8: [ 0xF0, 0x90, 0x80, 0x80 ],
    utf16: [ 0xD800, 0xDC00 ],
    scalars: [ 0x0001_0000 ]))

  // U+10100 AEGEAN WORD SEPARATOR LINE
utfTests.append(
  UTFTest(
    string: "\u{10100}",
    utf8: [ 0xF0, 0x90, 0x84, 0x80 ],
    utf16: [ 0xD800, 0xDD00 ],
    scalars: [ 0x0001_0100 ]))

  // U+103FF (unassigned)
utfTests.append(
  UTFTest(
    string: "\u{103FF}",
    utf8: [ 0xF0, 0x90, 0x8F, 0xBF ],
    utf16: [ 0xD800, 0xDFFF ],
    scalars: [ 0x0001_03FF ]))

  // U+E0000 (unassigned)
utfTests.append(
  UTFTest(
    string: "\u{E0000}",
    utf8: [ 0xF3, 0xA0, 0x80, 0x80 ],
    utf16: [ 0xDB40, 0xDC00 ],
    scalars: [ 0x000E_0000 ]))

  // U+E0100 VARIATION SELECTOR-17
utfTests.append(
  UTFTest(
    string: "\u{E0100}",
    utf8: [ 0xF3, 0xA0, 0x84, 0x80 ],
    utf16: [ 0xDB40, 0xDD00 ],
    scalars: [ 0x000E_0100 ]))

  // U+E03FF (unassigned)
utfTests.append(
  UTFTest(
    string: "\u{E03FF}",
    utf8: [ 0xF3, 0xA0, 0x8F, 0xBF ],
    utf16: [ 0xDB40, 0xDFFF ],
    scalars: [ 0x000E_03FF ]))

  // U+10FC00 (private use)
utfTests.append(
  UTFTest(
    string: "\u{10FC00}",
    utf8: [ 0xF4, 0x8F, 0xB0, 0x80 ],
    utf16: [ 0xDBFF, 0xDC00 ],
    scalars: [ 0x0010_FC00 ]))

  // U+10FD00 (private use)
utfTests.append(
  UTFTest(
    string: "\u{10FD00}",
    utf8: [ 0xF4, 0x8F, 0xB4, 0x80 ],
    utf16: [ 0xDBFF, 0xDD00 ],
    scalars: [ 0x0010_FD00 ]))

  // U+10FFFF (private use, noncharacter)
utfTests.append(
  UTFTest(
    string: "\u{10FFFF}",
    utf8: [ 0xF4, 0x8F, 0xBF, 0xBF ],
    utf16: [ 0xDBFF, 0xDFFF ],
    scalars: [ 0x0010_FFFF ]))
//===----------------------------------------------------------------------===//

UTF8Decoder.test("SmokeTest").forEach(in: utfTests) {
  test in

  expectTrue(
    checkDecodeUTF8(test.utf32, [], test.utf8),
    stackTrace: test.loc.withCurrentLoc())
  return ()
}

UTF8Decoder.test("FirstPossibleSequence") {
  //
  // First possible sequence of a certain length
  //

  // U+0000 NULL
  expectTrue(checkDecodeUTF8([ 0x0000 ], [], [ 0x00 ]))

  // U+0080 PADDING CHARACTER
  expectTrue(checkDecodeUTF8([ 0x0080 ], [], [ 0xc2, 0x80 ]))

  // U+0800 SAMARITAN LETTER ALAF
  expectTrue(checkDecodeUTF8(
      [ 0x0800 ], [],
      [ 0xe0, 0xa0, 0x80 ]))

  // U+10000 LINEAR B SYLLABLE B008 A
  expectTrue(checkDecodeUTF8(
      [ 0x10000 ], [],
      [ 0xf0, 0x90, 0x80, 0x80 ]))

  // U+200000 (invalid)
  expectTrue(checkDecodeUTF8(
      [], [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xf8, 0x88, 0x80, 0x80, 0x80 ]))

  // U+4000000 (invalid)
  expectTrue(checkDecodeUTF8(
      [], [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xfc, 0x84, 0x80, 0x80, 0x80, 0x80 ]))
}

UTF8Decoder.test("LastPossibleSequence") {
  //
  // Last possible sequence of a certain length
  //

  // U+007F DELETE
  expectTrue(checkDecodeUTF8([ 0x007f ], [], [ 0x7f ]))

  // U+07FF (unassigned)
  expectTrue(checkDecodeUTF8([ 0x07ff ], [], [ 0xdf, 0xbf ]))

  // U+FFFF (noncharacter)
  expectTrue(checkDecodeUTF8(
      [ 0xffff ], [],
      [ 0xef, 0xbf, 0xbf ]))

  // U+1FFFFF (invalid)
  expectTrue(checkDecodeUTF8(
      [], [ 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xf7, 0xbf, 0xbf, 0xbf ]))

  // U+3FFFFFF (invalid)
  expectTrue(checkDecodeUTF8(
      [], [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xfb, 0xbf, 0xbf, 0xbf, 0xbf ]))

  // U+7FFFFFFF (invalid)
  expectTrue(checkDecodeUTF8(
      [], [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xfd, 0xbf, 0xbf, 0xbf, 0xbf, 0xbf ]))
}

UTF8Decoder.test("CodeSpaceBoundaryConditions") {
  //
  // Other boundary conditions
  //

  // U+D7FF (unassigned)
  expectTrue(checkDecodeUTF8([ 0xd7ff ], [], [ 0xed, 0x9f, 0xbf ]))

  // U+E000 (private use)
  expectTrue(checkDecodeUTF8([ 0xe000 ], [], [ 0xee, 0x80, 0x80 ]))

  // U+FFFD REPLACEMENT CHARACTER
  expectTrue(checkDecodeUTF8([ 0xfffd ], [], [ 0xef, 0xbf, 0xbd ]))

  // U+10FFFF (noncharacter)
  expectTrue(checkDecodeUTF8([ 0x10ffff ], [], [ 0xf4, 0x8f, 0xbf, 0xbf ]))
  
  // U+110000 (invalid)
  expectTrue(checkDecodeUTF8(
      [], [ 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xf4, 0x90, 0x80, 0x80 ]))
}

UTF8Decoder.test("UnexpectedContinuationBytes") {
  //
  // Unexpected continuation bytes
  //

  // A sequence of unexpected continuation bytes that don't follow a first
  // byte, every byte is a maximal subpart.

  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0x80 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xbf ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0x80, 0x80 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0x80, 0xbf ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xbf, 0x80 ]))
  expectTrue(checkDecodeUTF8(
      [], [ 0xfffd, 0xfffd, 0xfffd ],
      [ 0x80, 0xbf, 0x80 ]))
  expectTrue(checkDecodeUTF8(
      [], [ 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0x80, 0xbf, 0x80, 0xbf ]))
  expectTrue(checkDecodeUTF8(
      [], [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0x80, 0xbf, 0x82, 0xbf, 0xaa ]))
  expectTrue(checkDecodeUTF8(
      [], [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xaa, 0xb0, 0xbb, 0xbf, 0xaa, 0xa0 ]))
  expectTrue(checkDecodeUTF8(
      [], [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xaa, 0xb0, 0xbb, 0xbf, 0xaa, 0xa0, 0x8f ]))

  // All continuation bytes (0x80--0xbf).
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd,
        0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd,
        0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd,
        0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd,
        0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd,
        0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd,
        0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd,
        0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0x80, 0x81, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87,
        0x88, 0x89, 0x8a, 0x8b, 0x8c, 0x8d, 0x8e, 0x8f,
        0x90, 0x91, 0x92, 0x93, 0x94, 0x95, 0x96, 0x97,
        0x98, 0x99, 0x9a, 0x9b, 0x9c, 0x9d, 0x9e, 0x9f,
        0xa0, 0xa1, 0xa2, 0xa3, 0xa4, 0xa5, 0xa6, 0xa7,
        0xa8, 0xa9, 0xaa, 0xab, 0xac, 0xad, 0xae, 0xaf,
        0xb0, 0xb1, 0xb2, 0xb3, 0xb4, 0xb5, 0xb6, 0xb7,
        0xb8, 0xb9, 0xba, 0xbb, 0xbc, 0xbd, 0xbe, 0xbf ]))
}

UTF8Decoder.test("LonelyStartBytes") {
  //
  // Lonely start bytes
  //

  // Start bytes of 2-byte sequences (0xc0--0xdf).
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd,
        0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd,
        0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd,
        0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xc0, 0xc1, 0xc2, 0xc3, 0xc4, 0xc5, 0xc6, 0xc7,
        0xc8, 0xc9, 0xca, 0xcb, 0xcc, 0xcd, 0xce, 0xcf,
        0xd0, 0xd1, 0xd2, 0xd3, 0xd4, 0xd5, 0xd6, 0xd7,
        0xd8, 0xd9, 0xda, 0xdb, 0xdc, 0xdd, 0xde, 0xdf ]))

  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020,
        0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020,
        0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020,
        0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020,
        0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020,
        0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020,
        0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020,
        0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020 ],
      [ 0xc0, 0x20, 0xc1, 0x20, 0xc2, 0x20, 0xc3, 0x20,
        0xc4, 0x20, 0xc5, 0x20, 0xc6, 0x20, 0xc7, 0x20,
        0xc8, 0x20, 0xc9, 0x20, 0xca, 0x20, 0xcb, 0x20,
        0xcc, 0x20, 0xcd, 0x20, 0xce, 0x20, 0xcf, 0x20,
        0xd0, 0x20, 0xd1, 0x20, 0xd2, 0x20, 0xd3, 0x20,
        0xd4, 0x20, 0xd5, 0x20, 0xd6, 0x20, 0xd7, 0x20,
        0xd8, 0x20, 0xd9, 0x20, 0xda, 0x20, 0xdb, 0x20,
        0xdc, 0x20, 0xdd, 0x20, 0xde, 0x20, 0xdf, 0x20 ]))

  // Start bytes of 3-byte sequences (0xe0--0xef).
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd,
        0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xe0, 0xe1, 0xe2, 0xe3, 0xe4, 0xe5, 0xe6, 0xe7,
        0xe8, 0xe9, 0xea, 0xeb, 0xec, 0xed, 0xee, 0xef ]))

  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020,
        0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020,
        0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020,
        0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020 ],
      [ 0xe0, 0x20, 0xe1, 0x20, 0xe2, 0x20, 0xe3, 0x20,
        0xe4, 0x20, 0xe5, 0x20, 0xe6, 0x20, 0xe7, 0x20,
        0xe8, 0x20, 0xe9, 0x20, 0xea, 0x20, 0xeb, 0x20,
        0xec, 0x20, 0xed, 0x20, 0xee, 0x20, 0xef, 0x20 ]))

  // Start bytes of 4-byte sequences (0xf0--0xf7).
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xf0, 0xf1, 0xf2, 0xf3, 0xf4, 0xf5, 0xf6, 0xf7 ]))

  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020,
        0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020 ],
      [ 0xf0, 0x20, 0xf1, 0x20, 0xf2, 0x20, 0xf3, 0x20,
        0xf4, 0x20, 0xf5, 0x20, 0xf6, 0x20, 0xf7, 0x20 ]))

  // Start bytes of 5-byte sequences (0xf8--0xfb).
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xf8, 0xf9, 0xfa, 0xfb ]))

  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020 ],
      [ 0xf8, 0x20, 0xf9, 0x20, 0xfa, 0x20, 0xfb, 0x20 ]))

  // Start bytes of 6-byte sequences (0xfc--0xfd).
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xfc, 0xfd ]))

  expectTrue(checkDecodeUTF8(
      [], [ 0xfffd, 0x0020, 0xfffd, 0x0020 ],
      [ 0xfc, 0x20, 0xfd, 0x20 ]))
}

UTF8Decoder.test("InvalidStartBytes") {
  //
  // Other bytes (0xc0--0xc1, 0xfe--0xff).
  //

  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xc0 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xc1 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xfe ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xff ]))

  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xc0, 0xc1, 0xfe, 0xff ]))

  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xfe, 0xfe, 0xff, 0xff ]))

  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xfe, 0x80, 0x80, 0x80, 0x80, 0x80 ]))

  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xff, 0x80, 0x80, 0x80, 0x80, 0x80 ]))

  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020 ],
      [ 0xc0, 0x20, 0xc1, 0x20, 0xfe, 0x20, 0xff, 0x20 ]))
}

UTF8Decoder.test("MissingContinuationBytes") {
  //
  // Sequences with one continuation byte missing
  //

  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xc2 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xdf ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0x0041 ], [ 0xc2, 0x41 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0x0041 ], [ 0xdf, 0x41 ]))

  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xe0, 0xa0 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xe0, 0xbf ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0x0041 ], [ 0xe0, 0xa0, 0x41 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0x0041 ], [ 0xe0, 0xbf, 0x41 ]))

  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xe1, 0x80 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xec, 0xbf ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0x0041 ], [ 0xe1, 0x80, 0x41 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0x0041 ], [ 0xec, 0xbf, 0x41 ]))

  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xed, 0x80 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xed, 0x9f ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0x0041 ], [ 0xed, 0x80, 0x41 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0x0041 ], [ 0xed, 0x9f, 0x41 ]))

  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xee, 0x80 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xef, 0xbf ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0x0041 ], [ 0xee, 0x80, 0x41 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0x0041 ], [ 0xef, 0xbf, 0x41 ]))

  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xf0, 0x90, 0x80 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xf0, 0xbf, 0xbf ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0x0041 ], [ 0xf0, 0x90, 0x80, 0x41 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0x0041 ], [ 0xf0, 0xbf, 0xbf, 0x41 ]))

  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xf1, 0x80, 0x80 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xf3, 0xbf, 0xbf ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0x0041 ], [ 0xf1, 0x80, 0x80, 0x41 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0x0041 ], [ 0xf3, 0xbf, 0xbf, 0x41 ]))

  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xf4, 0x80, 0x80 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xf4, 0x8f, 0xbf ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0x0041 ], [ 0xf4, 0x80, 0x80, 0x41 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0x0041 ], [ 0xf4, 0x8f, 0xbf, 0x41 ]))

  // Overlong sequences with one trailing byte missing.
  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xc0 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xc1 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xe0, 0x80 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xe0, 0x9f ]))
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd ],
      [ 0xf0, 0x80, 0x80 ]))
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd ],
      [ 0xf0, 0x8f, 0x80 ]))
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xf8, 0x80, 0x80, 0x80 ]))
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xfc, 0x80, 0x80, 0x80, 0x80 ]))

  // Sequences that represent surrogates with one trailing byte missing.
  // High-surrogates
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xed, 0xa0 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xed, 0xac ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xed, 0xaf ]))
  // Low-surrogates
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xed, 0xb0 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xed, 0xb4 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xed, 0xbf ]))

  // Ill-formed 4-byte sequences.
  // 11110zzz 10zzyyyy 10yyyyxx 10xxxxxx
  // U+1100xx (invalid)
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd ],
      [ 0xf4, 0x90, 0x80 ]))
  // U+13FBxx (invalid)
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd ],
      [ 0xf4, 0xbf, 0xbf ]))
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd ],
      [ 0xf5, 0x80, 0x80 ]))
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd ],
      [ 0xf6, 0x80, 0x80 ]))
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd ],
      [ 0xf7, 0x80, 0x80 ]))
  // U+1FFBxx (invalid)
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd ],
      [ 0xf7, 0xbf, 0xbf ]))

  // Ill-formed 5-byte sequences.
  // 111110uu 10zzzzzz 10zzyyyy 10yyyyxx 10xxxxxx
  // U+2000xx (invalid)
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xf8, 0x88, 0x80, 0x80 ]))
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xf8, 0xbf, 0xbf, 0xbf ]))
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xf9, 0x80, 0x80, 0x80 ]))
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xfa, 0x80, 0x80, 0x80 ]))
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xfb, 0x80, 0x80, 0x80 ]))
  // U+3FFFFxx (invalid)
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xfb, 0xbf, 0xbf, 0xbf ]))

  // Ill-formed 6-byte sequences.
  // 1111110u 10uuuuuu 10uzzzzz 10zzzyyyy 10yyyyxx 10xxxxxx
  // U+40000xx (invalid)
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xfc, 0x84, 0x80, 0x80, 0x80 ]))
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xfc, 0xbf, 0xbf, 0xbf, 0xbf ]))
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xfd, 0x80, 0x80, 0x80, 0x80 ]))
  // U+7FFFFFxx (invalid)
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xfd, 0xbf, 0xbf, 0xbf, 0xbf ]))

  //
  // Sequences with two continuation bytes missing
  //

  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xf0, 0x90 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xf0, 0xbf ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xf1, 0x80 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xf3, 0xbf ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xf4, 0x80 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xf4, 0x8f ]))

  // Overlong sequences with two trailing byte missing.
  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xe0 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xf0, 0x80 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xf0, 0x8f ]))
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd ],
      [ 0xf8, 0x80, 0x80 ]))
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xfc, 0x80, 0x80, 0x80 ]))

  // Sequences that represent surrogates with two trailing bytes missing.
  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xed ]))

  // Ill-formed 4-byte sequences.
  // 11110zzz 10zzyyyy 10yyyyxx 10xxxxxx
  // U+110yxx (invalid)
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xf4, 0x90 ]))
  // U+13Fyxx (invalid)
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xf4, 0xbf ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xf5, 0x80 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xf6, 0x80 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xf7, 0x80 ]))
  // U+1FFyxx (invalid)
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xf7, 0xbf ]))

  // Ill-formed 5-byte sequences.
  // 111110uu 10zzzzzz 10zzyyyy 10yyyyxx 10xxxxxx
  // U+200yxx (invalid)
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd ],
      [ 0xf8, 0x88, 0x80 ]))
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd ],
      [ 0xf8, 0xbf, 0xbf ]))
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd ],
      [ 0xf9, 0x80, 0x80 ]))
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd ],
      [ 0xfa, 0x80, 0x80 ]))
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd ],
      [ 0xfb, 0x80, 0x80 ]))
  // U+3FFFyxx (invalid)
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd ],
      [ 0xfb, 0xbf, 0xbf ]))

  // Ill-formed 6-byte sequences.
  // 1111110u 10uuuuuu 10zzzzzz 10zzyyyy 10yyyyxx 10xxxxxx
  // U+4000yxx (invalid)
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xfc, 0x84, 0x80, 0x80 ]))
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xfc, 0xbf, 0xbf, 0xbf ]))
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xfd, 0x80, 0x80, 0x80 ]))
  // U+7FFFFyxx (invalid)
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xfd, 0xbf, 0xbf, 0xbf ]))

  //
  // Sequences with three continuation bytes missing
  //

  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xf0 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xf1 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xf2 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xf3 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xf4 ]))

  // Broken overlong sequences.
  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xf0 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xf8, 0x80 ]))
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd ],
      [ 0xfc, 0x80, 0x80 ]))

  // Ill-formed 4-byte sequences.
  // 11110zzz 10zzyyyy 10yyyyxx 10xxxxxx
  // U+14yyxx (invalid)
  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xf5 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xf6 ]))
  // U+1Cyyxx (invalid)
  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xf7 ]))

  // Ill-formed 5-byte sequences.
  // 111110uu 10zzzzzz 10zzyyyy 10yyyyxx 10xxxxxx
  // U+20yyxx (invalid)
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xf8, 0x88 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xf8, 0xbf ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xf9, 0x80 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xfa, 0x80 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xfb, 0x80 ]))
  // U+3FCyyxx (invalid)
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xfb, 0xbf ]))

  // Ill-formed 6-byte sequences.
  // 1111110u 10uuuuuu 10zzzzzz 10zzyyyy 10yyyyxx 10xxxxxx
  // U+400yyxx (invalid)
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd ],
      [ 0xfc, 0x84, 0x80 ]))
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd ],
      [ 0xfc, 0xbf, 0xbf ]))
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd ],
      [ 0xfd, 0x80, 0x80 ]))
  // U+7FFCyyxx (invalid)
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd ],
      [ 0xfd, 0xbf, 0xbf ]))

  //
  // Sequences with four continuation bytes missing
  //

  // Ill-formed 5-byte sequences.
  // 111110uu 10zzzzzz 10zzyyyy 10yyyyxx 10xxxxxx
  // U+uzyyxx (invalid)
  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xf8 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xf9 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xfa ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xfb ]))
  // U+3zyyxx (invalid)
  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xfb ]))

  // Broken overlong sequences.
  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xf8 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xfc, 0x80 ]))

  // Ill-formed 6-byte sequences.
  // 1111110u 10uuuuuu 10zzzzzz 10zzyyyy 10yyyyxx 10xxxxxx
  // U+uzzyyxx (invalid)
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xfc, 0x84 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xfc, 0xbf ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xfd, 0x80 ]))
  // U+7Fzzyyxx (invalid)
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xfd, 0xbf ]))

  //
  // Sequences with five continuation bytes missing
  //

  // Ill-formed 6-byte sequences.
  // 1111110u 10uuuuuu 10zzzzzz 10zzyyyy 10yyyyxx 10xxxxxx
  // U+uzzyyxx (invalid)
  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xfc ]))
  // U+uuzzyyxx (invalid)
  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xfd ]))

  //
  // Consecutive sequences with trailing bytes missing
  //

  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, /**/ 0xfffd, 0xfffd, /**/ 0xfffd, 0xfffd, 0xfffd,
        0xfffd, 0xfffd, 0xfffd, 0xfffd,
        0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd,
        0xfffd, /**/ 0xfffd, /**/ 0xfffd, 0xfffd, 0xfffd,
        0xfffd, 0xfffd, 0xfffd, 0xfffd,
        0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xc0, /**/ 0xe0, 0x80, /**/ 0xf0, 0x80, 0x80,
        0xf8, 0x80, 0x80, 0x80,
        0xfc, 0x80, 0x80, 0x80, 0x80,
        0xdf, /**/ 0xef, 0xbf, /**/ 0xf7, 0xbf, 0xbf,
        0xfb, 0xbf, 0xbf, 0xbf,
        0xfd, 0xbf, 0xbf, 0xbf, 0xbf ]))
}

UTF8Decoder.test("OverlongSequences") {
  //
  // Overlong UTF-8 sequences
  //

  // U+002F SOLIDUS
  expectTrue(checkDecodeUTF8([ 0x002f ], [], [ 0x2f ]))

  // Overlong sequences of the above.
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xc0, 0xaf ]))
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd ],
      [ 0xe0, 0x80, 0xaf ]))
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xf0, 0x80, 0x80, 0xaf ]))
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xf8, 0x80, 0x80, 0x80, 0xaf ]))
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xfc, 0x80, 0x80, 0x80, 0x80, 0xaf ]))

  // U+0000 NULL
  expectTrue(checkDecodeUTF8([ 0x0000 ], [], [ 0x00 ]))

  // Overlong sequences of the above.
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xc0, 0x80 ]))
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd ],
      [ 0xe0, 0x80, 0x80 ]))
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xf0, 0x80, 0x80, 0x80 ]))
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xf8, 0x80, 0x80, 0x80, 0x80 ]))
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xfc, 0x80, 0x80, 0x80, 0x80, 0x80 ]))

  // Other overlong and ill-formed sequences.
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xc0, 0xbf ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xc1, 0x80 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xc1, 0xbf ]))
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd ],
      [ 0xe0, 0x9f, 0xbf ]))
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd ],
      [ 0xed, 0xa0, 0x80 ]))
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd ],
      [ 0xed, 0xbf, 0xbf ]))
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xf0, 0x8f, 0x80, 0x80 ]))
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xf0, 0x8f, 0xbf, 0xbf ]))
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xf8, 0x87, 0xbf, 0xbf, 0xbf ]))
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xfc, 0x83, 0xbf, 0xbf, 0xbf, 0xbf ]))
}

UTF8Decoder.test("IsolatedSurrogates") {
  // Unicode 6.3.0:
  //
  //    D71.  High-surrogate code point: A Unicode code point in the range
  //    U+D800 to U+DBFF.
  //
  //    D73.  Low-surrogate code point: A Unicode code point in the range
  //    U+DC00 to U+DFFF.

  // Note: U+E0100 is <DB40 DD00> in UTF-16.

  // High-surrogates

  // U+D800
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd ],
      [ 0xed, 0xa0, 0x80 ]))
  expectTrue(checkDecodeUTF8(
      [ 0x0041 ],
      [ 0xfffd, 0xfffd, 0xfffd, 0x0041 ],
      [ 0x41, 0xed, 0xa0, 0x80, 0x41 ]))

  // U+DB40
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd ],
      [ 0xed, 0xac, 0xa0 ]))

  // U+DBFF
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd ],
      [ 0xed, 0xaf, 0xbf ]))

  // Low-surrogates

  // U+DC00
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd ],
      [ 0xed, 0xb0, 0x80 ]))

  // U+DD00
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd ],
      [ 0xed, 0xb4, 0x80 ]))

  // U+DFFF
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd ],
      [ 0xed, 0xbf, 0xbf ]))
}

UTF8Decoder.test("SurrogatePairs") {
  // Surrogate pairs

  // U+D800 U+DC00
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xed, 0xa0, 0x80, 0xed, 0xb0, 0x80 ]))

  // U+D800 U+DD00
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xed, 0xa0, 0x80, 0xed, 0xb4, 0x80 ]))

  // U+D800 U+DFFF
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xed, 0xa0, 0x80, 0xed, 0xbf, 0xbf ]))

  // U+DB40 U+DC00
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xed, 0xac, 0xa0, 0xed, 0xb0, 0x80 ]))

  // U+DB40 U+DD00
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xed, 0xac, 0xa0, 0xed, 0xb4, 0x80 ]))

  // U+DB40 U+DFFF
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xed, 0xac, 0xa0, 0xed, 0xbf, 0xbf ]))

  // U+DBFF U+DC00
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xed, 0xaf, 0xbf, 0xed, 0xb0, 0x80 ]))

  // U+DBFF U+DD00
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xed, 0xaf, 0xbf, 0xed, 0xb4, 0x80 ]))

  // U+DBFF U+DFFF
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xed, 0xaf, 0xbf, 0xed, 0xbf, 0xbf ]))
}

UTF8Decoder.test("Noncharacters") {
  //
  // Noncharacters
  //

  // Unicode 6.3.0:
  //
  //    D14.  Noncharacter: A code point that is permanently reserved for
  //    internal use and that should never be interchanged. Noncharacters
  //    consist of the values U+nFFFE and U+nFFFF (where n is from 0 to 1016)
  //    and the values U+FDD0..U+FDEF.

  // U+FFFE
  expectTrue(checkDecodeUTF8([ 0xfffe ], [], [ 0xef, 0xbf, 0xbe ]))

  // U+FFFF
  expectTrue(checkDecodeUTF8([ 0xffff ], [], [ 0xef, 0xbf, 0xbf ]))

  // U+1FFFE
  expectTrue(checkDecodeUTF8([ 0x1fffe ], [], [ 0xf0, 0x9f, 0xbf, 0xbe ]))

  // U+1FFFF
  expectTrue(checkDecodeUTF8([ 0x1ffff ], [], [ 0xf0, 0x9f, 0xbf, 0xbf ]))

  // U+2FFFE
  expectTrue(checkDecodeUTF8([ 0x2fffe ], [], [ 0xf0, 0xaf, 0xbf, 0xbe ]))

  // U+2FFFF
  expectTrue(checkDecodeUTF8([ 0x2ffff ], [], [ 0xf0, 0xaf, 0xbf, 0xbf ]))

  // U+3FFFE
  expectTrue(checkDecodeUTF8([ 0x3fffe ], [], [ 0xf0, 0xbf, 0xbf, 0xbe ]))

  // U+3FFFF
  expectTrue(checkDecodeUTF8([ 0x3ffff ], [], [ 0xf0, 0xbf, 0xbf, 0xbf ]))

  // U+4FFFE
  expectTrue(checkDecodeUTF8([ 0x4fffe ], [], [ 0xf1, 0x8f, 0xbf, 0xbe ]))

  // U+4FFFF
  expectTrue(checkDecodeUTF8([ 0x4ffff ], [], [ 0xf1, 0x8f, 0xbf, 0xbf ]))

  // U+5FFFE
  expectTrue(checkDecodeUTF8([ 0x5fffe ], [], [ 0xf1, 0x9f, 0xbf, 0xbe ]))

  // U+5FFFF
  expectTrue(checkDecodeUTF8([ 0x5ffff ], [], [ 0xf1, 0x9f, 0xbf, 0xbf ]))

  // U+6FFFE
  expectTrue(checkDecodeUTF8([ 0x6fffe ], [], [ 0xf1, 0xaf, 0xbf, 0xbe ]))

  // U+6FFFF
  expectTrue(checkDecodeUTF8([ 0x6ffff ], [], [ 0xf1, 0xaf, 0xbf, 0xbf ]))

  // U+7FFFE
  expectTrue(checkDecodeUTF8([ 0x7fffe ], [], [ 0xf1, 0xbf, 0xbf, 0xbe ]))

  // U+7FFFF
  expectTrue(checkDecodeUTF8([ 0x7ffff ], [], [ 0xf1, 0xbf, 0xbf, 0xbf ]))

  // U+8FFFE
  expectTrue(checkDecodeUTF8([ 0x8fffe ], [], [ 0xf2, 0x8f, 0xbf, 0xbe ]))

  // U+8FFFF
  expectTrue(checkDecodeUTF8([ 0x8ffff ], [], [ 0xf2, 0x8f, 0xbf, 0xbf ]))

  // U+9FFFE
  expectTrue(checkDecodeUTF8([ 0x9fffe ], [], [ 0xf2, 0x9f, 0xbf, 0xbe ]))

  // U+9FFFF
  expectTrue(checkDecodeUTF8([ 0x9ffff ], [], [ 0xf2, 0x9f, 0xbf, 0xbf ]))

  // U+AFFFE
  expectTrue(checkDecodeUTF8([ 0xafffe ], [], [ 0xf2, 0xaf, 0xbf, 0xbe ]))

  // U+AFFFF
  expectTrue(checkDecodeUTF8([ 0xaffff ], [], [ 0xf2, 0xaf, 0xbf, 0xbf ]))

  // U+BFFFE
  expectTrue(checkDecodeUTF8([ 0xbfffe ], [], [ 0xf2, 0xbf, 0xbf, 0xbe ]))

  // U+BFFFF
  expectTrue(checkDecodeUTF8([ 0xbffff ], [], [ 0xf2, 0xbf, 0xbf, 0xbf ]))

  // U+CFFFE
  expectTrue(checkDecodeUTF8([ 0xcfffe ], [], [ 0xf3, 0x8f, 0xbf, 0xbe ]))

  // U+CFFFF
  expectTrue(checkDecodeUTF8([ 0xcfffF ], [], [ 0xf3, 0x8f, 0xbf, 0xbf ]))

  // U+DFFFE
  expectTrue(checkDecodeUTF8([ 0xdfffe ], [], [ 0xf3, 0x9f, 0xbf, 0xbe ]))

  // U+DFFFF
  expectTrue(checkDecodeUTF8([ 0xdffff ], [], [ 0xf3, 0x9f, 0xbf, 0xbf ]))

  // U+EFFFE
  expectTrue(checkDecodeUTF8([ 0xefffe ], [], [ 0xf3, 0xaf, 0xbf, 0xbe ]))

  // U+EFFFF
  expectTrue(checkDecodeUTF8([ 0xeffff ], [], [ 0xf3, 0xaf, 0xbf, 0xbf ]))

  // U+FFFFE
  expectTrue(checkDecodeUTF8([ 0xffffe ], [], [ 0xf3, 0xbf, 0xbf, 0xbe ]))

  // U+FFFFF
  expectTrue(checkDecodeUTF8([ 0xfffff ], [], [ 0xf3, 0xbf, 0xbf, 0xbf ]))

  // U+10FFFE
  expectTrue(checkDecodeUTF8([ 0x10fffe ], [], [ 0xf4, 0x8f, 0xbf, 0xbe ]))

  // U+10FFFF
  expectTrue(checkDecodeUTF8([ 0x10ffff ], [], [ 0xf4, 0x8f, 0xbf, 0xbf ]))

  // U+FDD0
  expectTrue(checkDecodeUTF8([ 0xfdd0 ], [], [ 0xef, 0xb7, 0x90 ]))

  // U+FDD1
  expectTrue(checkDecodeUTF8([ 0xfdd1 ], [], [ 0xef, 0xb7, 0x91 ]))

  // U+FDD2
  expectTrue(checkDecodeUTF8([ 0xfdd2 ], [], [ 0xef, 0xb7, 0x92 ]))

  // U+FDD3
  expectTrue(checkDecodeUTF8([ 0xfdd3 ], [], [ 0xef, 0xb7, 0x93 ]))

  // U+FDD4
  expectTrue(checkDecodeUTF8([ 0xfdd4 ], [], [ 0xef, 0xb7, 0x94 ]))

  // U+FDD5
  expectTrue(checkDecodeUTF8([ 0xfdd5 ], [], [ 0xef, 0xb7, 0x95 ]))

  // U+FDD6
  expectTrue(checkDecodeUTF8([ 0xfdd6 ], [], [ 0xef, 0xb7, 0x96 ]))

  // U+FDD7
  expectTrue(checkDecodeUTF8([ 0xfdd7 ], [], [ 0xef, 0xb7, 0x97 ]))

  // U+FDD8
  expectTrue(checkDecodeUTF8([ 0xfdd8 ], [], [ 0xef, 0xb7, 0x98 ]))

  // U+FDD9
  expectTrue(checkDecodeUTF8([ 0xfdd9 ], [], [ 0xef, 0xb7, 0x99 ]))

  // U+FDDA
  expectTrue(checkDecodeUTF8([ 0xfdda ], [], [ 0xef, 0xb7, 0x9a ]))

  // U+FDDB
  expectTrue(checkDecodeUTF8([ 0xfddb ], [], [ 0xef, 0xb7, 0x9b ]))

  // U+FDDC
  expectTrue(checkDecodeUTF8([ 0xfddc ], [], [ 0xef, 0xb7, 0x9c ]))

  // U+FDDD
  expectTrue(checkDecodeUTF8([ 0xfddd ], [], [ 0xef, 0xb7, 0x9d ]))

  // U+FDDE
  expectTrue(checkDecodeUTF8([ 0xfdde ], [], [ 0xef, 0xb7, 0x9e ]))

  // U+FDDF
  expectTrue(checkDecodeUTF8([ 0xfddf ], [], [ 0xef, 0xb7, 0x9f ]))

  // U+FDE0
  expectTrue(checkDecodeUTF8([ 0xfde0 ], [], [ 0xef, 0xb7, 0xa0 ]))

  // U+FDE1
  expectTrue(checkDecodeUTF8([ 0xfde1 ], [], [ 0xef, 0xb7, 0xa1 ]))

  // U+FDE2
  expectTrue(checkDecodeUTF8([ 0xfde2 ], [], [ 0xef, 0xb7, 0xa2 ]))

  // U+FDE3
  expectTrue(checkDecodeUTF8([ 0xfde3 ], [], [ 0xef, 0xb7, 0xa3 ]))

  // U+FDE4
  expectTrue(checkDecodeUTF8([ 0xfde4 ], [], [ 0xef, 0xb7, 0xa4 ]))

  // U+FDE5
  expectTrue(checkDecodeUTF8([ 0xfde5 ], [], [ 0xef, 0xb7, 0xa5 ]))

  // U+FDE6
  expectTrue(checkDecodeUTF8([ 0xfde6 ], [], [ 0xef, 0xb7, 0xa6 ]))

  // U+FDE7
  expectTrue(checkDecodeUTF8([ 0xfde7 ], [], [ 0xef, 0xb7, 0xa7 ]))

  // U+FDE8
  expectTrue(checkDecodeUTF8([ 0xfde8 ], [], [ 0xef, 0xb7, 0xa8 ]))

  // U+FDE9
  expectTrue(checkDecodeUTF8([ 0xfde9 ], [], [ 0xef, 0xb7, 0xa9 ]))

  // U+FDEA
  expectTrue(checkDecodeUTF8([ 0xfdea ], [], [ 0xef, 0xb7, 0xaa ]))

  // U+FDEB
  expectTrue(checkDecodeUTF8([ 0xfdeb ], [], [ 0xef, 0xb7, 0xab ]))

  // U+FDEC
  expectTrue(checkDecodeUTF8([ 0xfdec ], [], [ 0xef, 0xb7, 0xac ]))

  // U+FDED
  expectTrue(checkDecodeUTF8([ 0xfded ], [], [ 0xef, 0xb7, 0xad ]))

  // U+FDEE
  expectTrue(checkDecodeUTF8([ 0xfdee ], [], [ 0xef, 0xb7, 0xae ]))

  // U+FDEF
  expectTrue(checkDecodeUTF8([ 0xfdef ], [], [ 0xef, 0xb7, 0xaf ]))

  // U+FDF0
  expectTrue(checkDecodeUTF8([ 0xfdf0 ], [], [ 0xef, 0xb7, 0xb0 ]))

  // U+FDF1
  expectTrue(checkDecodeUTF8([ 0xfdf1 ], [], [ 0xef, 0xb7, 0xb1 ]))

  // U+FDF2
  expectTrue(checkDecodeUTF8([ 0xfdf2 ], [], [ 0xef, 0xb7, 0xb2 ]))

  // U+FDF3
  expectTrue(checkDecodeUTF8([ 0xfdf3 ], [], [ 0xef, 0xb7, 0xb3 ]))

  // U+FDF4
  expectTrue(checkDecodeUTF8([ 0xfdf4 ], [], [ 0xef, 0xb7, 0xb4 ]))

  // U+FDF5
  expectTrue(checkDecodeUTF8([ 0xfdf5 ], [], [ 0xef, 0xb7, 0xb5 ]))

  // U+FDF6
  expectTrue(checkDecodeUTF8([ 0xfdf6 ], [], [ 0xef, 0xb7, 0xb6 ]))

  // U+FDF7
  expectTrue(checkDecodeUTF8([ 0xfdf7 ], [], [ 0xef, 0xb7, 0xb7 ]))

  // U+FDF8
  expectTrue(checkDecodeUTF8([ 0xfdf8 ], [], [ 0xef, 0xb7, 0xb8 ]))

  // U+FDF9
  expectTrue(checkDecodeUTF8([ 0xfdf9 ], [], [ 0xef, 0xb7, 0xb9 ]))

  // U+FDFA
  expectTrue(checkDecodeUTF8([ 0xfdfa ], [], [ 0xef, 0xb7, 0xba ]))

  // U+FDFB
  expectTrue(checkDecodeUTF8([ 0xfdfb ], [], [ 0xef, 0xb7, 0xbb ]))

  // U+FDFC
  expectTrue(checkDecodeUTF8([ 0xfdfc ], [], [ 0xef, 0xb7, 0xbc ]))

  // U+FDFD
  expectTrue(checkDecodeUTF8([ 0xfdfd ], [], [ 0xef, 0xb7, 0xbd ]))

  // U+FDFE
  expectTrue(checkDecodeUTF8([ 0xfdfe ], [], [ 0xef, 0xb7, 0xbe ]))

  // U+FDFF
  expectTrue(checkDecodeUTF8([ 0xfdff ], [], [ 0xef, 0xb7, 0xbf ]))
}

var UTF16Decoder = TestSuite("UTF16Decoder")

UTF16Decoder.test("UTF16.transcodedLength") {
  do {
    let u8: [UTF8.CodeUnit] = [ 0, 1, 2, 3, 4, 5 ]
    let (count, isASCII) = UTF16.transcodedLength(
      of: u8.makeIterator(),
      decodedAs: UTF8.self,
      repairingIllFormedSequences: false)!
    expectEqual(6, count)
    expectTrue(isASCII)
  }

  do {
    // "€" == U+20AC.
    let u8: [UTF8.CodeUnit] = [ 0xF0, 0xA4, 0xAD, 0xA2 ]
    let (count, isASCII) = UTF16.transcodedLength(
      of: u8.makeIterator(),
      decodedAs: UTF8.self,
      repairingIllFormedSequences: false)!
    expectEqual(2, count)
    expectFalse(isASCII)
  }

  do {
    let u16: [UTF16.CodeUnit] = [ 6, 7, 8, 9, 10, 11 ]
    let (count, isASCII) = UTF16.transcodedLength(
      of: u16.makeIterator(),
      decodedAs: UTF16.self,
      repairingIllFormedSequences: false)!
    expectEqual(6, count)
    expectTrue(isASCII)
  }
}

UTF16Decoder.test("Decoding1").forEach(in: utfTests) {
  test in

  expectTrue(
    checkDecodeUTF16(
      test.utf32, test.utf32RepairedTail, test.utf16),
    stackTrace: test.loc.withCurrentLoc())
  return ()
}

UTF16Decoder.test("Decoding2") {
  for (name, batch) in utf16Tests {
    print("Batch: \(name)")
    for test in batch {
      expectTrue(checkDecodeUTF16(test.scalarsHead, test.scalarsRepairedTail,
          test.encoded), stackTrace: test.loc.withCurrentLoc())
    }
  }
}

public struct UTF16Test {
  public let scalarsHead: [UInt32]
  public let scalarsRepairedTail: [UInt32]
  public let encoded: [UInt16]
  public let loc: SourceLoc

  public init(
    _ scalarsHead: [UInt32], _ scalarsRepairedTail: [UInt32],
    _ encoded: [UInt16],
    file: String = #file, line: UInt = #line
  ) {
    self.scalarsHead = scalarsHead
    self.scalarsRepairedTail = scalarsRepairedTail
    self.encoded = encoded
    self.loc = SourceLoc(file, line, comment: "test data")
  }
}

public let utf16Tests = [
  "Incomplete": [
    //
    // Incomplete sequences that end right before EOF.
    //

    // U+D800 (high-surrogate)
    UTF16Test([], [ 0xFFFD ], [ 0xD800 ]),

    // U+D800 (high-surrogate)
    // U+D800 (high-surrogate)
    UTF16Test([], [ 0xFFFD, 0xFFFD ], [ 0xD800, 0xD800 ]),

    // U+0041 LATIN CAPITAL LETTER A
    // U+D800 (high-surrogate)
    UTF16Test([ 0x0041 ], [ 0xFFFD ], [ 0x0041, 0xD800 ]),

    // U+10000 LINEAR B SYLLABLE B008 A
    // U+D800 (high-surrogate)
    UTF16Test(
        [ 0x0001_0000 ], [ 0xFFFD ],
        [ 0xD800, 0xDC00, 0xD800 ]),

    //
    // Incomplete sequences with more code units following them.
    //

    // U+D800 (high-surrogate)
    // U+0041 LATIN CAPITAL LETTER A
    UTF16Test([], [ 0xFFFD, 0x0041 ], [ 0xD800, 0x0041 ]),

    // U+D800 (high-surrogate)
    // U+10000 LINEAR B SYLLABLE B008 A
    UTF16Test(
        [], [ 0xFFFD, 0x0001_0000 ],
        [ 0xD800, 0xD800, 0xDC00 ]),

    // U+0041 LATIN CAPITAL LETTER A
    // U+D800 (high-surrogate)
    // U+0041 LATIN CAPITAL LETTER A
    UTF16Test(
        [ 0x0041 ], [ 0xFFFD, 0x0041 ],
        [ 0x0041, 0xD800, 0x0041 ]),

    // U+0041 LATIN CAPITAL LETTER A
    // U+D800 (high-surrogate)
    // U+10000 LINEAR B SYLLABLE B008 A
    UTF16Test(
        [ 0x0041 ], [ 0xFFFD, 0x0001_0000 ],
        [ 0x0041, 0xD800, 0xD800, 0xDC00 ]),

    // U+0041 LATIN CAPITAL LETTER A
    // U+D800 (high-surrogate)
    // U+DB40 (high-surrogate)
    // U+0041 LATIN CAPITAL LETTER A
    UTF16Test(
        [ 0x0041 ], [ 0xFFFD, 0xFFFD, 0x0041 ],
        [ 0x0041, 0xD800, 0xDB40, 0x0041 ]),

    // U+0041 LATIN CAPITAL LETTER A
    // U+D800 (high-surrogate)
    // U+DB40 (high-surrogate)
    // U+10000 LINEAR B SYLLABLE B008 A
    UTF16Test(
        [ 0x0041 ], [ 0xFFFD, 0xFFFD, 0x0001_0000 ],
        [ 0x0041, 0xD800, 0xDB40, 0xD800, 0xDC00 ]),

    // U+0041 LATIN CAPITAL LETTER A
    // U+D800 (high-surrogate)
    // U+DB40 (high-surrogate)
    // U+DBFF (high-surrogate)
    // U+0041 LATIN CAPITAL LETTER A
    UTF16Test(
        [ 0x0041 ], [ 0xFFFD, 0xFFFD, 0xFFFD, 0x0041 ],
        [ 0x0041, 0xD800, 0xDB40, 0xDBFF, 0x0041 ]),

    // U+0041 LATIN CAPITAL LETTER A
    // U+D800 (high-surrogate)
    // U+DB40 (high-surrogate)
    // U+DBFF (high-surrogate)
    // U+10000 LINEAR B SYLLABLE B008 A
    UTF16Test(
        [ 0x0041 ], [ 0xFFFD, 0xFFFD, 0xFFFD, 0x0001_0000 ],
        [ 0x0041, 0xD800, 0xDB40, 0xDBFF, 0xD800, 0xDC00 ]),
  ],

  "IllFormed": [
    //
    // Low-surrogate right before EOF.
    //

    // U+DC00 (low-surrogate)
    UTF16Test([], [ 0xFFFD ], [ 0xDC00 ]),

    // U+DC00 (low-surrogate)
    // U+DC00 (low-surrogate)
    UTF16Test([], [ 0xFFFD, 0xFFFD ], [ 0xDC00, 0xDC00 ]),

    // U+0041 LATIN CAPITAL LETTER A
    // U+DC00 (low-surrogate)
    UTF16Test([ 0x0041 ], [ 0xFFFD ], [ 0x0041, 0xDC00 ]),

    // U+10000 LINEAR B SYLLABLE B008 A
    // U+DC00 (low-surrogate)
    UTF16Test(
        [ 0x0001_0000 ], [ 0xFFFD ],
        [ 0xD800, 0xDC00, 0xDC00 ]),

    //
    // Low-surrogate with more code units following it.
    //

    // U+DC00 (low-surrogate)
    // U+0041 LATIN CAPITAL LETTER A
    UTF16Test([], [ 0xFFFD, 0x0041 ], [ 0xDC00, 0x0041 ]),

    // U+DC00 (low-surrogate)
    // U+10000 LINEAR B SYLLABLE B008 A
    UTF16Test(
        [], [ 0xFFFD, 0x0001_0000 ],
        [ 0xDC00, 0xD800, 0xDC00 ]),

    // U+0041 LATIN CAPITAL LETTER A
    // U+DC00 (low-surrogate)
    // U+0041 LATIN CAPITAL LETTER A
    UTF16Test(
        [ 0x0041 ], [ 0xFFFD, 0x0041 ],
        [ 0x0041, 0xDC00, 0x0041 ]),

    // U+0041 LATIN CAPITAL LETTER A
    // U+DC00 (low-surrogate)
    // U+10000 LINEAR B SYLLABLE B008 A
    UTF16Test(
        [ 0x0041 ], [ 0xFFFD, 0x0001_0000 ],
        [ 0x0041, 0xDC00, 0xD800, 0xDC00 ]),

    // U+0041 LATIN CAPITAL LETTER A
    // U+DC00 (low-surrogate)
    // U+DD00 (low-surrogate)
    // U+0041 LATIN CAPITAL LETTER A
    UTF16Test(
        [ 0x0041 ], [ 0xFFFD, 0xFFFD, 0x0041 ],
        [ 0x0041, 0xDC00, 0xDD00, 0x0041 ]),

    // U+0041 LATIN CAPITAL LETTER A
    // U+DC00 (low-surrogate)
    // U+DD00 (low-surrogate)
    // U+10000 LINEAR B SYLLABLE B008 A
    UTF16Test(
        [ 0x0041 ], [ 0xFFFD, 0xFFFD, 0x0001_0000 ],
        [ 0x0041, 0xDC00, 0xDD00, 0xD800, 0xDC00 ]),

    // U+0041 LATIN CAPITAL LETTER A
    // U+DC00 (low-surrogate)
    // U+DD00 (low-surrogate)
    // U+DFFF (low-surrogate)
    // U+0041 LATIN CAPITAL LETTER A
    UTF16Test(
        [ 0x0041 ], [ 0xFFFD, 0xFFFD, 0xFFFD, 0x0041 ],
        [ 0x0041, 0xDC00, 0xDD00, 0xDFFF, 0x0041 ]),

    // U+0041 LATIN CAPITAL LETTER A
    // U+DC00 (low-surrogate)
    // U+DD00 (low-surrogate)
    // U+DFFF (low-surrogate)
    // U+10000 LINEAR B SYLLABLE B008 A
    UTF16Test(
        [ 0x0041 ], [ 0xFFFD, 0xFFFD, 0xFFFD, 0x0001_0000 ],
        [ 0x0041, 0xDC00, 0xDD00, 0xDFFF, 0xD800, 0xDC00 ]),

    //
    // Low-surrogate followed by high-surrogate.
    //

    // U+DC00 (low-surrogate)
    // U+D800 (high-surrogate)
    UTF16Test([], [ 0xFFFD, 0xFFFD ], [ 0xDC00, 0xD800 ]),

    // U+DC00 (low-surrogate)
    // U+DB40 (high-surrogate)
    UTF16Test([], [ 0xFFFD, 0xFFFD ], [ 0xDC00, 0xDB40 ]),

    // U+DC00 (low-surrogate)
    // U+DBFF (high-surrogate)
    UTF16Test([], [ 0xFFFD, 0xFFFD ], [ 0xDC00, 0xDBFF ]),


    // U+DD00 (low-surrogate)
    // U+D800 (high-surrogate)
    UTF16Test([], [ 0xFFFD, 0xFFFD ], [ 0xDD00, 0xD800 ]),

    // U+DD00 (low-surrogate)
    // U+DB40 (high-surrogate)
    UTF16Test([], [ 0xFFFD, 0xFFFD ], [ 0xDD00, 0xDB40 ]),

    // U+DD00 (low-surrogate)
    // U+DBFF (high-surrogate)
    UTF16Test([], [ 0xFFFD, 0xFFFD ], [ 0xDD00, 0xDBFF ]),


    // U+DFFF (low-surrogate)
    // U+D800 (high-surrogate)
    UTF16Test([], [ 0xFFFD, 0xFFFD ], [ 0xDFFF, 0xD800 ]),

    // U+DFFF (low-surrogate)
    // U+DB40 (high-surrogate)
    UTF16Test([], [ 0xFFFD, 0xFFFD ], [ 0xDFFF, 0xDB40 ]),

    // U+DFFF (low-surrogate)
    // U+DBFF (high-surrogate)
    UTF16Test([], [ 0xFFFD, 0xFFFD ], [ 0xDFFF, 0xDBFF ]),


    // U+DC00 (low-surrogate)
    // U+D800 (high-surrogate)
    // U+0041 LATIN CAPITAL LETTER A
    UTF16Test(
        [], [ 0xFFFD, 0xFFFD, 0x0041 ],
        [ 0xDC00, 0xD800, 0x0041 ]),

    // U+DC00 (low-surrogate)
    // U+D800 (high-surrogate)
    // U+10000 LINEAR B SYLLABLE B008 A
    UTF16Test(
        [], [ 0xFFFD, 0xFFFD, 0x10000 ],
        [ 0xDC00, 0xD800, 0xD800, 0xDC00 ]),
  ],
]

runAllTests()

#else
//===--- benchmarking -----------------------------------------------------===//

@inline(never)
public func run_UTF8Decode(_ N: Int) {
  // 1-byte sequences
  // This test case is the longest as it's the most performance sensitive.
  let ascii = "Swift is a multi-paradigm, compiled programming language created for iOS, OS X, watchOS, tvOS and Linux development by Apple Inc. Swift is designed to work with Apple's Cocoa and Cocoa Touch frameworks and the large body of existing Objective-C code written for Apple products. Swift is intended to be more resilient to erroneous code (\"safer\") than Objective-C and also more concise. It is built with the LLVM compiler framework included in Xcode 6 and later and uses the Objective-C runtime, which allows C, Objective-C, C++ and Swift code to run within a single program."
  // 2-byte sequences
  let russian = "Ру́сский язы́к один из восточнославянских языков, национальный язык русского народа."
  // 3-byte sequences
  let japanese = "日本語（にほんご、にっぽんご）は、主に日本国内や日本人同士の間で使われている言語である。"
  // 4-byte sequences
  // Most commonly emoji, which are usually mixed with other text.
  let emoji = "Panda 🐼, Dog 🐶, Cat 🐱, Mouse 🐭."

  let strings = [ascii, russian, japanese, emoji].map { Array($0.utf8) }

  func isEmpty(_ result: UnicodeDecodingResult) -> Bool {
    switch result {
    case .emptyInput:
      return true
    default:
      return false
    }
  }

  var total: UInt32 = 0
  
  for _ in 1...200*N {
    for string in strings {
#if BASELINE
      _ = transcode(
        string.makeIterator(), from: UTF8.self, to: UTF32.self,
        stoppingOnError: false
      ) {
        total = total &+ $0
      }
#else
  #if FORWARD
      var it = string.makeIterator()
      typealias D = UTF8.ForwardDecoder
      D.decode(&it, repairingIllFormedSequences: true) { total = total &+ $0.value }
  #elseif REVERSE
      var it = string.reversed().makeIterator()
      typealias D = UTF8.ReverseDecoder
      D.decode(&it, repairingIllFormedSequences: true) { total = total &+ $0.value }
  #elseif SEQUENCE
      for s in Unicode.DefaultScalarView(string, fromEncoding: UTF8.self) {
        total = total &+ s.value
      }
  #elseif COLLECTION
      let scalars = Unicode.DefaultScalarView(string, fromEncoding: UTF8.self)
      var i = scalars.startIndex
      while i != scalars.endIndex {
        total = total &+ scalars[i].value
        i = scalars.index(after: i)
      }
#elseif REVERSE_COLLECTION
      let scalars = Unicode.DefaultScalarView(string, fromEncoding: UTF8.self)
      var i = scalars.endIndex
      while i != scalars.startIndex {
        i = scalars.index(before: i)
        total = total &+ scalars[i].value
      }
  #else
      Error_Unknown_Benchmark()
  #endif
#endif
    }
  }
  if CommandLine.arguments.count > 1000 { print(total) }
}

run_UTF8Decode(10000)
#endif

