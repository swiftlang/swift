// RUN: rm -f %t && %target-build-swift -I %S/icu -licucore %s -o %t
// RUN: %target-run %t
// REQUIRES: executable_test

import StdlibUnittest
import ICU

/// A lazy collection of `Encoding.EncodedScalar` that results
/// from parsing an instance of codeUnits using that `Encoding`.
public struct ParsedUnicode<
  CodeUnits: BidirectionalCollection,
  Encoding: UnicodeEncoding
>
where Encoding.EncodedScalar.Iterator.Element == CodeUnits.Iterator.Element,
  CodeUnits.SubSequence : BidirectionalCollection,
  CodeUnits.SubSequence.Index == CodeUnits.Index,
  CodeUnits.SubSequence.SubSequence == CodeUnits.SubSequence,
  CodeUnits.SubSequence.Iterator.Element == CodeUnits.Iterator.Element {
  let codeUnits: CodeUnits
  
  init(_ codeUnits: CodeUnits, _: Encoding.Type = Encoding.self) {
    self.codeUnits = codeUnits
  }
}

extension ParsedUnicode {
  // Because parsing produces a buffer and a new index, to avoid
  // repeatedly decoding the same data, this index stores that buffer
  // and the next index.  This would obviously be more complicated if
  // the buffer contained more than a single scalar (and it probably
  // should).
  public struct Index : Comparable {
    let base: CodeUnits.Index
    // FIXME: We might get a much better memory footprint if we used a
    // UInt8 to store the distance between base and next, rather than
    // storing next explicitly.  CodeUnits will be random-access in
    // practice.
    let next: CodeUnits.Index
    // FIXME: there should be an invalid inhabitant we can use in
    // EncodedScalar so as not to waste a separate bool here.
    let scalar: Encoding.EncodedScalar?

    public static func < (lhs: Index, rhs: Index) -> Bool {
      return lhs.base < rhs.base
    }
    public static func == (lhs: Index, rhs: Index) -> Bool {
      return lhs.base == rhs.base
    }
  }
}

//===----------------------------------------------------------------------===//
//===--- Missing stdlib niceties ------------------------------------------===//
//===----------------------------------------------------------------------===//

//===--- UnicodeScalar / Character affordances ----------------------------===//
extension UnicodeScalar {
  init<E: EncodedScalarProtocol>(_ e: E) {
    // FIXME:use init(_unchecked:) when in stdlib
    self = UnicodeScalar(e.utf32[0])!
  }
}
extension Character {
  init<S: Sequence>(_ s: S) where S.Iterator.Element == UnicodeScalar {
    // FIXME: Horribly inefficient, but the stuff to make it fast is private.
    // FIXME: Also, constructing "👩‍❤️‍👩" is foiled by precondition checks
    var r = ""
    for scalar in s {
      r += String(scalar)
    }
    self = Character(r)
  } 
}

//===--- Comparison between UnsafePointer<T> and UnsafeMutablePointer<T> ---===//
extension UnsafePointer {
  static func == (
    self_: UnsafePointer, mut: UnsafeMutablePointer<Pointee>) -> Bool {
    return self_ == UnsafePointer(mut)
  }
  static func == (
    mut: UnsafeMutablePointer<Pointee>, self_: UnsafePointer) -> Bool {
    return self_ == UnsafePointer(mut)
  }
  static func != (
    self_: UnsafePointer, mut: UnsafeMutablePointer<Pointee>) -> Bool {
    return self_ == UnsafePointer(mut)
  }
  static func != (
    mut: UnsafeMutablePointer<Pointee>, self_: UnsafePointer) -> Bool {
    return self_ == UnsafePointer(mut)
  }
  // FIXME: add <, >, <=, >=
}

//===--- Integer coercion operator ----------------------------------------===//
// You may think it's icky, but it sure cleans up a lot of ugly numericCast()
// invocations in this file.
postfix operator ^

extension _SignedInteger {
  static postfix func ^ <U : _SignedInteger>(_ x: Self) -> U {
    return numericCast(x)
  }
  static postfix func ^ <U : UnsignedInteger>(_ x: Self) -> U {
    return numericCast(x)
  }
}
extension UnsignedInteger {
  static postfix func ^ <U : UnsignedInteger>(_ x: Self) -> U {
    return numericCast(x)
  }
  static postfix func ^ <U : SignedInteger>(_ x: Self) -> U {
    return numericCast(x)
  }
}

//===--- Missing clamping of Comparable to a Range ------------------------===//
extension Comparable {
  func clamped(to r: ClosedRange<Self>) -> Self {
    return self < r.lowerBound ? r.lowerBound
         : self > r.upperBound ? r.upperBound
         : self
  }
}

//===--- Index affordances ------------------------------------------------===//
extension Collection {
  func index<I: SignedInteger>(atOffset offset: I) -> Index {
    return index(startIndex, offsetBy: offset^)
  }
  func offset(of i: Index) -> IndexDistance {
    return distance(from: startIndex, to: i)
  }
}

//===--- someCollection[...] notation -------------------------------------===//
// Thanks to Joe Groff for the technique
enum FullSlice {}
postfix func ... (_:())->FullSlice {
  fatalError("do not call me")
}
extension Collection {
  subscript(_:(())->FullSlice) -> SubSequence {
    return self[startIndex...]
  }
}

extension MutableCollection {
  subscript(_:(())->FullSlice) -> SubSequence {
    get {
      return self[startIndex...]
    }
    set {
      self[startIndex...] = newValue
    }
  }
}

//===--- Algorithms -------------------------------------------------------===//
extension MutableCollection {
  /// Copies elements from `source` into `self`, starting at the beginning of
  /// each.
  ///
  /// - Returns:
  ///
  ///   - `limit`: the first index in `self` that was not copied into, or
  ///     `endIndex` if all elements were assigned.
  ///
  ///   - `remainder`: the subsequence of source that didn't fit into `self`, 
  ///     or `source[endIndex...]` if all elements fit.
  @discardableResult
  mutating func copy<Source: Collection>(from source: Source)
  -> (limit: Index, remainder: Source.SubSequence)
  where Source.SubSequence : Collection,
  Source.SubSequence.Iterator.Element == Iterator.Element,
  Source.SubSequence == Source.SubSequence.SubSequence {
    // This method should be optimizable for segmented collections
    var r = source[...]
    var i: Index = startIndex
    while i != endIndex {
      guard let e = r.popFirst()
      else { return (limit: i, remainder: r) }
      self[i] = e
      i = index(after: i)
    }
    return (limit: endIndex, remainder: r)
  }
}

//===--- One-sided ranges -------------------------------------------------===//
prefix operator ..<
struct IncompleteRangeUpTo<T: Comparable> {
  init(_ upperBound: T) { self.upperBound = upperBound }
  let upperBound: T
}
extension Comparable {
  static prefix func ..<(x: Self) -> IncompleteRangeUpTo<Self> {
    return IncompleteRangeUpTo(x)
  }
}
extension Collection {
  subscript(r: IncompleteRangeUpTo<Index>) -> SubSequence {
    return self[self.startIndex..<r.upperBound]
  }
}
extension MutableCollection {
  subscript(r: IncompleteRangeUpTo<Index>) -> SubSequence {
    get {
      return self[self.startIndex..<r.upperBound]
    }
    set {
      self[self.startIndex..<r.upperBound] = newValue
    }
  }
}

prefix operator ...
struct IncompleteRangeThrough<T: Comparable> {
  init(_ upperBound: T) { self.upperBound = upperBound }
  let upperBound: T
}
extension Comparable {
  static prefix func ...(x: Self) -> IncompleteRangeThrough<Self> {
    return IncompleteRangeThrough(x)
  }
}
extension Collection {
  subscript(r: IncompleteRangeThrough<Index>) -> SubSequence {
    return self[self.startIndex...r.upperBound]
  }
}
extension MutableCollection {
  subscript(r: IncompleteRangeThrough<Index>) -> SubSequence {
    get {
      return self[self.startIndex...r.upperBound]
    }
    set {
      self[self.startIndex...r.upperBound] = newValue
    }
  }
}

postfix operator ...
struct IncompleteRangeFrom<T: Comparable> {
  init(_ lowerBound: T) { self.lowerBound = lowerBound }
  let lowerBound: T
}
extension Comparable {
  static postfix func ...(x: Self) -> IncompleteRangeFrom<Self> {
    return IncompleteRangeFrom(x)
  }
}
extension Collection {
  subscript(r: IncompleteRangeFrom<Index>) -> SubSequence {
    return self[r.lowerBound..<self.endIndex]
  }
}
extension MutableCollection {
  subscript(r: IncompleteRangeFrom<Index>) -> SubSequence {
    get {
      return self[r.lowerBound..<self.endIndex]
    }
    set {
      self[r.lowerBound..<self.endIndex] = newValue
    }
  }
}

//===----------------------------------------------------------------------===//
//===--- Logging ----------------------------------------------------------===//
//===----------------------------------------------------------------------===//
var logging = false
func debugLog(_ arg: @autoclosure ()->Any) {
  guard logging == true else { return }
  print(arg())
}
func debugLog(_ arg0: @autoclosure ()->Any, _ arg1: @autoclosure ()->Any) {
  guard logging else { return }
  print(arg0(), arg1())
}
//===----------------------------------------------------------------------===//

/// Collection Conformance
extension ParsedUnicode : BidirectionalCollection {
  public var startIndex: Index {
    if _slowPath(codeUnits.isEmpty) { return endIndex }
    let s = codeUnits.startIndex
    return index(after: Index(base: s, next: s, scalar: nil))
  }
  
  public var endIndex: Index {
    let s = codeUnits.endIndex
    return Index(base: s, next: s, scalar: nil)
  }
  
  public subscript(i: Index) -> Encoding.EncodedScalar {
    if let r = i.scalar {
      return r
    }
    return index(after:
      Index(base: i.base, next: i.base, scalar: nil)).scalar!
  }

  public func index(after i: Index) -> Index {
    var remainder = codeUnits[i.next...]
    while true {
      switch Encoding.parse1Forward(remainder, knownCount: 0) {
      case .valid(let scalar, let nextIndex):
        return Index(base:i.next, next: nextIndex, scalar: scalar)
      case .error(let nextIndex):
        // FIXME: don't go through UnicodeScalar once this is in the stdlib
        if let replacement = Encoding.encode(
          UTF32.EncodedScalar(UnicodeScalar(0xFFFD)!)) {
          return Index(
            base:i.next, next: nextIndex,
            scalar: replacement)
        }
        remainder = remainder.dropFirst()
      case .emptyInput:
        return endIndex
      }
    }
  }

  public func index(before i: Index) -> Index {
    var remainder = codeUnits[..<i.base]
    while true {
      switch Encoding.parse1Reverse(remainder, knownCount: 0) {
      case .valid(let scalar, let priorIndex):
        return Index(base: priorIndex, next: i.base, scalar: scalar)
      case .error(let priorIndex):
        // FIXME: don't go through UnicodeScalar once this is in the stdlib
        if let replacement = Encoding.encode(
          UTF32.EncodedScalar(UnicodeScalar(0xFFFD)!)) {
          return Index(
            base: priorIndex, next: i.base, 
            scalar: replacement)        
        }
        remainder = remainder.dropLast()
      case .emptyInput:
        fatalError("Indexing past start of code units")
      }
    }
  }
}

/// Given `CodeUnits` representing text that has been encoded with
/// `FromEncoding`, provides a collection of `ToEncoding.CodeUnit`s
/// representing the same text.
struct TranscodedView<
  CodeUnits : BidirectionalCollection,
  FromEncoding : UnicodeEncoding,
  ToEncoding : UnicodeEncoding
> 
where FromEncoding.EncodedScalar.Iterator.Element == CodeUnits.Iterator.Element,
  CodeUnits.SubSequence : BidirectionalCollection,
  CodeUnits.SubSequence.Index == CodeUnits.Index,
  CodeUnits.SubSequence.SubSequence == CodeUnits.SubSequence,
  CodeUnits.SubSequence.Iterator.Element == CodeUnits.Iterator.Element
{
  // We could just be a generic typealias as this type, but it turns
  // out to be impossible, or nearly so, to write the init() below.
  // Instead, we wrap an instance of Base.
  typealias Base = FlattenBidirectionalCollection<
    LazyMapBidirectionalCollection<
      ParsedUnicode<CodeUnits, FromEncoding>,
      ToEncoding.EncodedScalar
    >
  >
  let base: Base
}

extension TranscodedView : BidirectionalCollection {
  typealias SubSequence = BidirectionalSlice<TranscodedView>
  
  public var startIndex : Base.Index {
    return base.startIndex
  }
  public var endIndex : Base.Index {
    return base.endIndex
  }
  public subscript(i: Base.Index) -> Base.Iterator.Element {
    return base[i]
  }
  public init(_ codeUnits: CodeUnits,
    from src: FromEncoding.Type = FromEncoding.self,
    to dst: ToEncoding.Type = ToEncoding.self
  ) {
    base = Base(ParsedUnicode(codeUnits, src).lazy.map {
        dst.encode($0)!
      })
  }
  public func index(after i: Base.Index) -> Base.Index {
    return base.index(after: i)
  }
  public func index(before i: Base.Index) -> Base.Index {
    return base.index(before: i)
  }
}

protocol Unicode {
  associatedtype Encoding: UnicodeEncoding
  associatedtype CodeUnits: RandomAccessCollection
  /* where CodeUnits.Iterator.Element == Encoding.CodeUnit */
  var codeUnits: CodeUnits {get}
  
  associatedtype ValidUTF8View : BidirectionalCollection
  // where ValidUTF8View.Iterator.Element == UTF8.CodeUnit */
  // = TranscodedView<CodeUnits, Encoding, UTF8>
  var utf8: ValidUTF8View {get}
  
  associatedtype ValidUTF16View : BidirectionalCollection
  // where ValidUTF16View.Iterator.Element == UTF16.CodeUnit
  // = TranscodedView<CodeUnits, Encoding, UTF16>
  var utf16: ValidUTF16View {get}
  
  associatedtype ValidUTF32View : BidirectionalCollection
  // where ValidUTF32View.Iterator.Element == UTF32.CodeUnit
  // = TranscodedView<CodeUnits, Encoding, UTF32>
  var utf32: ValidUTF32View {get}
  
  associatedtype ExtendedASCII : BidirectionalCollection // FIXME: Can this be Random Access?
  /* where ExtendedASCII.Iterator.Element == UInt32 */
  var extendedASCII: ExtendedASCII {get}

  associatedtype Characters : BidirectionalCollection
  /* where Characters.Iterator.Element == Character */
  var characters: Characters { get }
  
  func isASCII(scan: Bool/* = true */) -> Bool 
  func isLatin1(scan: Bool/* = true */) -> Bool 
  func isNormalizedNFC(scan: Bool/* = true*/) -> Bool
  func isNormalizedNFD(scan: Bool/* = true*/) -> Bool
  func isInFastCOrDForm(scan: Bool/* = true*/) -> Bool
}


struct UnicodeStorage<
  CodeUnits : RandomAccessCollection,
  Encoding : UnicodeEncoding
>
where Encoding.EncodedScalar.Iterator.Element == CodeUnits.Iterator.Element,
  CodeUnits.SubSequence : RandomAccessCollection,
  CodeUnits.SubSequence.Index == CodeUnits.Index,
  CodeUnits.SubSequence.SubSequence == CodeUnits.SubSequence,
  CodeUnits.SubSequence.Iterator.Element == CodeUnits.Iterator.Element {

  init(_ codeUnits: CodeUnits, _: Encoding.Type = Encoding.self) {
    self.codeUnits = codeUnits
  }
  
  let codeUnits: CodeUnits
}

extension UText {
  /// Invokes body, passing this UText's buffer area as a parameter
  mutating func withBuffer<R>(
    _ body: (UnsafeMutableBufferPointer<UChar>)->R
  ) -> R {
    // Currently we are using the p, q, and r fields to get 12 UWords of
    // contiguous storage on 64-bit machines and 6 on 32-bit.  It's not much.
    return withUnsafeMutablePointer(to: &p) { bufferStart in
      let rawBufferStart = UnsafeRawPointer(bufferStart)
      let capacity = withUnsafeMutablePointer(to: &privP) {
        bufferLimit in
        (
          UnsafeRawPointer(bufferLimit).assumingMemoryBound(to: Int8.self)
          - rawBufferStart.assumingMemoryBound(to: Int8.self)
        ) / MemoryLayout<UChar>.stride
      }
      let start = rawBufferStart.bindMemory(to: UChar.self, capacity: capacity)
      let mutableStart = UnsafeMutablePointer(mutating: start)
      let buffer = UnsafeMutableBufferPointer(start: mutableStart, count: capacity)
      return body(buffer)
    }
  }
  
  mutating func validate() {
    let base = self.withBuffer { $0.baseAddress! }
    assert(chunkContents! == base, "UText moved!")
  }

  mutating func setup() {
    chunkContents = self.withBuffer { UnsafePointer($0.baseAddress!) }
    pExtra = withUnsafeMutablePointer(to: &self) {
      UnsafeMutableRawPointer($0 + 1)
    }
  }
}

fileprivate protocol _UTextable {
  func _nativeLength(_ uText: inout UText) -> Int64
  func _access(_ u: inout UText, _ nativeIndex: Int64, _ forward: Bool) -> Bool
  
  func _clone(
    _ dst: UnsafeMutablePointer<UText>?, _ u: UnsafePointer<UText>,
    _ deep: Bool, _ status: UnsafeMutablePointer<UErrorCode>?
  ) -> UnsafeMutablePointer<UText>
  
  func _extract(
    _ u: inout UText,
    _ nativeStart: Int64, _ nativeLimit: Int64,
    _ destination: UnsafeMutableBufferPointer<UChar>,
    _ error: UnsafeMutablePointer<UErrorCode>?
  ) -> Int32
  
  func _mapOffsetToNative(_ u: UnsafePointer<UText>) -> Int64
  func _mapNativeIndexToUTF16(_ u: UnsafePointer<UText>, _ nativeIndex: Int64) -> Int32
}

extension UnicodeStorage : _UTextable {
  fileprivate func _nativeLength(_ uText: inout UText) -> Int64 {
    uText.validate()
    return codeUnits.count^
  }

  fileprivate func _parsedSlice(
    _ offset: Int64,
    _ slice: (CodeUnits.Index) -> CodeUnits.SubSequence
  ) -> ParsedUnicode<CodeUnits.SubSequence,Encoding>.SubSequence {
    return ParsedUnicode(
      slice(codeUnits.index(atOffset: offset)), Encoding.self
    ).dropFirst(0)
  }

  fileprivate func _parsedSuffix(
    fromOffset offset: Int64
  ) -> ParsedUnicode<CodeUnits.SubSequence,Encoding>.SubSequence {
    return _parsedSlice(offset, codeUnits.suffix(from:))
  }

  fileprivate func _clone(
    _ dst: UnsafeMutablePointer<UText>?, _ src: UnsafePointer<UText>,
    _ deep: Bool, _ status: UnsafeMutablePointer<UErrorCode>?
  ) ->  UnsafeMutablePointer<UText> {
    UnsafeMutablePointer(mutating: src).pointee.validate()
    debugLog("_clone with dst = \(String(describing: dst))")
    debugLog("src: \(src.pointee)")
    let r = dst
      ?? UnsafeMutablePointer.allocate(capacity: MemoryLayout<UText>.size)
    r.pointee = src.pointee
    r.pointee.setup()
    r.pointee.validate()
    debugLog("clone result: \(r.pointee)")
    return r
  }

  fileprivate func _access(
    _ u: inout UText, _ nativeTargetIndex: Int64, _ forward: Bool
  ) -> Bool {

    debugLog("_access(u: \(u), nativeTargetIndex: \(nativeTargetIndex), forward: \(forward))")
    u.validate()
    u.chunkOffset = 0

    let inBoundsTarget = nativeTargetIndex - (forward ? 0 : 1)
    if (u.chunkNativeStart..<u.chunkNativeLimit).contains(inBoundsTarget) {

      var parsedChunk = _parsedSuffix(fromOffset: u.chunkNativeStart)
      
      var nativeOffset = u.chunkNativeStart
      while nativeOffset < nativeTargetIndex,
      let scalar = parsedChunk.popFirst() {
        nativeOffset += scalar.count^
        u.chunkOffset += scalar.utf16.count^
      }
      return true
    }
    debugLog("_access: filling buffer")
    
    guard (0...codeUnits.count^).contains(nativeTargetIndex)
    else { return false }

    u.chunkLength = 0
    u.chunkNativeStart = nativeTargetIndex
    u.chunkNativeLimit = nativeTargetIndex
    
    u.withBuffer { buffer in
      if forward {
        let chunkSource = _parsedSuffix(fromOffset: nativeTargetIndex)

        for (i, scalar) in zip(chunkSource.indices, chunkSource) {
          let newChunkLength = u.chunkLength + scalar.utf16.count^          
          // don't overfill the buffer
          if newChunkLength > buffer.count^ { break }
          for unit in scalar.utf16 {
            debugLog("# unit: \(String(unit, radix: 16))")
            buffer[u.chunkLength^] = unit
            u.chunkLength += 1
          }
          u.chunkNativeLimit = codeUnits.offset(of: i.next)^
        }
      }
      else {
        let chunkSource
          = _parsedSlice(nativeTargetIndex, codeUnits.prefix(upTo:))

        // FIXME: must call reversed twice below because zip won't return a
        // BidirectionalCollection... which might be hard!
        for (i, scalar) in zip(
          chunkSource.indices.reversed(), chunkSource.reversed()) {
          
          let newChunkLength = u.chunkLength + scalar.utf16.count^
          // don't overfill the buffer
          if newChunkLength > buffer.count^ { break }
          for unit in scalar.utf16.reversed() {
            buffer[u.chunkLength^] = unit
            u.chunkLength += 1
          }
          u.chunkNativeStart = codeUnits.offset(of: i.base)^
          u.chunkOffset = u.chunkLength
        }
        var b = buffer // copy due to https://bugs.swift.org/browse/SR-3782
        b[..<buffer.index(atOffset: u.chunkLength)].reverse()
      }
    }
    debugLog("_access filled buffer, u = \(u)")
    return true
  }
  
  fileprivate func _extract(
    _ u: inout UText,
    _ nativeStart: Int64, _ nativeLimit: Int64,
    _ destination: UnsafeMutableBufferPointer<UChar>,
    _ error: UnsafeMutablePointer<UErrorCode>?
  ) -> Int32 {
    debugLog("_extract: \(u)")
    u.validate()

    let s = nativeStart.clamped(to: 0...codeUnits.count^)
    let l = nativeLimit.clamped(to: 0...codeUnits.count^)
    u.chunkNativeStart = l
    u.chunkNativeLimit = l
    u.chunkLength = 0
    
    if s < l { // anything to extract?
      let base = codeUnits[
        codeUnits.index(atOffset: s)..<codeUnits.index(atOffset: l)
      ]
      let source = TranscodedView(base, from: Encoding.self, to: UTF16.self)
      var d = destination // copy due to https://bugs.swift.org/browse/SR-3782
      let (limit, remainder) = d.copy(from: source)
      
      // Add null termination if it fits
      if limit < d.endIndex { d[limit] = 0 }

      // If no overflow, we're done
      if remainder.isEmpty { return Int32(destination.offset(of: limit)) }

      // Report the error and account for the overflow length in the return value
      error!.pointee = U_BUFFER_OVERFLOW_ERROR
      return Int32(destination.offset(of: limit) + remainder.count)
    }
    return 0
  }
  
  fileprivate func _mapOffsetToNative(_ u: UnsafePointer<UText>) -> Int64 {
    UnsafeMutablePointer(mutating: u).pointee.validate()
    
    if u.pointee.chunkOffset == 0 { return u.pointee.chunkNativeStart }
    
    let chunkSource = _parsedSuffix(fromOffset: u.pointee.chunkNativeStart)
    var chunkOffset = 0
    
    for i in chunkSource.indices {
      chunkOffset += chunkSource[i].utf16.count
      if chunkOffset == u.pointee.chunkOffset^ {
        return codeUnits.offset(of: i.next)^
      }
    }
    fatalError("supposed to be unreachable")
  }
  
  fileprivate func _mapNativeIndexToUTF16(_ u: UnsafePointer<UText>, _ nativeIndex: Int64) -> Int32 {
    debugLog("_mapNativeIndexToUTF16: \(u)")
    UnsafeMutablePointer(mutating: u).pointee.validate()
    
    if u.pointee.chunkNativeStart == nativeIndex { return 0 }

    let nativeChunk = codeUnits[
      codeUnits.index(atOffset: u.pointee.chunkNativeStart)
      ..<
      codeUnits.index(atOffset: nativeIndex)]
    
    return TranscodedView(
      nativeChunk, from: Encoding.self, to: UTF16.self
    ).count^
  }
  
  public func withUText<R>(_ body: (UnsafeMutablePointer<UText>)->R) -> R {

    var copy: _UTextable = self

    return withUnsafePointer(to: &copy) { pSelf in
      
      var vtable = UTextFuncs(
        tableSize: Int32(MemoryLayout<UTextFuncs>.stride),
        reserved1: 0, reserved2: 0, reserved3: 0,
        clone: { dst, u, deep, err in
          debugLog("clone(\(dst!), \(u!), \(deep), \(String(describing: err)))")
          let _self = u!.pointee.context.assumingMemoryBound(
            to: _UTextable.self).pointee
          return _self._clone(dst, u!, deep != 0, err)
        },
        
        nativeLength: { u in
          debugLog("nativeLength(\(u!))")
          let _self = u!.pointee.context.assumingMemoryBound(
            to: _UTextable.self).pointee
          let r = _self._nativeLength(&u!.pointee)
          debugLog("# nativeLength: \(r)")
          return r
        },
        
        access: { u, nativeIndex, forward in
          debugLog("access(\(u!), \(nativeIndex), \(forward))")
          let _self = u!.pointee.context.assumingMemoryBound(
            to: _UTextable.self).pointee
          return _self._access(&u!.pointee, nativeIndex, forward != 0) 
            ? 1 : 0
        },
        
        extract: { u, nativeStart, nativeLimit, dest, destCapacity, status in
          debugLog("extract(\(u!), \(nativeStart), \(nativeLimit), \(dest!), \(destCapacity), \(String(describing: status)))")
          let _self = u!.pointee.context.assumingMemoryBound(
            to: _UTextable.self).pointee
          
          let destination = UnsafeMutableBufferPointer(
            start: dest, count: destCapacity^)

          return _self._extract(
            &u!.pointee, nativeStart, nativeLimit, destination, status)
        },
        
        replace: nil,
        copy: nil,
        
        mapOffsetToNative: { u in 
          debugLog("mapOffsetToNative(\(u!.pointee.chunkOffset))")
          let _self = u!.pointee.context.assumingMemoryBound(
            to: _UTextable.self).pointee
          let r = _self._mapOffsetToNative(u!)
          debugLog("# mapOffsetToNative: \(r)")
          return r
        },
        
        mapNativeIndexToUTF16: { u, nativeIndex in 
          debugLog("mapNativeIndexToUTF16(nativeIndex: \(nativeIndex), u: \(u!.pointee))")
          let _self = u!.pointee.context.assumingMemoryBound(
            to: _UTextable.self).pointee
          let r = _self._mapNativeIndexToUTF16(u!, nativeIndex)
          debugLog("# mapNativeIndexToUTF16: \(r)")
          return r
        },
        close: nil,
        spare1: nil, spare2: nil, spare3: nil)

      var u = UText(
        magic: UInt32(UTEXT_MAGIC),
        flags: 0,
        providerProperties: 0,
        sizeOfStruct: Int32(MemoryLayout<UText>.size),
        chunkNativeLimit: 0,
        extraSize: 0,
        nativeIndexingLimit: 0,
        chunkNativeStart: 0,
        chunkOffset: 0,
        chunkLength: 0,
        chunkContents: nil,
        pFuncs: &vtable,
        pExtra: nil,
        context: UnsafeRawPointer(pSelf),
        p: nil, q: nil, r: nil,
        privP: nil,
        a: 0, b: 0, c: 0,
        privA: 0, privB: 0, privC: 0)
      u.setup()
      u.validate()
      return body(&u)
    }
  }
}

extension UnicodeStorage {
  var scalars: ParsedUnicode<CodeUnits, Encoding> {
    return ParsedUnicode(codeUnits, Encoding.self)
  }
}

extension UErrorCode {
  var isFailure: Bool { return rawValue > U_ZERO_ERROR.rawValue }
  var isWarning: Bool { return rawValue < U_ZERO_ERROR.rawValue }
  var isSuccess: Bool { return rawValue <= U_ZERO_ERROR.rawValue }
}
typealias UBreakIterator = OpaquePointer

struct CharacterView<
  CodeUnits : RandomAccessCollection,
  Encoding : UnicodeEncoding
> 
where Encoding.EncodedScalar.Iterator.Element == CodeUnits.Iterator.Element,
  CodeUnits.SubSequence : RandomAccessCollection,
  CodeUnits.SubSequence.Index == CodeUnits.Index,
  CodeUnits.SubSequence.SubSequence == CodeUnits.SubSequence,
  CodeUnits.SubSequence.Iterator.Element == CodeUnits.Iterator.Element {

  init(_ codeUnits: CodeUnits, _: Encoding.Type = Encoding.self) {
    self.storage = UnicodeStorage(codeUnits)
  }
  
  fileprivate let storage: UnicodeStorage<CodeUnits, Encoding>
}

extension CharacterView : BidirectionalCollection {
  typealias Index = CodeUnits.Index
  typealias SubSequence = BidirectionalSlice<CharacterView>
  
  public var startIndex: Index { return storage.codeUnits.startIndex }
  public var endIndex: Index { return storage.codeUnits.endIndex }

  fileprivate func _withUBreakIterator<R>(
    at i: Index, _ body: (UBreakIterator)->R
  ) -> R {
    var err = U_ZERO_ERROR;

    debugLog("ubrk_open")
    let bi = ubrk_open(
      /*type:*/ UBRK_CHARACTER, /*locale:*/ nil,
      /*text:*/ nil, /*textLength:*/ 0, /*status:*/ &err)
    precondition(err.isSuccess, "unexpected ubrk_open failure, \(err)")
    defer { ubrk_close(bi) }
    
    return storage.withUText { u in
      //let access = u.pointee.pFuncs.pointee.access(u, storage.codeUnits.offset(of: i)^, 1)
      //debugLog("access result:", access)
      debugLog("ubrk_setUText")
      ubrk_setUText(bi, u, &err)
      precondition(err.isSuccess, "unexpected ubrk_setUText failure: \(err)")
      return body(bi!)
    }
  }
  
  subscript(i: Index) -> Character {
    debugLog("subscript: i=\(i)")
    let j = index(after: i)
    debugLog("subscript: j=\(j)")
    let scalars = ParsedUnicode(storage.codeUnits[i..<j], Encoding.self)
    debugLog("scalars: \(Array(scalars))")
    return Character(scalars.lazy.map { UnicodeScalar($0) })
  }    

  func index(after i: Index) -> Index {
    // FIXME: there is always a grapheme break between two scalars that are both
    // < U+0300.  Use that to optimize.  Can we make a stronger statement, that
    // there's always a break before any scalar < U+0300?
    debugLog("index(after: \(i))")
    let nextOffset = _withUBreakIterator(at: i) {
      ubrk_following($0, storage.codeUnits.offset(of: i)^)
    }
    debugLog("  index(after: \(i)): \(nextOffset)")
    return storage.codeUnits.index(atOffset: nextOffset)
  }
  
  func index(before i: Index) -> Index {
    // FIXME: there is always a grapheme break between two scalars that are both
    // < U+0300.  Use that to optimize.  Can we make a stronger statement, that
    // there's always a break before any scalar < U+0300?
    debugLog("index(before: \(i))")
    let previousOffset = _withUBreakIterator(at: i) {
      ubrk_preceding($0, storage.codeUnits.offset(of: i)^)
    }
    debugLog("  -> \(previousOffset)")
    return storage.codeUnits.index(atOffset: previousOffset)
  }
}

struct Latin1String<Base : RandomAccessCollection> : Unicode
where Base.Iterator.Element == UInt8, Base.Index == Base.SubSequence.Index,
Base.SubSequence.SubSequence == Base.SubSequence,
Base.Iterator.Element == UInt8,
Base.SubSequence.Iterator.Element == Base.Iterator.Element {
  typealias Encoding = Latin1
  typealias CodeUnits = Base
  let codeUnits: CodeUnits
  let _isASCII: Bool?

  init(_ codeUnits: CodeUnits, isASCII: Bool? = nil) {
    self.codeUnits = codeUnits
    self._isASCII = isASCII
  }
  
  typealias ValidUTF8View = TranscodedView<CodeUnits, Encoding, UTF8>
  var utf8: ValidUTF8View { return ValidUTF8View(codeUnits) }
  
  typealias ValidUTF16View = TranscodedView<CodeUnits, Encoding, UTF16>
  var utf16: ValidUTF16View { return ValidUTF16View(codeUnits) }
  
  typealias ValidUTF32View = TranscodedView<CodeUnits, Encoding, UTF32>
  var utf32: ValidUTF32View { return ValidUTF32View(codeUnits) }
  
  typealias ExtendedASCII = LazyMapRandomAccessCollection<CodeUnits, UInt32>
  var extendedASCII: ExtendedASCII {
    return codeUnits.lazy.map { UInt32($0) }
  }

  typealias Characters = LazyMapRandomAccessCollection<CodeUnits, Character>
  var characters: Characters {
    return codeUnits.lazy.map {
      Character(UnicodeScalar(UInt32($0))!)
    }
  }
  
  func isASCII(scan: Bool = true) -> Bool {
    if let result = _isASCII { return result }
    return scan && !codeUnits.contains { $0 > 0x7f }
  }
  func isLatin1(scan: Bool = true) -> Bool {
    return true
  }
  func isNormalizedNFC(scan: Bool = true) -> Bool {
    return true
  }
  func isNormalizedNFD(scan: Bool = true) -> Bool {
    return true
  }
  func isInFastCOrDForm(scan: Bool = true) -> Bool {
    return true
  }
}

var t = TestSuite("t")
t.test("basic") {
  let s = "abcdefghijklmnopqrstuvwxyz\n"
  + "🇸🇸🇬🇱🇱🇸🇩🇯🇺🇸\n"
  + "Σὲ 👥🥓γνωρίζω ἀπὸ τὴν κόψη χαῖρε, ὦ χαῖρε, ᾿Ελευθεριά!\n"
  + "Οὐχὶ ταὐτὰ παρίσταταί μοι γιγνώσκειν, ὦ ἄνδρες ᾿Αθηναῖοι,\n"
  + "გთხოვთ ახლავე გაიაროთ რეგისტრაცია Unicode-ის მეათე საერთაშორისო\n"
  + "Зарегистрируйтесь сейчас на Десятую Международную Конференцию по\n"
  + "  ๏ แผ่นดินฮั่นเสื่อมโทรมแสนสังเวช  พระปกเกศกองบู๊กู้ขึ้นใหม่\n"
  + "ᚻᛖ ᚳᚹᚫᚦ ᚦᚫᛏ ᚻᛖ ᛒᚢᛞᛖ ᚩᚾ ᚦᚫᛗ ᛚᚪᚾᛞᛖ ᚾᚩᚱᚦᚹᛖᚪᚱᛞᚢᛗ ᚹᛁᚦ ᚦᚪ ᚹᛖᛥᚫ"
  let s32 = s.unicodeScalars.lazy.map { $0.value }
  let s16 = s.utf16
  let s8 = Array(s.utf8)
  let s16to32 = TranscodedView(s16, from: UTF16.self, to: UTF32.self)
  let s16to8 = TranscodedView(s16, from: UTF16.self, to: UTF8.self)
  let s8to16 = TranscodedView(s8, from: UTF8.self, to: UTF16.self)
  let s8Vto16 = TranscodedView(s8, from: ValidUTF8.self, to: UTF16.self)
  expectTrue(s32.elementsEqual(s16to32))
  expectTrue(s8.elementsEqual(s16to8))
  expectTrue(s16.elementsEqual(s8to16))
  expectTrue(s16.elementsEqual(s8Vto16))

  expectTrue(s32.reversed().elementsEqual(s16to32.reversed()))
  expectTrue(s8.reversed().elementsEqual(s16to8.reversed()))
  expectTrue(s16.reversed().elementsEqual(s8to16.reversed()))
  expectTrue(s16.reversed().elementsEqual(s8Vto16.reversed()))

  do {
    // We happen to know that alphabet is non-ASCII, but we're not going to say
    // anything about that.
    let alphabet = Latin1String(s8.prefix(27))
    expectTrue(alphabet.isASCII())
    expectFalse(alphabet.isASCII(scan: false))
    
    // We know that if you interpret s8 as Latin1, it has a lot of non-ASCII
    let nonASCII = Latin1String(s8) 
    expectFalse(nonASCII.isASCII(scan: true))
    expectFalse(nonASCII.isASCII(scan: false))
  }

  do {
    let alphabet = Latin1String(s8.prefix(27), isASCII: true)
    let nonASCII = Latin1String(s8, isASCII: false)
    expectTrue(alphabet.isASCII())
    expectTrue(alphabet.isASCII(scan: false))
    expectFalse(nonASCII.isASCII(scan: true))
    expectFalse(nonASCII.isASCII(scan: false))
  }
}

t.test("CharacterView") {
  // FIXME: precondition checks in Character prevent us from trying this last
  // one.
  let s = "🇸🇸🇬🇱abc🇱🇸🇩🇯🇺🇸\nΣὲ 👥🥓γ͙᷏̃̂᷀νω" // + "👩‍❤️‍👩"
  let a: [Character] = [
    "🇸🇸", "🇬🇱", "a", "b", "c", "🇱🇸", "🇩🇯", "🇺🇸", "\n",
    "Σ", "ὲ", " ", "👥", "🥓", "γ͙᷏̃̂᷀", "ν", "ω"
  ] // + "👩‍❤️‍👩"
  
  let v8 = CharacterView(Array(s.utf8), UTF8.self)
  expectEqual(a, Array(v8))
  for (n, (c, e)) in zip(v8, a).enumerated() {
    debugLog("###### \(n): \(c) =?= \(e)")
    expectEqual(e, c)
  }
  let v16 = CharacterView(Array(s.utf16), UTF16.self)
  expectEqual(a, Array(v16))

  // FIXME: reverse traversal doesn't work yet.
  logging = true
  for (n, (c, e)) in zip(v8.reversed(), a.reversed()).enumerated() {
    debugLog("###### \(n): \(c) =?= \(e)")
    expectEqual(e, c)
  }
  for (n, (c, e)) in zip(v16.reversed(), a.reversed()).enumerated() {
    debugLog("###### \(n): \(c) =?= \(e)")
    expectEqual(e, c)
  }

  // This one demonstrates that we get grapheme breaking of regional indicators
  // (RI) right, while Swift 3 string does not.
  expectFalse(a.elementsEqual(s.characters))
}
runAllTests()
