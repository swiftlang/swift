//===--- UnicodeViews.swift -----------------------------------------------===//
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

import SwiftShims

public var _debugLogging = false
public func _debugLog(_ arg: @autoclosure ()->Any) {
  guard _debugLogging == true else { return }
  print(arg())
}
public func _debugLog(_ arg0: @autoclosure ()->Any, _ arg1: @autoclosure ()->Any) {
  guard _debugLogging else { return }
  print(arg0(), arg1())
}

public func __swift_stdlib_U_SUCCESS(_ x: __swift_stdlib_UErrorCode) -> Bool {
 return x.rawValue <= __swift_stdlib_U_ZERO_ERROR.rawValue
}

public func __swift_stdlib_U_FAILURE(_ x: __swift_stdlib_UErrorCode) -> Bool {
 return x.rawValue > __swift_stdlib_U_ZERO_ERROR.rawValue
}

/// Unicode views should conform to this protocol, which supports index
/// interchange and String type erasure.
public protocol _UnicodeView : BidirectionalCollection {
  func index(atEncodedOffset: Int64) -> Index
  static func encodedOffset(of: Index) -> Int64
}

extension _UnicodeView {
  /// Constructs a copy of other
  public init(_ other: Self) { self = other }
}

/// A UnicodeView that is already using AnyUnicodeIndex has trivial interchange
/// with encoded offsets.
extension _UnicodeView where Index == AnyUnicodeIndex {
  public func index(atEncodedOffset x: Int64) -> Index {
    return Index(encodedOffset: x)
  }
  public static func encodedOffset(of i: Index) -> Int64 {
    return i.encodedOffset
  }
}

public protocol UnicodeView : _UnicodeView {
  associatedtype SubSequence: _UnicodeView = UnicodeViewSlice<Self>
}

//===--- RandomAccessUnicodeView ------------------------------------------===//
/// Adapts any `RandomAccessCollection` to a `UnicodeView`, with
/// `encodedOffset`s equal to the number of index steps from the `startIndex`.
///
/// Computing `encodedOffset` this way is pretty safe because if the base view
/// has random access, it must have a constant number N of elements per code
/// unit, and in all the usual instances, N = 1
public struct RandomAccessUnicodeView<Base_: RandomAccessCollection> {
  public typealias Base = Base_
  public typealias Iterator = Base.Iterator
  public var base: Base
  public init(_ base: Base) { self.base = base }
}

extension RandomAccessUnicodeView : BidirectionalCollectionWrapper {
  public struct Index : ForwardingWrapper, Comparable {
    public var base: Base_.IndexDistance
  }
  public typealias IndexDistance = Base.IndexDistance
  public func _wrap(_ i: Base_.Index) -> Index {
    return Index(base: base.offset(of: i))
  }
  public func _unwrap(_ i: Index) -> Base.Index {
    return base.index(atOffset: i.base)
  }
}

extension RandomAccessUnicodeView {
  public mutating func _tryToReplaceSubrange<C: Collection>(
    from targetStart: Index, to targetEnd: Index, with replacement: C
  ) -> Bool
  where C.Iterator.Element == Iterator.Element {
    // UnicodeViews must have value semantics.  Note that this check will fail
    // to work if the view being wrapped is itself a wrapper that forwards
    // _tryToReplaceSubrange to an underlying reference type.
    if Base_.self is AnyObject.Type {
      if (
        withUnsafeMutablePointer(to: &base) {
          !_isUnique(&UnsafeMutableRawPointer($0).assumingMemoryBound(
              to: AnyObject.self).pointee)
        }
      ) { return false }
    }
    return base._tryToReplaceSubrange(
      from: _unwrap(targetStart), to: _unwrap(targetEnd),  with: replacement)
  }
}

extension RandomAccessUnicodeView : RandomAccessCollection {}

extension RandomAccessUnicodeView : UnicodeView {
  public typealias SubSequence
  = RandomAccessUnicodeViewSlice<RandomAccessUnicodeView>
  
  public subscript(bounds: Range<Index>) -> SubSequence {
    return SubSequence(base: self, bounds: bounds)
  }
  
  public func index(atEncodedOffset position: Int64) -> Index {
    return Index(base: position^)
  }
  public static func encodedOffset(of i: Index) -> Int64 {
    return i.base^
  }
}
//===----------------------------------------------------------------------===//

/// A collection of `CodeUnit`s to be interpreted by some `Encoding`.
///
/// View types nested in _UnicodeViews may be suitable *generic* implementation
/// guts for views for models of Unicode, but specific models may want to
/// provide their own implementations.  For example, the UTF16 view of a
/// Latin1String would might be a simple lazy zero-extended mapping, rather than
/// something that goes through the transcoding machinery.
public struct _UnicodeViews<
  CodeUnits : RandomAccessCollection,
  Encoding : UnicodeEncoding
>
where Encoding.EncodedScalar.Iterator.Element == CodeUnits.Iterator.Element,
  CodeUnits.SubSequence : RandomAccessCollection,
  CodeUnits.SubSequence.Index == CodeUnits.Index,
  CodeUnits.SubSequence.SubSequence == CodeUnits.SubSequence,
  CodeUnits.SubSequence.Iterator.Element == CodeUnits.Iterator.Element {

  public init(_ codeUnits: CodeUnits, _: Encoding.Type = Encoding.self) {
    self.codeUnits = codeUnits
  }

  public var codeUnits: CodeUnits
}

//===--- Helper typealias -------------------------------------------------===//
/// A straightforward typealias for _UnicodeViews
///
/// Use this to escape the automatic deduction of the generic arguments given
/// the name `_UnicodeViews` from within nested contexts
/// (https://bugs.swift.org/browse/SR-4155).
internal typealias _UnicodeViews_<
  CodeUnits : RandomAccessCollection,
  Encoding : UnicodeEncoding
>  = _UnicodeViews<CodeUnits, Encoding>
where Encoding.EncodedScalar.Iterator.Element == CodeUnits.Iterator.Element,
  CodeUnits.SubSequence : RandomAccessCollection,
  CodeUnits.SubSequence.Index == CodeUnits.Index,
  CodeUnits.SubSequence.SubSequence == CodeUnits.SubSequence,
CodeUnits.SubSequence.Iterator.Element == CodeUnits.Iterator.Element
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// _UnicodeViews.EncodedScalars
//===----------------------------------------------------------------------===//
/// A lazy collection of `Encoding.EncodedScalar` that results
/// from parsing an instance of codeUnits using that `Encoding`.
extension _UnicodeViews {
  public struct EncodedScalars {
    let codeUnits: CodeUnits

    public typealias Self_ = EncodedScalars
    public init(_ codeUnits: CodeUnits, _: Encoding.Type = Encoding.self) {
      self.codeUnits = codeUnits
    }
  }
  public var encodedScalars: EncodedScalars {
    return EncodedScalars(codeUnits, Encoding.self)
  }
}

extension _UnicodeViews.EncodedScalars {
  public struct Index : Comparable {
    // In one call, parsing produces both:
    // - the buffer of code units comprising the scalar and
    // - the position of the next scalar
    // In a typical subscript-and-advance loop, the same call would need to be
    // repeated twice for each iteration if we didn't store all that information
    // in this index.
    //
    // When parsing is updated to handle a larger chunk at a time (e.g. a SIMD
    // vector's worth), this will become more complicated/fun.
    let base: CodeUnits.IndexDistance
    let scalar: Encoding.EncodedScalar?
    let count: UInt8

    // Having an init makes us impervious to member reordering.
    init(
      base: CodeUnits.IndexDistance,
      count: UInt8,
      scalar: Encoding.EncodedScalar? // There can be no scalar at the end index
    ) {
      self.base = base
      self.count = count
      self.scalar = scalar
    }

    public static func < (lhs: Index, rhs: Index) -> Bool {
      return lhs.base < rhs.base
    }
    public static func == (lhs: Index, rhs: Index) -> Bool {
      return lhs.base == rhs.base
    }
  }
}

/// Collection Conformance
extension _UnicodeViews.EncodedScalars : BidirectionalCollection {
  public var startIndex: Index {
    if _slowPath(codeUnits.isEmpty) { return endIndex }
    return index(after: Index(base: 0, count: 0, scalar: nil))
  }
  
  public var endIndex: Index {
    return Index(base: codeUnits.count, count: 0, scalar: nil)
  }
  
  public subscript(i: Index) -> Encoding.EncodedScalar {
    if let r = i.scalar {
      return r
    }
    return index(after:
      Index(base: i.base, count: 0, scalar: nil)).scalar!
  }

  public func index(after i: Index) -> Index {
    // Parse forward from the beginning of the next encoded unicode scalar
    let startOffset = i.base + i.count^
    let start = codeUnits.index(atOffset: startOffset)
    var remainder = codeUnits[start...]
    
    while true {
      switch Encoding.parse1Forward(remainder, knownCount: 0) {
        
      case .valid(let parsed, let next):
        let count = codeUnits.distance(from: start, to: next)
        return Index(base: startOffset, count: count^, scalar: parsed)
        
      case .error(let next):
        if let r = Encoding.EncodedScalar(UnicodeScalar.replacementCharacter) {
          let count = codeUnits.distance(from: start, to: next)
          return Index(base: startOffset, count: count^, scalar: r)
        }
        // Replacement character not representable; drop the scalar and continue
        remainder = codeUnits[next...]
        
      case .emptyInput:
        return endIndex
      }
    }
  }

  public func index(before i: Index) -> Index {
    // Parse backward from the beginning of the current encoded unicode scalar
    let end = codeUnits.index(atOffset: i.base)
    var remainder = codeUnits[..<end]
    while true {
      switch Encoding.parse1Reverse(remainder, knownCount: 0) {
        
      case .valid(let parsed, let prior):
        let count = codeUnits.distance(from: prior, to: end)
        return Index(
          base: codeUnits.offset(of: prior), count: count^, scalar: parsed)

      case .error(let prior):
        if let r = Encoding.EncodedScalar(UnicodeScalar.replacementCharacter) {
          let count = codeUnits.distance(from: prior, to: end)
          return Index(
            base: codeUnits.offset(of: prior), count: count^, scalar: r)
        }
        // Replacement character not representable; drop the scalar and continue
        remainder = codeUnits[..<prior]
        
      case .emptyInput:
        fatalError("Indexing past start of code units")
      }
    }
  }
}

extension _UnicodeViews.EncodedScalars : UnicodeView {
  public func index(atEncodedOffset offset: Int64) -> Index {
    return index(after: Index(base: offset^, count: 0, scalar: nil))
  }
  public static func encodedOffset(of i: Index) -> Int64 {
    return numericCast(i.base)
  }
  public typealias SubSequence = UnicodeViewSlice<Self_>
  public subscript(bounds: Range<Index>) -> SubSequence {
    return SubSequence(base: self, bounds: bounds)
  }
}

//===----------------------------------------------------------------------===//
// _UnicodeViews.UnicodeScalars
//===----------------------------------------------------------------------===//

/// A lazy collection of `Encoding.EncodedScalar` that results
/// from parsing an instance of codeUnits using that `Encoding`.
extension _UnicodeViews {
  public struct Scalars {
    let codeUnits: CodeUnits

    public typealias Self_ = Scalars
    public init(_ codeUnits: CodeUnits, _: Encoding.Type = Encoding.self) {
      self.codeUnits = codeUnits
    }
  }
  public var scalars: Scalars {
    return Scalars(codeUnits, Encoding.self)
  }
}

/// Collection Conformance
extension _UnicodeViews.Scalars : BidirectionalCollection {
  public typealias Base = _UnicodeViews<CodeUnits, Encoding>.EncodedScalars
  public typealias Element = UnicodeScalar
  public typealias Index = Base.Index

  var base: Base { return Base(codeUnits) }
  
  public var startIndex: Index {
    return base.startIndex
  }
  
  public var endIndex: Index {
    return base.endIndex
  }
  
  public subscript(i: Index) -> Element {
    return UnicodeScalar(base[i])
  }

  public func index(after i: Index) -> Index {
    return base.index(after: i)
  }

  public func index(before i: Index) -> Index {
    return base.index(before: i)
  }
}

extension _UnicodeViews.Scalars : UnicodeView {
  public func index(atEncodedOffset offset: Int64) -> Index {
    return base.index(atEncodedOffset: offset)
  }
  public static func encodedOffset(of i: Index) -> Int64 {
    return Base.encodedOffset(of: i)
  }
  public typealias SubSequence = UnicodeViewSlice<Self_>
  public subscript(bounds: Range<Index>) -> SubSequence {
    return SubSequence(base: self, bounds: bounds)
  }
}

//===----------------------------------------------------------------------===//
// _UnicodeViews.ScalarsTranscoded<ToEncoding>
//===----------------------------------------------------------------------===//
extension _UnicodeViews {
  public typealias ScalarsTranscoded<ToEncoding : UnicodeEncoding>
  = LazyMapBidirectionalCollection<EncodedScalars, ToEncoding.EncodedScalar>

  public func scalarsTranscoded<ToEncoding : UnicodeEncoding>(
    to dst: ToEncoding.Type
  )
  -> ScalarsTranscoded<ToEncoding> {
    return _UnicodeViews.EncodedScalars(codeUnits, Encoding.self).lazy.map {
      dst.encode($0)!
    }
  }
}

//===----------------------------------------------------------------------===//
// _UnicodeViews.TranscodedView<ToEncoding>
//===----------------------------------------------------------------------===//
extension _UnicodeViews {
  public typealias TranscodedView<ToEncoding : UnicodeEncoding>
  = _TranscodedView<CodeUnits, Encoding, ToEncoding>
  
  public func transcoded<ToEncoding>(
    to targetEncoding: ToEncoding.Type
  ) -> TranscodedView<ToEncoding> {
    return type(of: self).TranscodedView(self.codeUnits, to: targetEncoding)
  }
}
  /// Given `CodeUnits` representing text that has been encoded with
  /// `FromEncoding`, provides a collection of `ToEncoding.CodeUnit`s
  /// representing the same text.
public struct _TranscodedView<
CodeUnits : RandomAccessCollection,
FromEncoding_ : UnicodeEncoding,
ToEncoding : UnicodeEncoding
> 
	: BidirectionalCollection 
where FromEncoding_.EncodedScalar.Iterator.Element == CodeUnits.Iterator.Element,
  CodeUnits.SubSequence : RandomAccessCollection,
  CodeUnits.SubSequence.Index == CodeUnits.Index,
  CodeUnits.SubSequence.SubSequence == CodeUnits.SubSequence,
  CodeUnits.SubSequence.Iterator.Element == CodeUnits.Iterator.Element
{
  public typealias FromEncoding = FromEncoding_
    
  let codeUnits: CodeUnits

  // This view is, in spirit, the result of flattening the ScalarsTranscoded
  // view.  We flatten that view of the codeUnits' slice type just to make
  // index translation more straightforward.
  public typealias Base = FlattenBidirectionalCollection<
    _UnicodeViews<CodeUnits.SubSequence, FromEncoding>
    .ScalarsTranscoded<ToEncoding>
  >
  public typealias Index = Base.Index

  var base: Base {
    return Base(
      _UnicodeViews_(codeUnits[...]).scalarsTranscoded(to: ToEncoding.self))
  }

  public init(_ codeUnits: CodeUnits,
    from src: FromEncoding.Type = FromEncoding.self,
    to dst: ToEncoding.Type = ToEncoding.self
  ) {
    self.codeUnits = codeUnits
  }

  // FIXME: create generalized [Bidi|Random]CollectionWrapper protocols
  // instead of repeating this boilerplate.
  public var startIndex : Base.Index {
    return base.startIndex
  }
  public var endIndex : Base.Index {
    return base.endIndex
  }
  public subscript(i: Base.Index) -> Base.Iterator.Element {
    return base[i]
  }
  public func index(after i: Base.Index) -> Base.Index {
    return base.index(after: i)
  }
  public func index(before i: Base.Index) -> Base.Index {
    return base.index(before: i)
  }
  public typealias Segments = Base.Segments
  public var segments: Segments? { return base.segments }

  public typealias SubSequence = UnicodeViewSlice<_TranscodedView>
  public subscript(bounds: Range<Index>) -> SubSequence {
    return SubSequence(base: self, bounds: bounds)
  }
}


extension _TranscodedView : UnicodeView {
  public func index(atEncodedOffset offset: Int64) -> Base.Index {
    let scalarsTranscoded = base._base
    let encodedScalars = scalarsTranscoded._base
    let outerIndex = encodedScalars.index(atEncodedOffset: offset)
    if outerIndex == encodedScalars.endIndex { return endIndex }
    return Index(outerIndex, scalarsTranscoded[outerIndex].startIndex)
  }
  public static func encodedOffset(of i: Index) -> Int64 {
    return _UnicodeViews<CodeUnits.SubSequence, FromEncoding>
    .Scalars.encodedOffset(of: i._outer)
  }
}

extension _UnicodeViews : _UTextable {
  internal func _nativeLength(_ uText: inout _UText) -> Int64 {
    uText.validate()
    return codeUnits.count^
  }

  internal func _parsedSlice(
    _ offset: Int64,
    _ slice: (CodeUnits.Index) -> CodeUnits.SubSequence
  ) -> _UnicodeViews<CodeUnits.SubSequence,Encoding>.EncodedScalars.SubSequence {
    return _UnicodeViews_(
      slice(codeUnits.index(atOffset: offset)), Encoding.self
    ).encodedScalars.dropFirst(0)
  }

  internal func _parsedSuffix(
    fromOffset offset: Int64
  ) -> _UnicodeViews<CodeUnits.SubSequence,Encoding>.EncodedScalars.SubSequence {
    return _parsedSlice(offset, codeUnits.suffix(from:))
  }

  internal func _clone(
    _ dst: UnsafeMutablePointer<_UText>?, _ src: UnsafePointer<_UText>,
    _ deep: Bool, _ status: UnsafeMutablePointer<_UErrorCode>?
  ) ->  UnsafeMutablePointer<_UText> {
    _sanityCheck(!deep, "deep cloning not supported")
    UnsafeMutablePointer(mutating: src)[0].validate()
    // _debugLog("_clone with dst = \(String(describing: dst))")
    // _debugLog("src: \(src[0])")
    let r = dst
      ?? UnsafeMutablePointer.allocate(capacity: MemoryLayout<_UText>.size)
    r[0] = src[0]
    r[0].setup()
    r[0].validate()
    // _debugLog("clone result: \(r[0])")
    return r
  }

  internal func _access(
    _ u: inout _UText, _ nativeTargetIndex: Int64, _ forward: Bool
  ) -> Bool {

    // _debugLog("_access(u: \(u), nativeTargetIndex: \(nativeTargetIndex), forward: \(forward))")
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
    // _debugLog("_access: filling buffer")

    // FIXME: should we use parseForward/parseReverse on some slice?
    
    guard (0...codeUnits.count^).contains(nativeTargetIndex)
    else { return false }

    u.chunkLength = 0
    u.chunkNativeStart = nativeTargetIndex
    u.chunkNativeLimit = nativeTargetIndex
    
    u.withBuffer { buffer in
      if forward {
        let scalars = _parsedSuffix(fromOffset: nativeTargetIndex)

        for scalar in scalars {
          // don't overfill the buffer
          if u.chunkLength + scalar.utf16.count^ > buffer.count^ { break }
          for unit in scalar.utf16 {
            // _debugLog("# unit: \(String(unit, radix: 16))")
            buffer[u.chunkLength^] = unit
            u.chunkLength += 1
          }
          u.chunkNativeLimit += scalar.count^
        }
      }
      else {
        let scalars
          = _parsedSlice(nativeTargetIndex, codeUnits.prefix(upTo:))

        // Transcode the source in reverse, filling the buffer forward
        for scalar in scalars.reversed() {
          // don't overfill the buffer
          if u.chunkLength + scalar.utf16.count^ > buffer.count^ { break }
          for unit in scalar.utf16.reversed() {
            buffer[u.chunkLength^] = unit
            u.chunkLength += 1
          }
          u.chunkNativeStart -= scalar.count^
        }
        u.chunkOffset = u.chunkLength
        // Reverse the buffer contents to get everything in the right order.
        var b = buffer // copy due to https://bugs.swift.org/browse/SR-3782
        b[..<buffer.index(atOffset: u.chunkLength)].reverse()
      }
    }
    // _debugLog("_access filled buffer, u = \(u)")

    u.validate()
    
    // WORKAROUND? <rdar://30979421>: UBreakIterator attempts to index UText
    // out-of-range if we don't treat requests for empty chunks as
    // out-of-bounds.
    return u.chunkLength > 0
  }
  
  internal func _extract(
    _ u: inout _UText,
    _ nativeStart: Int64, _ nativeLimit: Int64,
    _ destination: UnsafeMutableBufferPointer<__swift_stdlib_UChar>,
    _ error: UnsafeMutablePointer<_UErrorCode>?
  ) -> Int32 {
    // _debugLog("_extract: \(u)")
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
      // FIXME: we should be using an associated UTF16View type here rather than
      // the generic TranscodedView, which is likely to be less efficient in
      // some common cases.
      let source
        = _UnicodeViews_(base, Encoding.self).transcoded(to: UTF16.self)
      var d = destination // copy due to https://bugs.swift.org/browse/SR-3782
      let (limit, remainder) = d.copy(from: source)
      
      // Add null termination if it fits
      if limit < d.endIndex { d[limit] = 0 }

      // If no overflow, we're done
      if remainder.isEmpty { return Int32(destination.offset(of: limit)) }

      // Report the error and account for the overflow length in the return value
      error![0] = __swift_stdlib_U_BUFFER_OVERFLOW_ERROR
      return Int32(destination.offset(of: limit) + remainder.count)
    }
    return 0
  }
  
  internal func _mapOffsetToNative(_ u: UnsafePointer<_UText>) -> Int64 {
    UnsafeMutablePointer(mutating: u)[0].validate()
    
    if u[0].chunkOffset == 0 { return u[0].chunkNativeStart }

    // Advance scalar by scalar in the source until we find the offset
    let scalars = _parsedSuffix(fromOffset: u[0].chunkNativeStart)
    var utf16Offset = 0, nativeOffset = 0
    for s in scalars {
      nativeOffset += s.count^
      utf16Offset += s.utf16.count^
      if utf16Offset == u[0].chunkOffset^ {
        return u[0].chunkNativeStart + nativeOffset^
      }
    }
    fatalError("supposed to be unreachable")
  }
  
  internal func _mapNativeIndexToUTF16(_ u: UnsafePointer<_UText>, _ nativeIndex: Int64) -> Int32 {
    // _debugLog("_mapNativeIndexToUTF16: \(u)")
    UnsafeMutablePointer(mutating: u)[0].validate()
    
    if u[0].chunkNativeStart == nativeIndex { return 0 }

    let nativeChunk = codeUnits[
      codeUnits.index(atOffset: u[0].chunkNativeStart)
      ..<
      codeUnits.index(atOffset: nativeIndex)]
    
    return _UnicodeViews_(
      nativeChunk, Encoding.self).transcoded(to: UTF16.self).count^
  }
}

extension _UnicodeViews {
  
  public struct CharacterView : UnicodeView {

    public init(_ codeUnits: CodeUnits, _: Encoding.Type = Encoding.self) {
      self.storage = _UnicodeViews(codeUnits)
    }

    internal let storage: _UnicodeViews

    public typealias SubSequence = UnicodeViewSlice<CharacterView>
    public subscript(bounds: Range<Index>) -> SubSequence {
      return SubSequence(base: self, bounds: bounds)
    }

    public struct Index : ForwardingWrapper, Comparable {
      public var base: CodeUnits.IndexDistance
    }
    
    public func index(atEncodedOffset position: Int64) -> Index {
      return Index(base: position^)
    }
    public static func encodedOffset(of i: Index) -> Int64 {
      return i.base^
    }
    
    public var startIndex: Index { return Index(base: 0) }
    public var endIndex: Index {
      return Index(base: storage.codeUnits.count)
    }

    public subscript(i: Index) -> Character {
      let j = index(after: i)
      let contents = _UnicodeViews_(
        storage.codeUnits[
          storage.codeUnits.index(atOffset: i.base)
          ..< storage.codeUnits.index(atOffset: j.base)],
        Encoding.self)
        
      if let small = Character(_smallUtf8: contents.transcoded(to: UTF8.self)) {
        return small
      }
      else {
        // FIXME: there is undoubtley a less ridiculous way to do this
        let scalars = contents.encodedScalars.lazy.map(UnicodeScalar.init)
        let string = Swift.String(Swift.String.UnicodeScalarView(scalars))
        return Character(_largeRepresentationString: string)
      }
    }     

    public func index(after i: Index) -> Index {
      // Michael note: The below FIXME is not true for CR-LF
      //
      // FIXME: there is always a grapheme break between two scalars that are
      // both < U+0300.  Use that to optimize.  Can we make a stronger
      // statement, that there's always a break before any scalar < U+0300?
      // _debugLog("index(after: \(i))")
      let nextOffset = _withUBreakIterator {
        __swift_stdlib_ubrk_following($0, i.base^)
      }
      // _debugLog("  index(after: \(i)): \(nextOffset)")
      return Index(base: nextOffset^)
    }

    public func index(before i: Index) -> Index {
      // Michael note: The below FIXME is not true for CR-LF
      //
      // FIXME: there is always a grapheme break between two scalars that are
      // both < U+0300.  Use that to optimize.  Can we make a stronger
      // statement, that there's always a break before any scalar < U+0300?
      // _debugLog("index(before: \(i))")
      let previousOffset = _withUBreakIterator {
        __swift_stdlib_ubrk_preceding($0, i.base^)
      }
      // _debugLog("  -> \(previousOffset)")
      return Index(base: previousOffset^)
    }
    
    internal func _withUBreakIterator<R>(_ body: (OpaquePointer)->R) -> R {
      var err = __swift_stdlib_U_ZERO_ERROR;

      // _debugLog("ubrk_open")
      let bi = __swift_stdlib_ubrk_open(
        /*type:*/ __swift_stdlib_UBRK_CHARACTER, /*locale:*/ nil,
        /*text:*/ nil, /*textLength:*/ 0, /*status:*/ &err)
      _precondition(err.isSuccess, "unexpected ubrk_open failure")
      defer { __swift_stdlib_ubrk_close(bi) }

      return storage._withUText { u in
        // _debugLog("ubrk_setUText(breakIterator: \(bi), u: \(u)")
        // _debugLog("u: \(u.pointee)")
        __swift_stdlib_ubrk_setUText(bi, u, &err)
        _precondition(err.isSuccess, "unexpected ubrk_setUText failure")
        return body(bi)
      }
    }  
  }

  public var characters: CharacterView {
    return CharacterView(codeUnits, Encoding.self)
  }
}

internal func _makeFCCNormalizer() -> OpaquePointer {
  var err = __swift_stdlib_U_ZERO_ERROR;
  let ret = __swift_stdlib_unorm2_getInstance(
    nil, "nfc", __swift_stdlib_UNORM2_COMPOSE_CONTIGUOUS, &err)
  _precondition(err.isSuccess, "unexpected unorm2_getInstance failure")
  return ret!
}

// Michael NOTE: made public for prototype, should be internal
public var _fccNormalizer = _makeFCCNormalizer()

extension _UnicodeViews {
  /// Invokes `body` on a contiguous buffer of our UTF16.
  ///
  /// - Note: `body` should be prepared to deal with invalid UTF16.
  internal func _withContiguousUTF16<R>(
    _ body: (UnsafeBufferPointer<UTF16.CodeUnit>)->R
  ) -> R {
    if Encoding.EncodedScalar.self is UTF16.EncodedScalar.Type {
      // It's a UTF16 encoding
      let r: R? = codeUnits.withExistingUnsafeBuffer {
        body($0 as Any as! UnsafeBufferPointer<UTF16.CodeUnit>)
      }
      if r != nil { return r! }
    }
    return Array(self.transcoded(to: UTF16.self)).withUnsafeBufferPointer(body)
  }
}

// Michael NOTE: Trying to nest this crashed the remangler...
//
// A Latin1 character view, more efficient than a general purpose character
// view. Checks for special cases inside Latin1, otherwise code-unit based.
//
// FIXME: Better name here. It's not just Latin1, but it's also a valid (TODO:
// prove) view for many ranges of unicode scalar values. Maybe name based on the
// GB_n level of rule application from the Unicode spec?
//
// TODO: Would it be better to unify under the general character view, and
// incorporate our fast paths as applicable? This seems like the better long-
// term direction to take here.
//
public struct Latin1CharacterView<
  CodeUnits : RandomAccessCollection,
  Encoding : UnicodeEncoding
> : UnicodeView
  where Encoding.EncodedScalar.Iterator.Element == CodeUnits.Iterator.Element,
  CodeUnits.SubSequence : RandomAccessCollection,
  CodeUnits.SubSequence.Index == CodeUnits.Index,
  CodeUnits.SubSequence.SubSequence == CodeUnits.SubSequence,
  CodeUnits.SubSequence.Iterator.Element == CodeUnits.Iterator.Element,
  CodeUnits.Iterator.Element : UnsignedInteger
{
  typealias CodeUnit = CodeUnits.Iterator.Element
  internal let _CR: CodeUnit = 0x0D
  internal let _LF: CodeUnit = 0x0A

  public init(_ codeUnits: CodeUnits) {
    print("created Latin1 character view")
    self.codeUnits = codeUnits
  }

  internal let codeUnits: CodeUnits

  public typealias SubSequence = UnicodeViewSlice<Latin1CharacterView>
  public subscript(bounds: Range<Index>) -> SubSequence {
    return SubSequence(base: self, bounds: bounds)
  }

  public struct Index : ForwardingWrapper, Comparable {
    public var base: CodeUnits.IndexDistance
  }

  public func index(atEncodedOffset position: Int64) -> Index {
    return Index(base: numericCast(position))
  }
  public static func encodedOffset(of i: Index) -> Int64 {
    return numericCast(i.base)
  }

  public var startIndex: Index { return Index(base: 0) }
  public var endIndex: Index { return Index(base: codeUnits.count) }

  internal func getCU(at i: Index) -> CodeUnit {
    let idx = codeUnits.index(atOffset: i.base)
    return codeUnits[idx]
  }
  internal func getIndex(_ i: CodeUnits.Index) -> Index {
    return Index(base: codeUnits.distance(from: codeUnits.startIndex, to: i))
  }

  public subscript(i: Index) -> Character {
    let nextIdx = index(after: i)
    print("subscripting the Latin1 character view")

    // Fast path: Single code unit character (i.e. not CR-LF)
    if _fastPath(nextIdx.base == i.base+1) {
      return Character(UnicodeScalar(numericCast(getCU(at: i))))
    }

    // FIXME: Is there anything else in Latin1 except CR-LF that's not single-
    // code-unit-is-single-grapheme?
    _sanityCheck(nextIdx.base == i.base+2)
    _sanityCheck(getCU(at: i) == _CR)
    _sanityCheck(getCU(at: nextIdx) == _LF)
    return Character("\u{0D}\u{0A}")
  }

  public func index(after i: Index) -> Index {
    print("index(after:) the Latin1 character view")
    let nextCUIdx = codeUnits.index(atOffset: i.base+1)
    // Fast path: Single code unit character (i.e. not CR-LF)
    if _fastPath(getCU(at: i) != _CR) {
      return getIndex(nextCUIdx)
    }

    // Special case: CR-LF is single grapheme
    if nextCUIdx != codeUnits.endIndex && codeUnits[nextCUIdx] == _LF {
      return getIndex(codeUnits.index(after: nextCUIdx))
    }

    // FIXME: Is there anything else in Latin1 except CR-LF that's not single-
    // code-unit-is-single-grapheme?
    return getIndex(nextCUIdx)
  }

  public func index(before i: Index) -> Index {
    print("index(before:) the Latin1 character view")
    let previousCUIdx = codeUnits.index(atOffset: i.base-1)
    // Fast path: Single code unit character (i.e. not CR-LF)
    if _fastPath(codeUnits[previousCUIdx] != _LF) {
      return getIndex(previousCUIdx)
    }

    // Special case: CR-LF is single grapheme
    if previousCUIdx != codeUnits.startIndex {
      let previousPreviousCUIdx = codeUnits.index(before: previousCUIdx)
      if codeUnits[previousPreviousCUIdx] == _CR {
        return getIndex(previousPreviousCUIdx)
      }
    }

    // FIXME: Is there anything else in Latin1 except CR-LF that's not single-
    // code-unit-is-single-grapheme?
    return getIndex(previousCUIdx)
  }
}

// TODO: Also valid for other encodings when properties over scalar values still
// apply.
extension _UnicodeViews where CodeUnits.Iterator.Element : UnsignedInteger {
  // TODO: Currently, present for all Strings. Might want to have type
  // restrictions.
  public var latin1CharacterView: Latin1CharacterView<CodeUnits, Encoding> {
    return Latin1CharacterView<CodeUnits, Encoding>(codeUnits)
  }
}
