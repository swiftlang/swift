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

internal func __swift_stdlib_U_SUCCESS(_ x: __swift_stdlib_UErrorCode) -> Bool {
 return x.rawValue <= __swift_stdlib_U_ZERO_ERROR.rawValue
}

internal func __swift_stdlib_U_FAILURE(_ x: __swift_stdlib_UErrorCode) -> Bool {
 return x.rawValue > __swift_stdlib_U_ZERO_ERROR.rawValue
}

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

/// A lazy collection of `Encoding.EncodedScalar` that results
/// from parsing an instance of codeUnits using that `Encoding`.
extension _UnicodeViews {
  public struct EncodedScalars {
    let codeUnits: CodeUnits
    
    public init(_ codeUnits: CodeUnits, _: Encoding.Type = Encoding.self) {
      self.codeUnits = codeUnits
    }
  }
}

extension _UnicodeViews.EncodedScalars {
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

/// Collection Conformance
extension _UnicodeViews.EncodedScalars : BidirectionalCollection {
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
    var remainder = codeUnits[i.next..<codeUnits.endIndex]
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
        // If we get here, the encoding couldn't represent a replacement
        // character, so the best we can do is to drop that scalar on the floor
        // and keep going.
        remainder = codeUnits[nextIndex...]
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
        // If we get here, the encoding couldn't represent a replacement
        // character, so the best we can do is to drop that scalar on the floor
        // and keep going.
        remainder = codeUnits[..<priorIndex]
      case .emptyInput:
        fatalError("Indexing past start of code units")
      }
    }
  }
}

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
  
  /// Given `CodeUnits` representing text that has been encoded with
  /// `FromEncoding`, provides a collection of `ToEncoding.CodeUnit`s
  /// representing the same text.
  public struct TranscodedView<ToEncoding : UnicodeEncoding> : BidirectionalCollection {
    public typealias FromEncoding = Encoding
    
    public typealias Segments = Base.Segments
    public var segments: Segments? { return base.segments }

    // We could just be a generic typealias as this type, but it turns
    // out to be impossible, or nearly so, to write the init() below.
    // Instead, we wrap an instance of Base.
    public typealias Base = FlattenBidirectionalCollection<
      LazyMapBidirectionalCollection<
        _UnicodeViews<CodeUnits, FromEncoding>.EncodedScalars,
        ToEncoding.EncodedScalar
      >
    >
    let base: Base

    public init(_ codeUnits: CodeUnits,
      from src: FromEncoding.Type = FromEncoding.self,
      to dst: ToEncoding.Type = ToEncoding.self
    ) {
      base = Base(_UnicodeViews(codeUnits).scalarsTranscoded(to: dst))
    }
    
    // FIXME: this should go in the extension below but for <rdar://30320012>
    //typealias SubSequence = BidirectionalSlice<TranscodedView>
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
    public typealias SubSequence = BidirectionalSlice<TranscodedView>
  }
  
  public func transcoded<OtherEncoding: UnicodeEncoding>(
    to otherEncoding: OtherEncoding.Type
  ) -> TranscodedView<OtherEncoding> {
    return type(of: self).TranscodedView(self.codeUnits, to: otherEncoding)
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
    ).scalars.dropFirst(0)
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
        let chunkSource = _parsedSuffix(fromOffset: nativeTargetIndex)

        for (i, scalar) in zip(chunkSource.indices, chunkSource) {
          let newChunkLength = u.chunkLength + scalar.utf16.count^          
          // don't overfill the buffer
          if newChunkLength > buffer.count^ { break }
          for unit in scalar.utf16 {
            // _debugLog("# unit: \(String(unit, radix: 16))")
            buffer[u.chunkLength^] = unit
            u.chunkLength += 1
          }
          u.chunkNativeLimit = codeUnits.offset(of: i.next)^
        }
      }
      else {
        let chunkSource
          = _parsedSlice(nativeTargetIndex, codeUnits.prefix(upTo:))

        // Transcode the source in reverse, filling the buffer forward
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
        // Reverse the buffer contents to get everything in the right order.
        var b = buffer // copy due to https://bugs.swift.org/browse/SR-3782
        b[..<buffer.index(atOffset: u.chunkLength)].reverse()
      }
    }
    // _debugLog("_access filled buffer, u = \(u)")
    
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
    
    let chunkSource = _parsedSuffix(fromOffset: u[0].chunkNativeStart)
    var chunkOffset = 0
    
    for i in chunkSource.indices {
      chunkOffset += chunkSource[i].utf16.count
      if chunkOffset == u[0].chunkOffset^ {
        return codeUnits.offset(of: i.next)^
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
  public var scalars: EncodedScalars {
    return EncodedScalars(codeUnits, Encoding.self)
  }
}

extension _UnicodeViews {
  
  public struct CharacterView : BidirectionalCollection {

    public init(_ codeUnits: CodeUnits, _: Encoding.Type = Encoding.self) {
      self.storage = _UnicodeViews(codeUnits)
    }

    internal let storage: _UnicodeViews

    public typealias SubSequence = BidirectionalSlice<CharacterView>

    public typealias Index = CodeUnits.Index
    public var startIndex: Index { return storage.codeUnits.startIndex }
    public var endIndex: Index { return storage.codeUnits.endIndex }

    public subscript(i: Index) -> Character {
      let j = index(after: i)
      let contents = _UnicodeViews<
                       // explicit generic parameters shouldn't be needed
                       // <rdar://problem/30882312>
                       CodeUnits.SubSequence,Encoding
                     >(storage.codeUnits[i..<j], Encoding.self)
        
      if let small = Character(_smallUtf8: contents.transcoded(to: UTF8.self)) {
        return small
      }
      else {
        // FIXME: there is undoubtley a less ridiculous way to do this
        let scalars = contents.scalars.lazy.map(UnicodeScalar.init)
        let string = Swift.String(Swift.String.UnicodeScalarView(scalars))
        return Character(_largeRepresentationString: string)
      }
    }     

    public func index(after i: Index) -> Index {
      // FIXME: there is always a grapheme break between two scalars that are both
      // < U+0300.  Use that to optimize.  Can we make a stronger statement, that
      // there's always a break before any scalar < U+0300?
      // _debugLog("index(after: \(i))")
      let nextOffset = _withUBreakIterator(at: i) {
        __swift_stdlib_ubrk_following($0, storage.codeUnits.offset(of: i)^)
      }
      // _debugLog("  index(after: \(i)): \(nextOffset)")
      return storage.codeUnits.index(atOffset: nextOffset)
    }

    public func index(before i: Index) -> Index {
      // FIXME: there is always a grapheme break between two scalars that are both
      // < U+0300.  Use that to optimize.  Can we make a stronger statement, that
      // there's always a break before any scalar < U+0300?
      // _debugLog("index(before: \(i))")
      let previousOffset = _withUBreakIterator(at: i) {
        __swift_stdlib_ubrk_preceding($0, storage.codeUnits.offset(of: i)^)
      }
      // _debugLog("  -> \(previousOffset)")
      return storage.codeUnits.index(atOffset: previousOffset)
    }
    internal func _withUBreakIterator<R>(
      at i: Index, _ body: (OpaquePointer)->R
    ) -> R {
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

internal var _fccNormalizer = _makeFCCNormalizer()

extension _UnicodeViews {
  
  public typealias FCCNormalizedUTF16View = [UInt16]

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

  public var fccNormalizedUTF16: FCCNormalizedUTF16View {
    return _withContiguousUTF16 {
      // Start by assuming we need no more storage than we started with for the
      // result.
      var result  = FCCNormalizedUTF16View(repeating: 0, count: $0.count)
      while true {
        var error = __swift_stdlib_U_ZERO_ERROR
        let usedCount = __swift_stdlib_unorm2_normalize(
          _fccNormalizer, $0.baseAddress, numericCast($0.count),
          &result, numericCast(result.count), &error)
        if __swift_stdlib_U_SUCCESS(error) {
          result.removeLast(result.count - numericCast(usedCount))
          return result
        }
        _sanityCheck(
          error == __swift_stdlib_U_BUFFER_OVERFLOW_ERROR,
          "Unknown normalization error")

        // extend result storage by 25%
        result.append(
          contentsOf: repeatElement(0, count: (result.count + 3) >> 2))
      }
    }
  }
}
