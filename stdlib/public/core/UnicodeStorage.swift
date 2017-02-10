//===--- UnicodeStorage.swift ---------------------------------------------===//
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

/// An index type for views onto random access collections whose elements are
/// effectively variable-width.
public protocol UnicodeIndexProtocol {
  var codeUnitOffset: Int64 { get }
}

extension UnicodeIndexProtocol {
  public static func == (l: UnicodeIndexProtocol, r: UnicodeIndexProtocol) -> Bool {
    return l.codeUnitOffset == r.codeUnitOffset
  }
  public static func < (l: UnicodeIndexProtocol, r: UnicodeIndexProtocol) -> Bool {
    return l.codeUnitOffset < r.codeUnitOffset
  }
  
  public static func == (l: Self, r: Self) -> Bool {
    return l.codeUnitOffset == r.codeUnitOffset
  }
  public static func < (l: Self, r: Self) -> Bool {
    return l.codeUnitOffset < r.codeUnitOffset
  }
}

/// A collection of `CodeUnit`s to be interpreted by some `Encoding`
public struct UnicodeStorage<
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
  
  public let codeUnits: CodeUnits
}


/// A lazy collection of `Encoding.EncodedScalar` that results
/// from parsing an instance of codeUnits using that `Encoding`.
extension UnicodeStorage {

  public struct EncodedScalars {
    let codeUnits: CodeUnits
    
    public init(_ codeUnits: CodeUnits, _: Encoding.Type = Encoding.self) {
      self.codeUnits = codeUnits
    }
  }
}

extension UnicodeStorage.EncodedScalars {
  // Because parsing produces a buffer and a new index, to avoid
  // repeatedly decoding the same data, this index stores that buffer
  // and the next index.  This would obviously be more complicated if
  // the buffer contained more than a single scalar (and it probably
  // should).
  public struct Index : UnicodeIndexProtocol, Comparable {
    let offset: CodeUnits.IndexDistance
    // FIXME: We might get a much better memory footprint if we used a
    // UInt8 to store the distance between base and next, rather than
    // storing next explicitly.  CodeUnits will be random-access in
    // practice.
    let nextStride: UInt8

    public var codeUnitOffset: Int64 { return numericCast(offset) }
    
    var nextOffset: CodeUnits.IndexDistance {
      return offset + numericCast(nextStride)
    }
    
    // FIXME: there should be an invalid inhabitant we can use in
    // EncodedScalar so as not to waste a separate bool here.
    let scalar: Encoding.EncodedScalar?
  }
}

/// Collection Conformance
extension UnicodeStorage.EncodedScalars : BidirectionalCollection {
  public var startIndex: Index {
    if _slowPath(codeUnits.isEmpty) { return endIndex }
    return index(after: Index(offset: 0, nextStride: 0, scalar: nil))
  }
  
  public var endIndex: Index {
    return Index(offset: codeUnits.count, nextStride: 0, scalar: nil)
  }
  
  public subscript(i: Index) -> Encoding.EncodedScalar {
    if let r = i.scalar {
      return r
    }
    return index(after:
      Index(offset: i.offset, nextStride: 0, scalar: nil)).scalar!
  }

  public func index(after i: Index) -> Index {
    let p = codeUnits.index(atOffset: i.nextOffset)
    var remainder = codeUnits[p...]
    while true {
      switch Encoding.parse1Forward(remainder, knownCount: 0) {
      case .valid(let scalar, let nextIndex):
        return Index(
          offset: i.nextOffset,
          nextStride: numericCast(remainder.offset(of: nextIndex)),
          scalar: scalar)
      case .error(let nextIndex):
        // FIXME: don't go through UnicodeScalar once this is in the stdlib
        if let replacement = Encoding.encode(
          UTF32.EncodedScalar(UnicodeScalar(0xFFFD)!)) {
          return Index(
            offset: i.nextOffset,
            nextStride: numericCast(remainder.offset(of: nextIndex)),
            scalar: replacement)
        }
        remainder = remainder.dropFirst()
      case .emptyInput:
        return endIndex
      }
    }
  }

  public func index(before i: Index) -> Index {
    var remainder = codeUnits[..<codeUnits.index(atOffset: i.offset)]
    while true {
      switch Encoding.parse1Reverse(remainder, knownCount: 0) {
      case .valid(let scalar, let priorIndex):
        let stride = remainder[priorIndex...].count
        return Index(
          offset: i.offset - numericCast(stride),
          nextStride: numericCast(stride),
          scalar: scalar)
      case .error(let priorIndex):
        let stride = remainder[priorIndex...].count
        // FIXME: don't go through UnicodeScalar once this is in the stdlib
        if let replacement = Encoding.encode(
          UTF32.EncodedScalar(UnicodeScalar(0xFFFD)!)) {
          return Index(
            offset: i.offset - numericCast(stride),
            nextStride: numericCast(stride),
            scalar: replacement)        
        }
        remainder = remainder.dropLast()
      case .emptyInput:
        fatalError("Indexing past start of code units")
      }
    }
  }
}

extension UnicodeStorage {
  /// Given `CodeUnits` representing text that has been encoded with
  /// `FromEncoding`, provides a collection of `ToEncoding.CodeUnit`s
  /// representing the same text.
  public struct TranscodedView<ToEncoding : UnicodeEncoding> : BidirectionalCollection {
    public typealias FromEncoding = Encoding
    
    // We could just be a generic typealias as this type, but it turns
    // out to be impossible, or nearly so, to write the init() below.
    // Instead, we wrap an instance of Base.
    public typealias Base = FlattenBidirectionalCollection<
      LazyMapBidirectionalCollection<
        UnicodeStorage<CodeUnits, FromEncoding>.EncodedScalars,
        ToEncoding.EncodedScalar
      >
    >
    let base: Base

    public init(_ codeUnits: CodeUnits,
      from src: FromEncoding.Type = FromEncoding.self,
      to dst: ToEncoding.Type = ToEncoding.self
    ) {
      base = Base(UnicodeStorage<CodeUnits, FromEncoding>.EncodedScalars(codeUnits, src).lazy.map {
          dst.encode($0)!
        })
    }
    
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
}

extension UnicodeStorage : _UTextable {
  internal func _nativeLength(_ uText: inout _UText) -> Int64 {
    uText.validate()
    return codeUnits.count^
  }

  internal func _parsedSlice(
    _ offset: Int64,
    _ slice: (CodeUnits.Index) -> CodeUnits.SubSequence
  ) -> UnicodeStorage<CodeUnits.SubSequence,Encoding>.EncodedScalars.SubSequence {
    return UnicodeStorage<CodeUnits.SubSequence, Encoding>(
      slice(codeUnits.index(atOffset: offset)), Encoding.self
    ).scalars.dropFirst(0)
  }

  internal func _parsedSuffix(
    fromOffset offset: Int64
  ) -> UnicodeStorage<CodeUnits.SubSequence,Encoding>.EncodedScalars.SubSequence {
    return _parsedSlice(offset, codeUnits.suffix(from:))
  }

  internal func _clone(
    _ dst: UnsafeMutablePointer<_UText>?, _ src: UnsafePointer<_UText>,
    _ deep: Bool, _ status: UnsafeMutablePointer<_UErrorCode>?
  ) ->  UnsafeMutablePointer<_UText> {
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
          u.chunkNativeLimit = i.nextOffset^
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
          u.chunkNativeStart = i.codeUnitOffset
          u.chunkOffset = u.chunkLength
        }
        var b = buffer // copy due to https://bugs.swift.org/browse/SR-3782
        b[..<buffer.index(atOffset: u.chunkLength)].reverse()
      }
    }
    // _debugLog("_access filled buffer, u = \(u)")
    return true
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
      let source = UnicodeStorage<CodeUnits.SubSequence,Encoding>.TranscodedView(
        base, from: Encoding.self, to: UTF16.self)
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
        return i.nextOffset^
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
    
    return UnicodeStorage<CodeUnits.SubSequence,Encoding>.TranscodedView(
      nativeChunk, from: Encoding.self, to: UTF16.self
    ).count^
  }
}

extension UnicodeStorage {
  var scalars: EncodedScalars {
    return EncodedScalars(codeUnits, Encoding.self)
  }
}

extension UnicodeStorage {
  
  public struct CharacterView<
    CodeUnits : RandomAccessCollection,
    Encoding : UnicodeEncoding
  > : BidirectionalCollection
  where Encoding.EncodedScalar.Iterator.Element == CodeUnits.Iterator.Element,
    CodeUnits.SubSequence : RandomAccessCollection,
    CodeUnits.SubSequence.Index == CodeUnits.Index,
    CodeUnits.SubSequence.SubSequence == CodeUnits.SubSequence,
    CodeUnits.SubSequence.Iterator.Element == CodeUnits.Iterator.Element {

    public init(_ codeUnits: CodeUnits, _: Encoding.Type = Encoding.self) {
      self.storage = UnicodeStorage<CodeUnits,Encoding>(codeUnits)
    }

    internal let storage: UnicodeStorage<CodeUnits, Encoding>

    public typealias SubSequence = BidirectionalSlice<CharacterView>

    public typealias Index = CodeUnits.Index

    public var startIndex: Index { return storage.codeUnits.startIndex }
    public var endIndex: Index { return storage.codeUnits.endIndex }

    public subscript(i: Index) -> Character {
      // _debugLog("subscript: i=\(i)")
      let j = index(after: i)
      // _debugLog("subscript: j=\(j)")
      let scalars = UnicodeStorage<CodeUnits.SubSequence, Encoding>(
        storage.codeUnits[i..<j], Encoding.self
      ).scalars
      // _debugLog("scalars: \(Array(scalars))")
      return Character(scalars.lazy.map { UnicodeScalar($0) })
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
        __swift_stdlib_ubrk_setUText(bi, u, &err)
        _precondition(err.isSuccess, "unexpected ubrk_setUText failure")
        return body(bi)
      }
    }  
  }
}
