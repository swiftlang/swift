//===--- StringNormalization.swift ----------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

internal
struct _NormalizedCodeUnitIterator: IteratorProtocol {
  internal typealias CodeUnit = UInt16
  var segmentBuffer = NormalizationBuffer()
  var intermediateBuffer = NormalizationBuffer()
  var source: _SegmentSource
  
  internal struct NormalizationBuffer {  
    enum Mode {
      case stack, heap
    }
    var stackStorage = _FixedArray16<CodeUnit>(allZeros: ())
    var heapStorage: [CodeUnit] = []
    var count = 0
    var readIndex = 0
    var mode: Mode = .stack
    
    internal mutating func reset() {
      count = 0
      readIndex = 0
    }
    
    internal var isStackBased: Bool {
      return mode == .stack
    }
    
    internal var canRead: Bool {
      return readIndex < count
    }
    
    internal mutating func read() -> CodeUnit {
      _sanityCheck(canRead)
      
      defer { readIndex += 1 }
      return self[readIndex]
    }
    
    internal mutating func promote(toSize size: Int, multiplier: Int = 1) {
      if isStackBased {
        heapStorage = [CodeUnit](repeating: 0, count: (size + count) * multiplier)
        for i in 0..<count {
          heapStorage[i] = stackStorage[i]
        }
        mode = .heap
      } else {
        fatalError("Invariant violated, a single heap allocation buffer is sufficient")
      }
    }
    
    internal subscript(index: Int) -> CodeUnit {
      _sanityCheck(index < count)
      _sanityCheck(index >= 0)
      if isStackBased {
        return stackStorage[index]
      } else {
        return heapStorage[index]
      }
    }
    
    // Comment explaining the contract
    mutating func fill(promotionSize: Int, using f: (UnsafeMutableBufferPointer<CodeUnit>) throws -> Int?) rethrows {
      if isStackBased { 
        if let filled = try stackStorage.withUnsafeMutableBufferPointer({ try f($0) }) {
          count = filled
          return
        }
        promote(toSize: promotionSize)
      }
      
      guard let filled = try heapStorage.withUnsafeMutableBufferPointer({ try f($0) }) else {
        fatalError()
      }
      count = filled
    }
    
    internal mutating func withUnsafeBufferPointer(
      _ f: (UnsafeBufferPointer<CodeUnit>) throws -> Void
    ) rethrows {
      if isStackBased {
        try stackStorage.withUnsafeBufferPointer { arrayBuffer in
          let slice = arrayBuffer[0..<count]
          let buffer = UnsafeBufferPointer(rebasing: slice)
          try f(buffer)
        }
      } else {
        try heapStorage.withUnsafeBufferPointer { arrayBuffer in 
          let slice = arrayBuffer[0..<count]
          let buffer = UnsafeBufferPointer(rebasing: slice)
          try f(buffer)
        }
      }
    }
  }
  
  init<Source: BidirectionalCollection>
    (_ collection: Source)
    where Source.Element == UInt16, Source.SubSequence == Source
  {
    source = _CollectionSource(collection)
  }
  
  init(_ guts: _StringGuts, _ range: Range<Int>, startIndex: Int = 0) {
    source = _StringGutsSource(guts, range, start: startIndex)
  }
  
  mutating func compare(with other: _NormalizedCodeUnitIterator) -> _Ordering {
    var mutableOther = other
    for cu in IteratorSequence(self) {
      if let otherCU = mutableOther.next() {
        let result = _lexicographicalCompare(cu, otherCU)
        if result == .equal {
          continue
        } else {
          return result
        }
      } else {
        //other returned nil, we are greater
        return .greater
      }
    }
    
    //we ran out of code units, either we are equal, or only we ran out and
    //other is greater
    if let _ = mutableOther.next() {
      return .less
    } else {
      return .equal
    }
  }
  
  struct _CollectionSource<Source: BidirectionalCollection>: _SegmentSource
    where Source.Element == UInt16, Source.SubSequence == Source
  {
    var remaining: Int {
      @_specialize(where Source == _UnmanagedString<UInt16>)
      @_specialize(where Source == _UnmanagedOpaqueString)
      get {
        return collection.distance(from: index, to: collection.endIndex)
      }
    }
    var collection: Source
    var index: Source.Index
    
    init(_ collection: Source) {
      self.collection = collection
      index = collection.startIndex
    }
    
    @_specialize(where Source == _UnmanagedString<UInt16>)
    @_specialize(where Source == _UnmanagedOpaqueString)
    mutating func tryFill(buffer: UnsafeMutableBufferPointer<UInt16>) -> Int? {
      var bufferIndex = 0
      let originalIndex = index
      repeat {
        guard index != collection.endIndex else {
          break
        }
        
        guard bufferIndex < buffer.count else {
          //The buffer isn't big enough for the current segment
          index = originalIndex
          return nil
        }
        
        let cu = collection[index]
        buffer[bufferIndex] = cu
        index = collection.index(after: index)
        bufferIndex += 1
      } while !collection.hasNormalizationBoundary(after: collection.index(before: index))
      
      return bufferIndex
    }
  }
  
  struct _StringGutsSource: _SegmentSource {
    var remaining: Int {
      return range.count - index
    }
    var guts: _StringGuts
    var index: Int
    var range: Range<Int>
    
    init(_ guts: _StringGuts, _ range: Range<Int>, start: Int = 0) {
      self.guts = guts
      self.range = range
      index = range.lowerBound + start
    }
    
    mutating func tryFill(buffer: UnsafeMutableBufferPointer<UInt16>) -> Int? {
      var bufferIndex = 0
      let originalIndex = index
      repeat {
        guard index < range.count else {
          break
        }
        
        guard bufferIndex < buffer.count else {
          //The buffer isn't big enough for the current segment
          index = originalIndex
          return nil
        }
        
        let cu = guts.codeUnit(atCheckedOffset: index)
        buffer[bufferIndex] = cu
        index += 1
        bufferIndex += 1
      } while !guts.hasNormalizationBoundary(after: index - 1)
      
      return bufferIndex
    }
  }
  
  private mutating func fillIntermediateBuffer() {
    intermediateBuffer.fill(promotionSize: source.remaining) {
      return source.tryFill(buffer: $0)
    }
  }
  
  private mutating func normalizeIntoSegmentBuffer() {
    let size = source.remaining + intermediateBuffer.count 
      * _Normalization._maxNFCExpansionFactor
    intermediateBuffer.withUnsafeBufferPointer { intermediateBuff in
      segmentBuffer.fill(promotionSize: size) {
        return _tryNormalize(intermediateBuff, into: $0)
      }
    }
  }
  
  internal mutating func next() -> CodeUnit? {    
    if !segmentBuffer.canRead {
      if source.remaining <= 0{
        // Our source of code units to normalize is empty and our buffer from 
        // previous normalizations is also empty.
        return nil
      }
    
      //time to fill the buffer if possible. Otherwise we are done, return nil
      // Normalize segment, and then compare first code unit
      segmentBuffer.reset()
      fillIntermediateBuffer()
      normalizeIntoSegmentBuffer()
    }
    
    return segmentBuffer.read()
  }
}

protocol _SegmentSource {
  var remaining: Int { get }
  mutating func tryFill(buffer: UnsafeMutableBufferPointer<UInt16>) -> Int?
}

extension _SegmentSource {
  mutating func tryFill(
    buffer: UnsafeMutablePointer<_Normalization._SegmentOutputBuffer>
  ) -> Int? {
    return tryFill(buffer: _castOutputBuffer(buffer))
  }
}
