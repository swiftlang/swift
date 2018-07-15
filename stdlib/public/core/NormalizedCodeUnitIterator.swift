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
  var segmentBuffer = _FixedArray16<CodeUnit>(allZeros:())
  var overflowBuffer: [CodeUnit]? = nil
  var normalizationBuffer: [CodeUnit]? = nil
  var source: _SegmentSource
  var segmentBufferIndex = 0
  var segmentBufferCount = 0
  var overflowBufferIndex = 0
  var overflowBufferCount = 0
  
  typealias CodeUnit = UInt16
  
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
        
        let cu = guts[index]
        buffer[bufferIndex] = cu
        index += 1
        bufferIndex += 1
      } while !guts.hasNormalizationBoundary(after: index - 1)
      
      return bufferIndex
    }
  }
  
  mutating func next() -> CodeUnit? {
    if segmentBufferCount == segmentBufferIndex {
      segmentBuffer = _FixedArray16<CodeUnit>(allZeros:())
      segmentBufferCount = 0
      segmentBufferIndex = 0
    }
    
    if overflowBufferCount == overflowBufferIndex {
      overflowBufferCount = 0
      overflowBufferIndex = 0
    }
    
    if source.remaining <= 0 
    && segmentBufferCount == 0 
    && overflowBufferCount == 0 {
      // Our source of code units to normalize is empty and our buffers from 
      // previous normalizations are also empty.
      return nil
    }
    
    if segmentBufferCount == 0 && overflowBufferCount == 0 {
      //time to fill a buffer if possible. Otherwise we are done, return nil
      // Normalize segment, and then compare first code unit
      var intermediateBuffer = _FixedArray16<CodeUnit>(allZeros:())
      if overflowBuffer == nil, 
         let filled = source.tryFill(buffer: &intermediateBuffer) 
      {
        guard let count = _tryNormalize(
          _castOutputBuffer(&intermediateBuffer, 
          endingAt: filled), 
          into: &segmentBuffer
        ) 
        else {
          fatalError("Output buffer was not big enough, this should not happen")
        }
        segmentBufferCount = count
      } else {
        let size = source.remaining * _Normalization._maxNFCExpansionFactor
        if overflowBuffer == nil {
          overflowBuffer = Array(repeating: 0, count: size)
          normalizationBuffer = Array(repeating:0, count: size)
        }
        
        guard let count = normalizationBuffer!.withUnsafeMutableBufferPointer({
          (normalizationBufferPtr) -> Int? in
          guard let filled = source.tryFill(buffer: normalizationBufferPtr) 
          else {
            fatalError("Invariant broken, buffer should have space")
          }
          return overflowBuffer!.withUnsafeMutableBufferPointer { 
            (overflowBufferPtr) -> Int? in
            return _tryNormalize(
              UnsafeBufferPointer( rebasing: normalizationBufferPtr[..<filled]), 
              into: overflowBufferPtr
            )
          }
        }) else {
          fatalError("Invariant broken, overflow buffer should have space")
        }
        
        overflowBufferCount = count
      }
    } 
    
    //exactly one of the buffers should have code units for us to return
    _sanityCheck((segmentBufferCount == 0) 
              != ((overflowBuffer?.count ?? 0) == 0))
    
    if segmentBufferIndex < segmentBufferCount {
      let index = segmentBufferIndex
      segmentBufferIndex += 1
      return segmentBuffer[index]
    } else if overflowBufferIndex < overflowBufferCount {
      _sanityCheck(overflowBufferIndex < overflowBuffer!.count)
      let index = overflowBufferIndex
      overflowBufferIndex += 1
      return overflowBuffer![index]
    } else {
        return nil
    }
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
