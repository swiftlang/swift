//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
//
//===----------------------------------------------------------------------===//

import Swift

@available(SwiftStdlib 5.9, *)
@frozen
public struct AnyExistentialContainer {
  public typealias Storage = (
    data: (Int, Int, Int),
    metadata: Metadata
  )
  
  @usableFromInline
  var storage: Storage
  
  @inlinable
  public var metadata: Metadata {
    storage.metadata
  }
  
  @inlinable
  public init(metadata: Metadata) {
    storage = (data: (0, 0, 0), metadata: metadata)
  }
  
  @inlinable
  public init(type: Any.Type) {
    storage = (data: (0, 0, 0), metadata: Metadata(type))
  }
}

@available(SwiftStdlib 5.9, *)
extension AnyExistentialContainer {
  @inlinable
  public mutating func allocateBox(_ body: (UnsafeMutableRawPointer) -> ()) {
    guard !metadata.vwt.flags.isValueInline else {
      withUnsafeMutablePointer(to: &self) {
        let raw = UnsafeMutableRawPointer($0)
        body(raw)
      }
      
      return
    }
    
    let pair = swift_allocBox(metadata)
    storage.data.0 = Int(bitPattern: pair.object.ptr)
    
    body(pair.buffer)
  }
  
  @inlinable
  public mutating func projectValue<T>(
    _ body: (UnsafeMutableRawPointer) throws -> T
  ) rethrows -> T {
    guard !metadata.vwt.flags.isValueInline else {
      return try withUnsafeMutablePointer(to: &self) {
        try body(UnsafeMutableRawPointer($0))
      }
    }
    
    let alignMask = UInt(metadata.vwt.flags.alignmentMask)
    let heapObjSize = UInt(MemoryLayout<Int>.size * 2)
    let byteOffset = (heapObjSize + alignMask) & ~alignMask
    
    return try withUnsafeMutablePointer(to: &self) {
      let raw = UnsafeMutableRawPointer($0)
      let heap = raw.loadUnaligned(as: UnsafeMutableRawPointer.self)
      
      return try body(heap + Int(byteOffset))
    }
  }
}

@available(SwiftStdlib 5.9, *)
@frozen
public struct ExistentialContainer {
  public typealias Storage = (
    base: AnyExistentialContainer,
    witnessTable: WitnessTable
  )
  
  @usableFromInline
  var storage: Storage
}

@available(SwiftStdlib 5.9, *)
@inlinable
public func container(for box: Any) -> AnyExistentialContainer {
  unsafeBitCast(box)
}
