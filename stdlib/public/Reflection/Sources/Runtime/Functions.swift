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
import CRuntime

@frozen
public struct BoxPair {
  public typealias Storage = (
    object: HeapObject,
    buffer: UnsafeMutableRawPointer
  )
  
  @usableFromInline
  let storage: Storage
  
  @inlinable
  public var object: HeapObject {
    storage.object
  }
  
  @inlinable
  public var buffer: UnsafeMutableRawPointer {
    storage.buffer
  }
}

@_silgen_name("swift_allocBox")
public func swift_allocBox(_: Metadata) -> BoxPair

@inlinable
public func swift_conformsToProtocol(
  _ type: Metadata,
  _ protocol: ProtocolDescriptor
) -> WitnessTable? {
  guard let wt = swift_conformsToProtocol(type.ptr, `protocol`.ptr) else {
    return nil
  }
  
  return WitnessTable(wt)
}

@inlinable
public func swift_projectBox(
  _ obj: HeapObject
) -> UnsafeMutableRawPointer {
  CRuntime.swift_projectBox(obj.ptr.mutable)
}
