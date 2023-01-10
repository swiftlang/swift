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

@frozen
public struct EnumDescriptor: PublicLayout {
  public typealias Layout = (
    base: TypeDescriptor.Layout,
    numberOfPayloadCasesAndPayloadSizeOffset: UInt32,
    numberOfEmptyCases: UInt32
  )
  
  public let ptr: UnsafeRawPointer
  
  @inlinable
  public init(_ ptr: UnsafeRawPointer) {
    self.ptr = ptr
  }
}

extension EnumDescriptor {
  @inlinable
  public var numberOfPayloadCases: Int {
    let and = layout.numberOfPayloadCasesAndPayloadSizeOffset & 0xFFFFFF
    return Int(truncatingIfNeeded: and)
  }
  
  @inlinable
  public var numberOfEmptyCases: Int {
    Int(truncatingIfNeeded: layout.numberOfEmptyCases)
  }
  
  @inlinable
  public var numberOfCases: Int {
    numberOfPayloadCases + numberOfEmptyCases
  }
}

extension EnumDescriptor {
  @inlinable
  public var type: TypeDescriptor {
    TypeDescriptor(ptr)
  }
  
  @inlinable
  public var genericSignature: GenericSignature? {
    guard type.base.flags.isGeneric else {
      return nil
    }
    
    return getGenericSignature(at: trailing + MemoryLayout<Int32>.size * 2)
  }
}

//===----------------------------------------------------------------------===//
// Stdlib conformances
//===----------------------------------------------------------------------===//

extension EnumDescriptor: Equatable {
  @inlinable
  public static func ==(lhs: EnumDescriptor, rhs: EnumDescriptor) -> Bool {
    lhs.ptr == rhs.ptr
  }
}

extension EnumDescriptor: Hashable {
  @inlinable
  public func hash(into hasher: inout Hasher) {
    hasher.combine(ptr)
  }
}
