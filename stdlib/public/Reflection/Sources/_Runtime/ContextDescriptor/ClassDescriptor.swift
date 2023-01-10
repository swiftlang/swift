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
public struct ClassDescriptor: PublicLayout {
  public typealias Layout = (
    base: TypeDescriptor.Layout,
    superclass: RelativeDirectPointer<CChar>,
    negativeSizeOrResilientBounds: UInt32,
    positiveSizeOrExtraFlags: UInt32,
    numberOfImmediateMembers: UInt32,
    numberOfFields: UInt32,
    fieldOffsetVectorOffset: UInt32
  )
  
  public let ptr: UnsafeRawPointer
  
  @inlinable
  public init(_ ptr: UnsafeRawPointer) {
    self.ptr = ptr
  }
}

extension ClassDescriptor {
  @inlinable
  public var base: TypeDescriptor {
    TypeDescriptor(ptr)
  }
  
  @inlinable
  public var numberOfFields: Int {
    Int(truncatingIfNeeded: layout.numberOfFields)
  }
}

extension ClassDescriptor {
  @inlinable
  public var genericSignature: GenericSignature? {
    guard base.base.flags.isGeneric else {
      return nil
    }
    
    return getGenericSignature(at: trailing + MemoryLayout<Int32>.size &* 2)
  }
}

extension ClassDescriptor {
  @inlinable
  var genericArgumentOffset: Int {
    if base.flags.classHasResilientSuperclass {
      return resilientImmediateMembersOffset / MemoryLayout<Int>.size
    } else {
      return nonResilientImmediateMembersOffset
    }
  }
  
  @inlinable
  var resilientImmediateMembersOffset: Int {
    typealias StoredClassBounds = (Int, UInt32, UInt32)
    
    let storedBounds = address(for: \.negativeSizeOrResilientBounds)
      .relativeDirectAddress(as: StoredClassBounds.self)
    
    // This integer is stored and accessed as an atomic, however according to
    // the comments in 'swift/include/swift/ABI/Metadata.h' for
    // 'TargetStoredClassMetadataBounds', by the time we access this it will
    // have already been initialized way before for us. Thus, it is safe to
    // access this value non-atomically.
    return storedBounds.load(as: Int.self)
  }
  
  @inlinable
  var nonResilientImmediateMembersOffset: Int {
    if base.flags.classAreImmediateMembersNegative {
      return -Int(truncatingIfNeeded: layout.negativeSizeOrResilientBounds)
    } else {
      return Int(
        truncatingIfNeeded:
          layout.positiveSizeOrExtraFlags &- layout.numberOfImmediateMembers
      )
    }
  }
}

//===----------------------------------------------------------------------===//
// Stdlib conformances
//===----------------------------------------------------------------------===//

extension ClassDescriptor: Equatable {
  @inlinable
  public static func ==(lhs: ClassDescriptor, rhs: ClassDescriptor) -> Bool {
    lhs.ptr == rhs.ptr
  }
}

extension ClassDescriptor: Hashable {
  @inlinable
  public func hash(into hasher: inout Hasher) {
    hasher.combine(ptr)
  }
}
