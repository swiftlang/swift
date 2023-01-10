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
public struct ContextDescriptor: PublicLayout {
  public typealias Layout = (
    flags: Flags,
    // This should really be RelativeDirectPointer<ContextDescriptor.Layout>,
    // but Swift doesn't allow circular references in tuples.
    parent: RelativeDirectPointer<ContextDescriptor>
  )
  
  public let ptr: UnsafeRawPointer
  
  @inlinable
  public init(_ ptr: UnsafeRawPointer) {
    self.ptr = ptr
  }
}

extension ContextDescriptor {
  @inlinable
  public var flags: Flags {
    layout.flags
  }
  
  @inlinable
  public var parent: ContextDescriptor? {
    guard !layout.parent.isNull else {
      return nil
    }
    
    return PtrAuth.signDescriptor(ContextDescriptor(address(for: \.parent)))
  }
  
  @inlinable
  var isType: Bool {
    switch flags.kind {
    case .class,
         .enum,
         .struct:
      return true
    default:
      return false
    }
  }
  
  @inlinable
  var sizeOfSelf: Int {
    switch flags.kind {
    case .module:
      return MemoryLayout<ModuleDescriptor.Layout>.size
    case .extension:
      return MemoryLayout<ExtensionDescriptor.Layout>.size
    case .anonymous:
      return MemoryLayout<AnonymousDescriptor.Layout>.size
    case .protocol:
      return MemoryLayout<ProtocolDescriptor.Layout>.size
    case .opaqueType:
      return MemoryLayout<OpaqueDescriptor.Layout>.size
    case .class:
      return MemoryLayout<ClassDescriptor.Layout>.size
    default:
      // case .enum, .struct
      //
      // These kinds share the same layout
      return MemoryLayout<StructDescriptor.Layout>.size
    }
  }
  
  @inlinable
  public var genericSignature: GenericSignature? {
    guard flags.isGeneric else {
      return nil
    }
    
    return getGenericSignature(at: ptr + sizeOfSelf)
  }
}

//===----------------------------------------------------------------------===//
// Stdlib conformances
//===----------------------------------------------------------------------===//

extension ContextDescriptor: Equatable {
  @inlinable
  public static func ==(
    lhs: ContextDescriptor,
    rhs: ContextDescriptor
  ) -> Bool {
    lhs.ptr == rhs.ptr
  }
}

extension ContextDescriptor: Hashable {
  @inlinable
  public func hash(into hasher: inout Hasher) {
    hasher.combine(ptr)
  }
}
