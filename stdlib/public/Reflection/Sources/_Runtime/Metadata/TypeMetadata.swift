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
public struct TypeMetadata: PublicLayout {
  public typealias Layout = Int
  
  public let ptr: UnsafeRawPointer
  
  @inlinable
  public init(_ ptr: UnsafeRawPointer) {
    self.ptr = ptr
  }
}

@available(SwiftStdlib 5.9, *)
extension TypeMetadata {
  @inlinable
  public var descriptor: TypeDescriptor {
    var address: UnsafeRawPointer

    switch metadata.kind {
    case .struct:
      address = ptr + MemoryLayout<StructMetadata.Layout>.offset(of: \.descriptor)!
    case .enum,
        .optional:
      address = ptr + MemoryLayout<EnumMetadata.Layout>.offset(of: \.descriptor)!
    default:
      address = ptr + MemoryLayout<ClassMetadata.Layout>.offset(of: \.descriptor)!
    }

    address = address.unprotectedLoad(as: UnsafeRawPointer.self)

    return PtrAuth.signDescriptor(TypeDescriptor(address))
  }

  @inlinable
  public var genericArguments: UnsafeRawPointer {
    switch metadata.kind {
    case .struct:
      return ptr + MemoryLayout<StructMetadata.Layout>.size
    case .enum,
         .optional:
      return ptr + MemoryLayout<EnumMetadata.Layout>.size
    default:
      var genericArgOffset = `class`.descriptor.genericArgumentOffset
      genericArgOffset = genericArgOffset &* MemoryLayout<Int>.size
      
      return ptr + genericArgOffset
    }
  }
}

@available(SwiftStdlib 5.9, *)
extension TypeMetadata {
  @inlinable
  public var metadata: Metadata {
    Metadata(ptr)
  }

  @inlinable
  public var `class`: ClassMetadata {
    ClassMetadata(ptr)
  }

  @inlinable
  public var `enum`: EnumMetadata {
    EnumMetadata(ptr)
  }

  @inlinable
  public var `struct`: StructMetadata {
    StructMetadata(ptr)
  }
}

@available(SwiftStdlib 5.9, *)
extension TypeMetadata {
  @inlinable
  public func resolve(_ typeRef: MangledTypeReference) -> Any.Type? {
    // Fast paths for known stdlib types
    if let ss = typeRef.standardSubstitution {
      return ss
    }

    return _resolve(typeRef)
  }

  @usableFromInline
  func _resolve(_ typeRef: MangledTypeReference) -> Any.Type? {
    typeCache.getOrInsert(typeRef, from: self)
  }
}

//===----------------------------------------------------------------------===//
// Stdlib conformances
//===----------------------------------------------------------------------===//

@available(SwiftStdlib 5.9, *)
extension TypeMetadata: Equatable {
  @inlinable
  public static func ==(lhs: TypeMetadata, rhs: TypeMetadata) -> Bool {
    lhs.ptr == rhs.ptr
  }
}

@available(SwiftStdlib 5.9, *)
extension TypeMetadata: Hashable {
  @inlinable
  public func hash(into hasher: inout Hasher) {
    hasher.combine(ptr)
  }
}
