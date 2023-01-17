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

@available(SwiftStdlib 9999, *)
@frozen
public struct TypeDescriptor: PublicLayout {
  public typealias Layout = (
    base: ContextDescriptor.Layout,
    name: RelativeDirectPointer<CChar>,
    accessor: RelativeDirectPointer<Metadata.AccessFunction>,
    fields: RelativeDirectPointer<FieldDescriptor.Layout>
  )
  
  public let ptr: UnsafeRawPointer
  
  @inlinable
  public init(_ ptr: UnsafeRawPointer) {
    self.ptr = ptr
  }
}

@available(SwiftStdlib 9999, *)
extension TypeDescriptor {
  @inlinable
  public var base: ContextDescriptor {
    ContextDescriptor(ptr)
  }
  
  @inlinable
  public var flags: Flags {
    Flags(value: base.flags.kindSpecificFlags)
  }
  
  @inlinable
  public var name: String {
    address(for: \.name).binaryString
  }
  
  @inlinable
  public var accessor: Metadata.AccessFunction {
    Metadata.AccessFunction(address(for: \.accessor))
  }
  
  @inlinable
  public var fields: FieldDescriptor {
    FieldDescriptor(address(for: \.fields))
  }
}

@available(SwiftStdlib 9999, *)
extension TypeDescriptor {
  @inlinable
  var sizeOfSelf: Int {
    switch base.flags.kind {
    case .struct:
      return MemoryLayout<StructDescriptor.Layout>.size
    case .enum:
      return MemoryLayout<EnumDescriptor.Layout>.size
    default:
      return MemoryLayout<ClassDescriptor.Layout>.size
    }
  }
  
  @inlinable
  public var genericSignature: GenericSignature? {
    guard base.flags.isGeneric else {
      return nil
    }
    
    var address = ptr + sizeOfSelf
    address += MemoryLayout<Int32>.size * 2
    
    return getGenericSignature(at: address)
  }
}

//===----------------------------------------------------------------------===//
// Stdlib conformances
//===----------------------------------------------------------------------===//

@available(SwiftStdlib 9999, *)
extension TypeDescriptor: Equatable {
  @inlinable
  public static func ==(_ lhs: TypeDescriptor, _ rhs: TypeDescriptor) -> Bool {
    lhs.ptr == rhs.ptr
  }
}

@available(SwiftStdlib 9999, *)
extension TypeDescriptor: Hashable {
  @inlinable
  public func hash(into hasher: inout Hasher) {
    hasher.combine(ptr)
  }
}
