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
public struct ProtocolDescriptor: PublicLayout {
  public typealias Layout = (
    base: ContextDescriptor.Layout,
    name: RelativeDirectPointer<CChar>,
    numberOfRequirementsInSignature: UInt32,
    numberOfRequirements: UInt32,
    associatedTypeNames: RelativeDirectPointer<CChar>
  )
  
  public let ptr: UnsafeRawPointer
  
  @inlinable
  public init(_ ptr: UnsafeRawPointer) {
    self.ptr = ptr
  }
}

extension ProtocolDescriptor {
  @inlinable
  public var name: String {
    address(for: \.name).binaryString
  }
  
  @inlinable
  public var requirementSignature: IndirectBufferView<GenericSignature.RequirementDescriptor> {
    IndirectBufferView(
      start: trailing,
      count: Int(truncatingIfNeeded: layout.numberOfRequirementsInSignature)
    )
  }
  
  @inlinable
  public var requirements: IndirectBufferView<ProtocolRequirement> {
    let start = trailing +
      MemoryLayout<GenericSignature.RequirementDescriptor.Layout>.size *
      Int(truncatingIfNeeded: layout.numberOfRequirementsInSignature)
    
    return IndirectBufferView(
      start: start,
      count: Int(truncatingIfNeeded: layout.numberOfRequirements)
    )
  }
  
  @inlinable
  public var associatedTypeNames: String {
    address(for: \.associatedTypeNames).binaryString
  }
}

@frozen
public struct ProtocolRequirement: PublicLayout {
  public typealias Layout = (
    flags: UInt32,
    defaultImplementation: RelativeDirectPointer<()>
  )
  
  public let ptr: UnsafeRawPointer
  
  @inlinable
  public init(_ ptr: UnsafeRawPointer) {
    self.ptr = ptr
  }
}

//===----------------------------------------------------------------------===//
// Stdlib conformances
//===----------------------------------------------------------------------===//

extension ProtocolDescriptor: Equatable {
  @inlinable
  public static func ==(
    lhs: ProtocolDescriptor,
    rhs: ProtocolDescriptor
  ) -> Bool {
    lhs.ptr == rhs.ptr
  }
}

extension ProtocolDescriptor: Hashable {
  @inlinable
  public func hash(into hasher: inout Hasher) {
    hasher.combine(ptr)
  }
}
