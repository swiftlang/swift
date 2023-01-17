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
public struct GenericSignature {
  public let header: Header
  public let parameters: BufferView<ParameterDescriptor>
  public let requirements: IndirectBufferView<RequirementDescriptor>
  
  @inlinable
  init(
    _ header: Header,
    _ parameters: BufferView<ParameterDescriptor>,
    _ requirements: IndirectBufferView<RequirementDescriptor>
  ) {
    self.header = header
    self.parameters = parameters
    self.requirements = requirements
  }
}

@available(SwiftStdlib 5.9, *)
extension GenericSignature {
  @frozen
  public struct Header {
    @usableFromInline
    typealias Storage = (
      numberOfParameters: UInt16,
      numberOfRequirements: UInt16,
      numberOfKeyArguments: UInt16,
      numberOfExtraArguments: UInt16
    )
    
    @usableFromInline
    let storage: Storage
    
    @inlinable
    public var numberOfParameters: Int {
      Int(truncatingIfNeeded: storage.numberOfParameters)
    }
    
    public var numberOfRequirements: Int {
      Int(truncatingIfNeeded: storage.numberOfRequirements)
    }
  }
}

@available(SwiftStdlib 5.9, *)
extension GenericSignature {
  @frozen
  public struct RequirementDescriptor: PublicLayout {
    public typealias Layout = (
      flags: Flags,
      parameter: RelativeDirectPointer<CChar>,
      // This field is a union which represents the type of requirement
      // that this parameter is constrained to. It is represented by the following:
      // 1. Same type requirement (RelativeDirectPointer<CChar>)
      // 2. Protocol requirement (RelativeIndirectablePointerIntPair<ProtocolDescriptor, Bool>)
      // 3. Conformance requirement (RelativeIndirectablePointer<ProtocolConformanceRecord>)
      // 4. Layout requirement (LayoutKind)
      requirement: Int32
    )
    
    public let ptr: UnsafeRawPointer
    
    @inlinable
    public init(_ ptr: UnsafeRawPointer) {
      self.ptr = ptr
    }
  }
}

@available(SwiftStdlib 5.9, *)
@inlinable
func getGenericSignature(at address: UnsafeRawPointer) -> GenericSignature {
  var address = address
  
  let header = address.loadUnaligned(as: GenericSignature.Header.self)
  address += MemoryLayout<GenericSignature.Header>.size
  
  let parameters = BufferView<GenericSignature.ParameterDescriptor>(
    start: address,
    count: header.numberOfParameters
  )
  // This accounts for padding
  address += (-header.numberOfParameters & 0x3) + header.numberOfParameters
  
  let requirements = IndirectBufferView<GenericSignature.RequirementDescriptor>(
    start: address,
    count: header.numberOfRequirements
  )
  
  return GenericSignature(header, parameters, requirements)
}
