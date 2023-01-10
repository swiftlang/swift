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
public struct ConformanceDescriptor: PublicLayout {
  public typealias Layout = (
    protocol: ProtocolDescriptor,
    typeRef: TypeReference.Layout,
    witnessTablePattern: RelativeDirectPointer<WitnessTable.Layout>,
    flags: Flags
  )
  
  public let ptr: UnsafeRawPointer
  
  @inlinable
  public init(_ ptr: UnsafeRawPointer) {
    self.ptr = ptr
  }
}

@available(SwiftStdlib 9999, *)
extension ConformanceDescriptor {
  @inlinable
  public var `protocol`: ProtocolDescriptor {
    PtrAuth.signDescriptor(layout.protocol)
  }
  
  @inlinable
  public var descriptor: ContextDescriptor {
    let typeRef = TypeReference(address(for: \.typeRef))
    return typeRef.descriptor(flags.typeReferenceKind)
  }
  
  @inlinable
  public var flags: Flags {
    layout.flags
  }
}
