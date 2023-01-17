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
public struct TypeReference: PublicLayout {
  public typealias Layout = Int32
  
  public let ptr: UnsafeRawPointer
  
  @inlinable
  public init(_ ptr: UnsafeRawPointer) {
    self.ptr = ptr
  }
}

@available(SwiftStdlib 5.9, *)
extension TypeReference {
  @inlinable
  public func descriptor(_ kind: Kind) -> ContextDescriptor {
    switch kind {
    case .directDescriptor:
      let address = ptr.relativeDirectAddress(as: ContextDescriptor.self)
      return ContextDescriptor(address)
      
    case .indirectDescriptor:
      let address = ptr.relativeIndirectAddress(as: ContextDescriptor.self)
      let descriptor = ContextDescriptor(address)
      return PtrAuth.signDescriptor(descriptor)
      
    default:
      fatalError("Unknown descriptor typeref kind")
    }
  }
}
