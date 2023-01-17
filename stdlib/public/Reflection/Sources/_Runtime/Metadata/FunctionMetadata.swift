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
public struct FunctionMetadata: PrivateLayout {
  typealias Layout = (
    base: Metadata.Layout,
    flags: Flags,
    resultMetadata: Metadata
  )
  
  let ptr: UnsafeRawPointer
  
  @usableFromInline
  init(_ ptr: UnsafeRawPointer) {
    self.ptr = ptr
  }
}

@available(SwiftStdlib 9999, *)
extension FunctionMetadata {
  public var convention: Convention {
    layout.flags.convention
  }
  
  public var `throws`: Bool {
    layout.flags.throws
  }
  
  public var hasParameterFlags: Bool {
    layout.flags.hasParameterFlags
  }
  
  public var isEscaping: Bool {
    layout.flags.isEscaping
  }
  
  public var isDifferential: Bool {
    layout.flags.isDifferential
  }
  
  public var hasGlobalActor: Bool {
    layout.flags.hasGlobalActor
  }
  
  public var isAsync: Bool {
    layout.flags.isAsync
  }
  
  public var isSendable: Bool {
    layout.flags.isSendable
  }
}

@available(SwiftStdlib 9999, *)
extension FunctionMetadata {
  public var differentiableKind: DifferentiableKind {
    guard isDifferential else {
      return .nonDifferentiable
    }
    
    var address = parameterMetadata.endAddress
    
    if hasParameterFlags {
      address = parameterFlags.endAddress
    }
    
    return address.load(as: DifferentiableKind.self)
  }
  
  public var resultMetadata: Metadata {
    layout.resultMetadata
  }
  
  public var parameterMetadata: BufferView<Metadata> {
    BufferView(
      start: trailing,
      count: layout.flags.numberOfParameters
    )
  }
  
  public var parameterFlags: BufferView<UInt32> {
    BufferView(
      start: parameterMetadata.endAddress,
      count: layout.flags.numberOfParameters
    )
  }
}

//===----------------------------------------------------------------------===//
// Stdlib conformances
//===----------------------------------------------------------------------===//

@available(SwiftStdlib 9999, *)
extension FunctionMetadata: Equatable {
  public static func ==(lhs: FunctionMetadata, rhs: FunctionMetadata) -> Bool {
    lhs.ptr == rhs.ptr
  }
}

@available(SwiftStdlib 9999, *)
extension FunctionMetadata: Hashable {
  public func hash(into hasher: inout Hasher) {
    hasher.combine(ptr)
  }
}
