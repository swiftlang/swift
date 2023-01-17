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
public struct AnonymousDescriptor: PublicLayout {
  public typealias Layout = ContextDescriptor.Layout
  
  public let ptr: UnsafeRawPointer
  
  @inlinable
  public init(_ ptr: UnsafeRawPointer) {
    self.ptr = ptr
  }
}

@available(SwiftStdlib 9999, *)
extension AnonymousDescriptor {
  @inlinable
  public var base: ContextDescriptor {
    ContextDescriptor(ptr)
  }
  
  @inlinable
  public var anonymousFlags: Flags {
    Flags(value: layout.flags.kindSpecificFlags)
  }
}

@available(SwiftStdlib 9999, *)
extension AnonymousDescriptor {
  @inlinable
  public var genericSignature: GenericSignature? {
    guard base.flags.isGeneric else {
      return nil
    }
    
    return getGenericSignature(at: trailing)
  }
}

//===----------------------------------------------------------------------===//
// Stdlib conformances
//===----------------------------------------------------------------------===//

@available(SwiftStdlib 9999, *)
extension AnonymousDescriptor: Equatable {
  @inlinable
  public static func ==(
    lhs: AnonymousDescriptor,
    rhs: AnonymousDescriptor
  ) -> Bool {
    lhs.ptr == rhs.ptr
  }
}

@available(SwiftStdlib 9999, *)
extension AnonymousDescriptor: Hashable {
  @inlinable
  public func hash(into hasher: inout Hasher) {
    hasher.combine(ptr)
  }
}
