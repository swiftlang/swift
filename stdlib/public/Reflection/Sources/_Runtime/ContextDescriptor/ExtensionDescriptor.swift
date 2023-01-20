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
public struct ExtensionDescriptor: PublicLayout {
  public typealias Layout = (
    base: ContextDescriptor.Layout,
    extendedContext: RelativeDirectPointer<CChar>
  )
  
  public let ptr: UnsafeRawPointer
  
  @inlinable
  public init(_ ptr: UnsafeRawPointer) {
    self.ptr = ptr
  }
}

@available(SwiftStdlib 5.9, *)
extension ExtensionDescriptor {
  @inlinable
  public var extendedContext: MangledTypeReference {
    MangledTypeReference(address(for: \.extendedContext))
  }
}

@available(SwiftStdlib 5.9, *)
extension ExtensionDescriptor {
  @inlinable
  public var genericSignature: GenericSignature? {
    getGenericSignature(at: trailing)
  }
}

//===----------------------------------------------------------------------===//
// Stdlib conformances
//===----------------------------------------------------------------------===//

@available(SwiftStdlib 5.9, *)
extension ExtensionDescriptor: Equatable {
  public static func ==(
    lhs: ExtensionDescriptor,
    rhs: ExtensionDescriptor
  ) -> Bool {
    lhs.ptr == rhs.ptr
  }
}

@available(SwiftStdlib 5.9, *)
extension ExtensionDescriptor: Hashable {
  @inlinable
  public func hash(into hasher: inout Hasher) {
    hasher.combine(ptr)
  }
}
