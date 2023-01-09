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
public struct ModuleDescriptor: PublicLayout {
  public typealias Layout = (
    base: ContextDescriptor.Layout,
    name: RelativeDirectPointer<CChar>
  )
  
  public let ptr: UnsafeRawPointer
  
  @inlinable
  public init(_ ptr: UnsafeRawPointer) {
    self.ptr = ptr
  }
}

extension ModuleDescriptor {
  @inlinable
  public var name: String {
    address(for: \.name).binaryString
  }
}

//===----------------------------------------------------------------------===//
// Stdlib conformances
//===----------------------------------------------------------------------===//

extension ModuleDescriptor: Equatable {
  @inlinable
  public static func ==(lhs: ModuleDescriptor, rhs: ModuleDescriptor) -> Bool {
    lhs.ptr == rhs.ptr
  }
}

extension ModuleDescriptor: Hashable {
  @inlinable
  public func hash(into hasher: inout Hasher) {
    hasher.combine(ptr)
  }
}
