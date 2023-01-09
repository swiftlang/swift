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
public struct MetatypeMetadata: PrivateLayout {
  typealias Layout = (
    base: Metadata.Layout,
    instanceMetadata: Metadata
  )
  
  let ptr: UnsafeRawPointer
  
  @usableFromInline
  init(_ ptr: UnsafeRawPointer) {
    self.ptr = ptr
  }
}

extension MetatypeMetadata {
  public var instanceMetadata: Metadata {
    layout.instanceMetadata
  }
}

//===----------------------------------------------------------------------===//
// Stdlib conformances
//===----------------------------------------------------------------------===//

extension MetatypeMetadata: Equatable {
  public static func ==(lhs: MetatypeMetadata, rhs: MetatypeMetadata) -> Bool {
    lhs.ptr == rhs.ptr
  }
}

extension MetatypeMetadata: Hashable {
  public func hash(into hasher: inout Hasher) {
    hasher.combine(ptr)
  }
}
