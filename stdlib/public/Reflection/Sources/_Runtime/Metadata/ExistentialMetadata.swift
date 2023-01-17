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
public struct ExistentialMetadata: PrivateLayout {
  typealias Layout = (
    base: Metadata.Layout,
    flags: Flags,
    numberOfProtocols: UInt32
  )
  
  let ptr: UnsafeRawPointer
  
  @usableFromInline
  init(_ ptr: UnsafeRawPointer) {
    self.ptr = ptr
  }
}

@available(SwiftStdlib 5.9, *)
extension ExistentialMetadata {
  public var protocols: BufferView<ProtocolDescriptor> {
    var start = trailing
    
    if layout.flags.hasSuperclassConstraint {
      start = start.offset(of: 1)
    }
    
    return BufferView(
      start: UnsafePointer(start._rawValue),
      count: Int(truncatingIfNeeded: layout.numberOfProtocols)
    )
  }
}

//===----------------------------------------------------------------------===//
// Stdlib conformances
//===----------------------------------------------------------------------===//

@available(SwiftStdlib 5.9, *)
extension ExistentialMetadata: Equatable {
  public static func ==(
    lhs: ExistentialMetadata,
    rhs: ExistentialMetadata
  ) -> Bool {
    lhs.ptr == rhs.ptr
  }
}

@available(SwiftStdlib 5.9, *)
extension ExistentialMetadata: Hashable {
  public func hash(into hasher: inout Hasher) {
    hasher.combine(ptr)
  }
}
