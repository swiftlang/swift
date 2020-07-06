//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import SwiftRemoteMirror

struct Allocation {
  let allocation_t: swift_metadata_allocation_t
  
  var tag: swift_metadata_allocation_tag_t { allocation_t.Tag }
  var ptr: swift_reflection_ptr_t { allocation_t.Ptr }
  var size: Int { Int(allocation_t.Size) }
}

extension Allocation: Comparable {
  static func == (lhs: Self, rhs: Self) -> Bool {
    lhs.ptr == rhs.ptr
  }

  static func < (lhs: Self, rhs: Self) -> Bool {
    lhs.ptr < rhs.ptr
  }
}

extension Allocation {
  func metadata(in context: SwiftReflectionContextRef) -> Metadata? {
    let ptr = context.metadataPointer(allocation: allocation_t)
    if ptr != 0 {
      let name = context.name(metadata: ptr) ?? "<unknown>"
      return .init(ptr: ptr, name: name)
    } else {
      return nil
    }
  }
}

extension BidirectionalCollection where Element == Allocation {
  func findGenericMetadata(in context: SwiftReflectionContextRef) -> [Metadata] {
    var metadatas = self.compactMap { $0.metadata(in: context) }
    for i in metadatas.indices {
      let metadata = metadatas[i]
      if let allocation = self.last(where: { metadata.ptr >= $0.ptr }) {
        metadatas[i].allocation = allocation
      }
    }
    return metadatas
  }
}

struct Metadata {
  let ptr: swift_reflection_ptr_t
  var allocation: Allocation? = nil
  let name: String
  
  var offset: Int? { allocation.map { Int(self.ptr - $0.ptr) } }
}
