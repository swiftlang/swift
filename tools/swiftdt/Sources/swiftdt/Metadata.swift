//
//  File.swift
//  
//
//  Created by Ben Cohen on 5/16/20.
//

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

struct Metadata {
  let ptr: swift_reflection_ptr_t
  let name: String
}
