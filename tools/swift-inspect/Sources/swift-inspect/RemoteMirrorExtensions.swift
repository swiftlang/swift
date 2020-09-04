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

extension SwiftReflectionContextRef {
  struct Error: Swift.Error, CustomStringConvertible {
    var description: String

    init(cString: UnsafePointer<CChar>) {
      description = String(cString: cString)
    }
  }

  func name(metadata: swift_reflection_ptr_t) -> String? {
    let tr = swift_reflection_typeRefForMetadata(self, UInt(metadata));
    guard tr != 0 else { return nil }

    guard let cstr = swift_reflection_copyDemangledNameForTypeRef(self, tr)
      else { return nil }
    defer { free(cstr) }
    return String(cString: cstr)
  }

  func name(proto: swift_reflection_ptr_t) -> String? {
    guard let cstr = swift_reflection_copyDemangledNameForProtocolDescriptor(
      self, proto) else { return nil }
    defer { free(cstr) }
    return String(cString: cstr)
  }

  func iterateConformanceCache(
    _ body: (swift_reflection_ptr_t, swift_reflection_ptr_t) -> Void
  ) throws {
    var body = body
    let errStr = swift_reflection_iterateConformanceCache(self, {
      let callPtr = $2!.bindMemory(to:
        ((swift_reflection_ptr_t, swift_reflection_ptr_t) -> Void).self,
        capacity: 1)
      callPtr.pointee($0, $1)
    }, &body)
    try throwError(str: errStr)
  }

  func iterateMetadataAllocations(
    _ body: (swift_metadata_allocation_t) -> Void
  ) throws {
    var body = body
    let errStr = swift_reflection_iterateMetadataAllocations(self, {
      let callPtr = $1!.bindMemory(to:
        ((swift_metadata_allocation_t) -> Void).self, capacity: 1)
      callPtr.pointee($0)
    }, &body)
    try throwError(str: errStr)
  }

  func iterateMetadataAllocationBacktraces(
    _ body: (swift_reflection_ptr_t, Int, UnsafePointer<swift_reflection_ptr_t>)
            -> Void
  ) throws {
    var body = body
    let errStr = swift_reflection_iterateMetadataAllocationBacktraces(self, {
      let callPtr = $3!.bindMemory(to:
        ((swift_reflection_ptr_t, Int, UnsafePointer<swift_reflection_ptr_t>)
         -> Void).self, capacity: 1)
      callPtr.pointee($0, $1, $2!)
    }, &body)
    try throwError(str: errStr)
  }

  func metadataPointer(
    allocation: swift_metadata_allocation_t
  ) -> swift_reflection_ptr_t {
    swift_reflection_allocationMetadataPointer(self, allocation)
  }

  func metadataTagName(_ tag: swift_metadata_allocation_tag_t) -> String? {
    swift_reflection_metadataAllocationTagName(self, tag)
      .map(String.init)
  }

  func metadataAllocationCacheNode(
    _ allocation: swift_metadata_allocation_t
  ) -> swift_metadata_cache_node_t? {
    var node = swift_metadata_cache_node_t();
    let success = swift_reflection_metadataAllocationCacheNode(
      self, allocation, &node)
    if success == 0 {
      return nil
    }
    return node
  }

  private func throwError(str: UnsafePointer<CChar>?) throws {
    if let str = str {
      throw Error(cString: str)
    }
  }
  
  var allocations: [Allocation] {
    var allocations: [Allocation] = []
    try! iterateMetadataAllocations { allocation_t in
      allocations.append(.init(allocation_t: allocation_t))
    }
    return allocations
  }

  var allocationBacktraces: [swift_reflection_ptr_t: Backtrace] {
    var backtraces: [swift_reflection_ptr_t: Backtrace] = [:]
    try! iterateMetadataAllocationBacktraces { allocation, count, ptrs in
      let array = Array(UnsafeBufferPointer(start: ptrs, count: count))
      backtraces[allocation] = Backtrace(ptrs: array)
    }
    return backtraces
  }
}
