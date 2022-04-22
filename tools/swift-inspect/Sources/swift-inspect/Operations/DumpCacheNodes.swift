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

import ArgumentParser
import SwiftRemoteMirror

internal struct DumpCacheNodes: ParsableCommand {
  static let configuration = CommandConfiguration(
    abstract: "Print the target's metadata cache nodes.")

  @OptionGroup()
  var options: UniversalOptions

  func run() throws {
    try inspect(options: options) { process in
      print("Address", "Tag", "Tag Name", "Size", "Left", "Right", separator: "\t")
      try process.context.allocations.forEach {
        var node: swift_metadata_cache_node_t = swift_metadata_cache_node_t()
        if swift_reflection_metadataAllocationCacheNode(process.context, $0, &node) == 0 {
          return
        }

        let name: String = process.context.name(allocation: $0.tag) ??  "<unknown>"
        print("\(hex: $0.ptr)\t\($0.tag)\t\(name)\t\($0.size)\t\(hex: node.Left)\t\(hex: node.Right)")
      }
    }
  }
}
