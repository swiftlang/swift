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

private struct Metadata {
  let ptr: swift_reflection_ptr_t
  var allocation: swift_metadata_allocation_t?

  let name: String
  let isArrayOfClass: Bool

  var offset: Int? { allocation.map { Int(self.ptr - $0.ptr) } }
}

internal struct DumpGenericMetadata: ParsableCommand {
  static let configuration = CommandConfiguration(
    abstract: "Print the target's generic metadata allocations.")

  @OptionGroup()
  var options: UniversalOptions

  @OptionGroup()
  var backtraceOptions: BacktraceOptions

  func run() throws {
    try inspect(options: options) { process in
      let allocations: [swift_metadata_allocation_t] =
          try process.context.allocations.sorted()

      let generics: [Metadata] = allocations.compactMap { allocation in
        let pointer = swift_reflection_allocationMetadataPointer(process.context, allocation)
        if pointer == 0 { return nil }

        return Metadata(ptr: pointer,
                        allocation: allocations.last(where: { pointer >= $0.ptr }),
                        name: process.context.name(type: pointer) ??
                        "<unknown>",
                        isArrayOfClass: process.context.isArrayOfClass(pointer))
      }

      let stacks: [swift_reflection_ptr_t:[swift_reflection_ptr_t]]? =
          backtraceOptions.style == nil
              ? nil
              : try process.context.allocationStacks

      print("Address", "Allocation", "Size", "Offset", "isArrayOfClass", "Name", separator: "\t")
      generics.forEach {
        print("\(hex: $0.ptr)", terminator: "\t")
        if let allocation = $0.allocation, let offset = $0.offset {
          print("\(hex: allocation.ptr)\t\(allocation.size)\t\(offset)", terminator: "\t")
        } else {
          print("???\t??\t???", terminator: "\t")
        }
        print($0.isArrayOfClass, terminator: "\t")
        print($0.name)
        if let style = backtraceOptions.style, let allocation = $0.allocation {
          if let stack = stacks?[allocation.ptr] {
            print(backtrace(stack, style: style, process.symbolicate))
          } else {
            print("  No stacktrace available")
          }
        }
      }
    }
  }
}
