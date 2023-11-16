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
import Foundation

private struct Metadata: Encodable {
  let ptr: swift_reflection_ptr_t
  var allocation: swift_metadata_allocation_t?
  let name: String
  let isArrayOfClass: Bool
  var garbage: Bool = false
  var offset: Int? { allocation.map { Int(self.ptr - $0.ptr) } }
  var backtrace: String?

  enum CodingKeys: String, CodingKey {
    case ptr = "address"
    case allocation
    case name
    case isArrayOfClass
    case garbage
    case offset
    case backtrace
  }

  func encode(to encoder: Encoder) throws {
    var container = encoder.container(keyedBy: CodingKeys.self)
    try container.encode(ptr, forKey: .ptr)
    try container.encode(name, forKey: .name)
    if isArrayOfClass {
      try container.encode(isArrayOfClass, forKey: .isArrayOfClass)
    }
    if garbage {
      try container.encode(garbage, forKey: .garbage)
    }
    if let offset {
      try container.encode(offset, forKey: .offset)
    }
    if let backtrace {
      try container.encode(backtrace, forKey: .backtrace)
    }
    if let allocation {
      try container.encode(allocation, forKey: .allocation)
    }
  }
}

internal struct DumpGenericMetadata: ParsableCommand {
  static let configuration = CommandConfiguration(
    abstract: "Print the target's generic metadata allocations.")

  @OptionGroup()
  var options: UniversalOptions

  @OptionGroup()
  var backtraceOptions: BacktraceOptions

  @OptionGroup()
  var genericMetadataOptions: GenericMetadataOptions

  func run() throws {
    try inspect(options: options) { process in
      let allocations: [swift_metadata_allocation_t] =
          try process.context.allocations.sorted()

      let stacks: [swift_reflection_ptr_t:[swift_reflection_ptr_t]] =
          backtraceOptions.style == nil
          ? [swift_reflection_ptr_t:[swift_reflection_ptr_t]]()
          : try process.context.allocationStacks

      let generics: [Metadata] = allocations.compactMap { allocation in
        let pointer = swift_reflection_allocationMetadataPointer(process.context, allocation)
        if pointer == 0 { return nil }
        let allocation = allocations.last(where: { pointer >= $0.ptr && pointer < $0.ptr + UInt64($0.size) })
        let garbage = (allocation == nil && swift_reflection_ownsAddressStrict(process.context, UInt(pointer)) == 0)
        var currentBacktrace: String?
        if let style = backtraceOptions.style, let allocation, let stack = stacks[allocation.ptr] {
          currentBacktrace = backtrace(stack, style: style, process.symbolicate)
        }

        return Metadata(ptr: pointer,
                        allocation: allocation,
                        name: process.context.name(type: pointer, mangled: genericMetadataOptions.mangled) ?? "<unknown>",
                        isArrayOfClass: process.context.isArrayOfClass(pointer),
                        garbage: garbage,
                        backtrace: currentBacktrace)
      }

      if let outputFile = genericMetadataOptions.outputJson {
        try dumpToJson(process: process, generics: generics,
                       outputPath: outputFile)
      } else {
        try dumpToStdout(process: process, generics: generics)
      }
    }
  }

  private func dumpToStdout(process: any RemoteProcess, generics: [Metadata]) throws {
    var errorneousMetadata: [(ptr: swift_reflection_ptr_t, name: String)] = []

    print("Address", "Allocation", "Size", "Offset", "isArrayOfClass", "Name", separator: "\t")
    generics.forEach {
      print("\(hex: $0.ptr)", terminator: "\t")
      if let allocation = $0.allocation, let offset = $0.offset {
        print("\(hex: allocation.ptr)\t\(allocation.size)\t\(offset)", terminator: "\t")
      } else {
        if $0.garbage {
          errorneousMetadata.append((ptr: $0.ptr, name: $0.name))
        }
        print("???\t??\t???", terminator: "\t")
      }
      print($0.isArrayOfClass, terminator: "\t")
      print($0.name)
      if let _ = backtraceOptions.style, let _ = $0.allocation {
        print($0.backtrace ?? "  No stacktrace available")
      }
    }

    if errorneousMetadata.count > 0 {
      print("Error: The following metadata was not found in any DATA or AUTH segments, may be garbage.")
      errorneousMetadata.forEach {
        print("\(hex: $0.ptr)\t\($0.name)")
      }
    }
  }

  private func dumpToJson(process: any RemoteProcess,
                          generics: [Metadata],
                          outputPath: String) throws {
    struct AllMetadataEntries: Encodable {
      var metadata: [Metadata]
    }
    let allMetadataEntries = AllMetadataEntries(metadata: generics)
    let encoder = JSONEncoder()
    encoder.outputFormatting = .prettyPrinted
    let data = try encoder.encode(allMetadataEntries)
    try String(data: data, encoding: .utf8)!
      .write(toFile: outputPath, atomically: true, encoding: .utf8)
  }

}
