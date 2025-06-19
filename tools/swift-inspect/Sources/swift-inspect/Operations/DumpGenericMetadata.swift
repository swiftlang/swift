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

private struct ProcessMetadata: Encodable {
  var name: String
  var pid: ProcessIdentifier
  var metadata: [Metadata]
}

private struct MetadataSummary: Encodable {
  var totalSize: Int
  var processes: Set<String>
}

internal struct Output: TextOutputStream {
  let fileHandle: FileHandle
  init(_ outputFile: String? = nil) throws {
    if let outputFile {
      if FileManager().createFile(atPath: outputFile, contents: nil) {
        self.fileHandle = FileHandle(forWritingAtPath: outputFile)!
      } else {
        print("Unable to create file \(outputFile)", to: &Std.err)
        exit(1)
      }
    } else {
      self.fileHandle = FileHandle.standardOutput
    }
  }

  mutating func write(_ string: String) {
    if let encodedString = string.data(using: .utf8) {
      fileHandle.write(encodedString)
    }
  }
}

internal func dumpJson(of: (any Encodable), outputFile: String?) throws {
  let encoder = JSONEncoder()
  encoder.outputFormatting = [.prettyPrinted, .sortedKeys]
  let data = try encoder.encode(of)
  let jsonOutput = String(data: data, encoding: .utf8)!
  if let outputFile = outputFile {
    try jsonOutput.write(toFile: outputFile, atomically: true, encoding: .utf8)
  } else {
    print(jsonOutput)
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
  var metadataOptions: MetadataOptions
  
  @Flag(help: "Show allocations in mangled form")
  var mangled: Bool = false

  func run() throws {
    disableStdErrBuffer()
    var metadataSummary = [String: MetadataSummary]()
    var allProcesses = [ProcessMetadata]()
    try inspect(options: options) { process in
      let allocations: [swift_metadata_allocation_t] =
          try process.context.allocations.sorted()

      let stacks: [swift_reflection_ptr_t:[swift_reflection_ptr_t]] =
          backtraceOptions.style == nil
          ? [swift_reflection_ptr_t:[swift_reflection_ptr_t]]()
          : try process.context.allocationStacks

      let generics: [Metadata] = allocations.compactMap { allocation -> Metadata? in
        let pointer = swift_reflection_allocationMetadataPointer(process.context, allocation)
        if pointer == 0 { return nil }
        let allocation = allocations.last(where: { pointer >= $0.ptr && pointer < $0.ptr + swift_reflection_ptr_t($0.size) })
        let garbage = (allocation == nil && swift_reflection_ownsAddressStrict(process.context, UInt(pointer)) == 0)
        var currentBacktrace: String?
        if let style = backtraceOptions.style, let allocation, let stack = stacks[allocation.ptr] {
          currentBacktrace = backtrace(stack, style: style, process.symbolicate)
        }

        return Metadata(ptr: pointer,
                        allocation: allocation,
                        name: process.context.name(type: pointer, mangled: mangled) ?? "<unknown>",
                        isArrayOfClass: process.context.isArrayOfClass(pointer),
                        garbage: garbage,
                        backtrace: currentBacktrace)
      } // generics

      // Update summary
      generics.forEach { metadata in
        if let allocation = metadata.allocation {
          let name = metadata.name
          if metadataSummary.keys.contains(name) {
            metadataSummary[name]!.totalSize += allocation.size
            metadataSummary[name]!.processes.insert(process.processName)
          } else {
            metadataSummary[name] = MetadataSummary(totalSize: allocation.size,
                                                    processes: Set([process.processName]))
            }
        }
      }

      if metadataOptions.json {
        let processMetadata = ProcessMetadata(name: process.processName,
                                              pid: process.processIdentifier as! ProcessIdentifier,
                                              metadata: generics)
        allProcesses.append(processMetadata)
      } else if !metadataOptions.summary {
        try dumpText(process: process, generics: generics)
        }
    } // inspect

    if metadataOptions.json {
      if metadataOptions.summary {
        try dumpJson(of: metadataSummary, outputFile: metadataOptions.outputFile)
      } else {
        try dumpJson(of: allProcesses, outputFile: metadataOptions.outputFile)
        }
    } else if metadataOptions.summary {
      try dumpTextSummary(of: metadataSummary)
    }
  }

  private func dumpText(process: any RemoteProcess, generics: [Metadata]) throws {
    var erroneousMetadata: [(ptr: swift_reflection_ptr_t, name: String)] = []
    var output = try Output(metadataOptions.outputFile)
    print("\(process.processName)(\(process.processIdentifier)):\n", to: &output)
    print("Address", "Allocation", "Size", "Offset", "isArrayOfClass", "Name", separator: "\t", to: &output)
    generics.forEach {
      print("\(hex: $0.ptr)", terminator: "\t", to: &output)
      if let allocation = $0.allocation, let offset = $0.offset {
        print("\(hex: allocation.ptr)\t\(allocation.size)\t\(offset)", terminator: "\t", to: &output)
      } else {
        if $0.garbage {
          erroneousMetadata.append((ptr: $0.ptr, name: $0.name))
        }
        print("???\t??\t???", terminator: "\t", to: &output)
      }
      print($0.isArrayOfClass, terminator: "\t", to: &output)
      print($0.name, to: &output)
      if let _ = backtraceOptions.style, let _ = $0.allocation {
        print($0.backtrace ?? "  No stacktrace available", to: &output)
      }
    }

    if erroneousMetadata.count > 0 {
      print("Warning: The following metadata was not found in any DATA or AUTH segments, may be garbage.", to: &output)
      erroneousMetadata.forEach {
        print("\(hex: $0.ptr)\t\($0.name)", to: &output)
      }
    }
    print("", to: &output)
  }

  private func dumpTextSummary(of: [String: MetadataSummary]) throws {
    var output = try Output(metadataOptions.outputFile)
    print("Size", "Owners", "Name", separator: "\t", to: &output)
    var totalSize = 0
    var unknownSize = 0
    of.sorted { first, second in
      first.value.processes.count > second.value.processes.count
    }
    .forEach { summary in
      totalSize += summary.value.totalSize
      if summary.key == "<unknown>" {
        unknownSize += summary.value.totalSize
      }
      print(summary.value.totalSize, summary.value.processes.count, summary.key,
        separator: "\t", to: &output)
    }
    print("\nTotal size:\t\(totalSize / 1024) KiB", to: &output)
    print("Unknown size:\t\(unknownSize / 1024) KiB", to: &output)
  }
}
