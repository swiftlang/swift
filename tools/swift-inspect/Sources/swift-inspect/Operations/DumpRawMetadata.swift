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

private struct AllocatorTagTotal: Encodable {
  let name: String
  let tag: Int
  var totalBytes: Int
}

private struct Summary: Encodable {
  let totalBytesAllocated: Int
  let allocatorTags: [AllocatorTagTotal]
}

private struct RawMetadataOutput: Encodable {
  let allocationList: [swift_metadata_allocation_t]?
  let summary: Summary?
}

internal struct DumpRawMetadata: ParsableCommand {
  static let configuration = CommandConfiguration(
    abstract: "Print the target's metadata allocations.")

  @OptionGroup()
  var options: UniversalOptions

  @OptionGroup()
  var backtraceOptions: BacktraceOptions
  
 @OptionGroup()
  var metadataOptions: MetadataOptions

  func run() throws {
    var allocatorTagTotals = [Int: AllocatorTagTotal]()
    var total: Int = 0
    var allocationList: [swift_metadata_allocation_t] = []

    try inspect(options: options) { process in
      let stacks: [swift_reflection_ptr_t:[swift_reflection_ptr_t]]? =
          backtraceOptions.style == nil
              ? nil
              : try process.context.allocationStacks

      try process.context.allocations.forEach { allocation in
        let name: String = process.context.name(allocation: allocation.tag) ??  "<unknown>"
        print("Metadata allocation at: \(hex: allocation.ptr) size: \(allocation.size) tag: \(allocation.tag) (\(name))")
        
        if metadataOptions.summary {
          if var allocatorTagTotal = allocatorTagTotals[Int(allocation.tag)] {
            allocatorTagTotal.totalBytes += allocation.size
            allocatorTagTotals[Int(allocation.tag)] = allocatorTagTotal
          } else {
            allocatorTagTotals[Int(allocation.tag)] = AllocatorTagTotal(name: name, tag: Int(allocation.tag), totalBytes: allocation.size)
          }
          
          total += allocation.size
        }
        allocationList.append(allocation)
        if let style = backtraceOptions.style {
          if let stack = stacks?[allocation.ptr] {
            print(backtrace(stack, style: style, process.symbolicate))
          } else {
            print("  No stack trace available")
          }
        }
      }
    }
    
    if metadataOptions.json {
      let jsonStruct: RawMetadataOutput
      let allocatorTagArray = Array(allocatorTagTotals.values).sorted(by: {$0.totalBytes > $1.totalBytes})
      
      if metadataOptions.summary {
        let summaryStruct = Summary(totalBytesAllocated: total, allocatorTags: allocatorTagArray)
        jsonStruct = RawMetadataOutput(allocationList: allocationList, summary: summaryStruct)
      } else {
        jsonStruct = RawMetadataOutput(allocationList: allocationList, summary: nil)
      }
      try dumpJson(of: jsonStruct, outputFile: metadataOptions.outputFile)
    } else if metadataOptions.summary {
      let allocatorTagArray = Array(allocatorTagTotals.values).sorted(by: {$0.totalBytes > $1.totalBytes})
      
      print("Metadata allocation summary:")
      for tag in allocatorTagArray {
        print("Tag: \(tag.tag) (\(tag.name)) Size: \(tag.totalBytes) bytes")
      }
      
      print("\nTotal bytes allocated: \(total)")
    }
  }
}
