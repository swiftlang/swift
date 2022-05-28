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

internal struct DumpRawMetadata: ParsableCommand {
  static let configuration = CommandConfiguration(
    abstract: "Print the target's metadata allocations.")

  @OptionGroup()
  var options: UniversalOptions

  @OptionGroup()
  var backtraceOptions: BacktraceOptions

  func run() throws {
    try inspect(options: options) { process in
      let stacks: [swift_reflection_ptr_t:[swift_reflection_ptr_t]]? =
          backtraceOptions.style == nil
              ? nil
              : try process.context.allocationStacks

      try process.context.allocations.forEach { allocation in
        let name: String = process.context.name(allocation: allocation.tag) ??  "<unknown>"
        print("Metadata allocation at: \(hex: allocation.ptr) size: \(allocation.size) tag: \(allocation.tag) (\(name))")
        if let style = backtraceOptions.style {
          if let stack = stacks?[allocation.ptr] {
            print(backtrace(stack, style: style, process.symbolicate))
          } else {
            print("  No stack trace available")
          }
        }
      }
    }
  }
}
