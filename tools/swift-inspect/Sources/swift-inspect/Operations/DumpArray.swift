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

#if !os(Linux)

import ArgumentParser
import SwiftRemoteMirror

internal struct DumpArrays: ParsableCommand {
  static let configuration = CommandConfiguration(
    abstract: "Print information about array objects in the target.")

  @OptionGroup()
  var options: UniversalOptions

  func run() throws {
    try inspect(options: options) { process in
      print("Address", "Size", "Count", "Is Class", separator: "\t")
      process.iterateHeap { (allocation, size) in
        let metadata: UInt =
            swift_reflection_metadataForObject(process.context, UInt(allocation))
        if metadata == 0 { return }

        guard process.context.isContiguousArray(swift_reflection_ptr_t(metadata)) else {
          return
        }

        let ReadBytes: RemoteProcess.ReadBytesFunction =
            type(of: process).ReadBytes
        let this = process.toOpaqueRef()

        let isClass = process.context.isArrayOfClass(swift_reflection_ptr_t(metadata))
        let count = process.context.arrayCount(swift_reflection_ptr_t(allocation),
                                               { ReadBytes(this, $0, UInt64($1), nil) })
        print("\(hex: swift_reflection_ptr_t(allocation))\t\(size)\t\(count.map(String.init) ?? "<unknown>")\t\(isClass)")
      }
    }
  }
}

#endif
