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

#if os(iOS) || os(macOS) || os(tvOS) || os(watchOS)

internal struct DumpArrays: ParsableCommand {
  static let configuration = CommandConfiguration(
    abstract: "Print information about array objects in the target.")

  @OptionGroup()
  var options: UniversalOptions

  func run() throws {
    try inspect(process: options.nameOrPid) { process in
      print("Address", "Size", "Count", "Is Class", separator: "\t")
      process.iterateHeap { (allocation, size) in
        let metadata: swift_reflectioN_ptr_t =
            swift_reflection_metadataForObject(process.context, UInt(allocation))
        if metadata == 0 { return }
        guard process.context.isContiguousArray(metadata: metadata) else { return }

        let isClass = process.context.isArrayOfClass(metdata)
        let count = process.context.arrayCount(swift_reflection_ptr_t(allocation),
                                               process.ReadBytes)
        print("\(hex: swift_reflection_ptr_t(allocation))\t\(size)\t\(count.map(String.init) ?? "<unknown>")\t\(isClass)")
      }
    }
  }
}

#endif
