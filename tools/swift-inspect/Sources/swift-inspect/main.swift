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


@main
internal struct SwiftInspect: ParsableCommand {
  // DumpArrays and DumpConcurrency cannot be reliably be ported outside of
  // Darwin due to the need to iterate the heap.
#if os(iOS) || os(macOS) || os(tvOS) || os(watchOS)
  static let subcommands: [ParsableCommand.Type] = [
    DumpConformanceCache.self,
    DumpRawMetadata.self,
    DumpGenericMetadata.self,
    DumpCacheNodes.self,
    DumpArrays.self,
    DumpConcurrency.self,
  ]
#else
  static let subcommands: [ParsableCommand.Type] = [
    DumpConformanceCache.self,
    DumpRawMetadata.self,
    DumpGenericMetadata.self,
    DumpCacheNodes.self,
    DumpArrays.self,
  ]
#endif

  static let configuration = CommandConfiguration(
    abstract: "Swift runtime debug tool",
    subcommands: subcommands)
}
