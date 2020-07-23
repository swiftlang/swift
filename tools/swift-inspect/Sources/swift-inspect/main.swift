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


func argFail(_ message: String) -> Never {
  print(message, to: &Std.err)
  exit(EX_USAGE)
}

func machErrStr(_ kr: kern_return_t) -> String {
  let errStr = String(cString: mach_error_string(kr))
  let errHex = String(kr, radix: 16)
  return "\(errStr) (0x\(errHex))"
}

func dumpConformanceCache(context: SwiftReflectionContextRef) throws {
  try context.iterateConformanceCache { type, proto in
    let typeName = context.name(metadata: type) ?? "<unknown>"
    let protoName = context.name(proto: proto) ?? "<unknown>"
    print("Conformance: \(typeName): \(protoName)")
  }
}

func dumpRawMetadata(
  context: SwiftReflectionContextRef,
  inspector: Inspector,
  backtraceStyle: Backtrace.Style?
) throws {
  let backtraces = backtraceStyle != nil ? context.allocationBacktraces : [:]
  for allocation in context.allocations {
    let tagName = context.metadataTagName(allocation.tag) ?? "<unknown>"
    print("Metadata allocation at: \(hex: allocation.ptr) " +
          "size: \(allocation.size) tag: \(allocation.tag) (\(tagName))")
    printBacktrace(style: backtraceStyle, for: allocation.ptr, in: backtraces, inspector: inspector)
  }
}

func dumpGenericMetadata(
  context: SwiftReflectionContextRef,
  inspector: Inspector,
  backtraceStyle: Backtrace.Style?
) throws {
  let allocations = context.allocations.sorted()
  let metadatas = allocations.findGenericMetadata(in: context)
  let backtraces = backtraceStyle != nil ? context.allocationBacktraces : [:]

  print("Address","Allocation","Size","Offset","Name", separator: "\t")
  for metadata in metadatas {
    print("\(hex: metadata.ptr)", terminator: "\t")

    if let allocation = metadata.allocation, let offset = metadata.offset {
      print("\(hex: allocation.ptr)\t\(allocation.size)\t\(offset)",
            terminator: "\t")
    } else {
      print("???\t???\t???", terminator: "\t")
    }
    print(metadata.name)
    if let allocation = metadata.allocation {
      printBacktrace(style: backtraceStyle, for: allocation.ptr, in: backtraces, inspector: inspector)
    }
  }
}

func dumpMetadataCacheNodes(
  context: SwiftReflectionContextRef,
  inspector: Inspector
) throws {
  print("Address","Tag","Tag Name","Size","Left","Right", separator: "\t")
  for allocation in context.allocations {
    guard let node = context.metadataAllocationCacheNode(allocation.allocation_t) else {
      continue
    }

    let tagName = context.metadataTagName(allocation.tag) ?? "<unknown>"
    print("\(hex: allocation.ptr)\t\(allocation.tag)\t\(tagName)\t" +
          "\(allocation.size)\t\(hex: node.Left)\t\(hex: node.Right)")
  }
}

func printBacktrace(
  style: Backtrace.Style?,
  for ptr: swift_reflection_ptr_t,
  in backtraces: [swift_reflection_ptr_t: Backtrace],
  inspector: Inspector
) {
  if let style = style {
    if let backtrace = backtraces[ptr] {
      print(backtrace.symbolicated(style: style, inspector: inspector))
    } else {
      print("Unknown backtrace.")
    }
  }
}

func makeReflectionContext(
  nameOrPid: String
) -> (Inspector, SwiftReflectionContextRef) {
  guard let pid = pidFromHint(nameOrPid) else {
    argFail("Cannot find pid/process \(nameOrPid)")
  }

  guard let inspector = Inspector(pid: pid) else {
    argFail("Failed to inspect pid \(pid) (are you running as root?)")
  }

  guard let reflectionContext = swift_reflection_createReflectionContextWithDataLayout(
    inspector.passContext(),
    Inspector.Callbacks.QueryDataLayout,
    Inspector.Callbacks.Free,
    Inspector.Callbacks.ReadBytes,
    Inspector.Callbacks.GetStringLength,
    Inspector.Callbacks.GetSymbolAddress
  ) else {
    argFail("Failed to create reflection context")
  }

  return (inspector, reflectionContext)
}

func withReflectionContext(
  nameOrPid: String,
  _ body: (SwiftReflectionContextRef, Inspector) throws -> Void
) throws {
  let (inspector, context) = makeReflectionContext(nameOrPid: nameOrPid)
  defer {
    swift_reflection_destroyReflectionContext(context)
    inspector.destroyContext()
  }
  try body(context, inspector)
}

struct SwiftInspect: ParsableCommand {
  static let configuration = CommandConfiguration(
    abstract: "Swift runtime debug tool",
    subcommands: [
      DumpConformanceCache.self,
      DumpRawMetadata.self,
      DumpGenericMetadata.self,
      DumpCacheNodes.self,
    ])
}

struct UniversalOptions: ParsableArguments {
  @Argument(help: "The pid or partial name of the target process")
  var nameOrPid: String
}

struct BacktraceOptions: ParsableArguments {
  @Flag(help: "Show the backtrace for each allocation")
  var backtrace: Bool

  @Flag(help: "Show a long-form backtrace for each allocation")
  var backtraceLong: Bool

  var style: Backtrace.Style? {
    backtrace ? .oneLine :
    backtraceLong ? .long :
    nil
  }
}

struct DumpConformanceCache: ParsableCommand {
  static let configuration = CommandConfiguration(
    abstract: "Print the contents of the target's protocol conformance cache.")

  @OptionGroup()
  var options: UniversalOptions

  func run() throws {
    try withReflectionContext(nameOrPid: options.nameOrPid) { context, _ in
      try dumpConformanceCache(context: context)
    }
  }
}

struct DumpRawMetadata: ParsableCommand {
  static let configuration = CommandConfiguration(
    abstract: "Print the target's metadata allocations.")

  @OptionGroup()
  var universalOptions: UniversalOptions

  @OptionGroup()
  var backtraceOptions: BacktraceOptions

  func run() throws {
    try withReflectionContext(nameOrPid: universalOptions.nameOrPid) {
      try dumpRawMetadata(context: $0,
                          inspector: $1,
                          backtraceStyle: backtraceOptions.style)
    }
  }
}

struct DumpGenericMetadata: ParsableCommand {
  static let configuration = CommandConfiguration(
    abstract: "Print the target's generic metadata allocations.")

  @OptionGroup()
  var universalOptions: UniversalOptions

  @OptionGroup()
  var backtraceOptions: BacktraceOptions

  func run() throws {
    try withReflectionContext(nameOrPid: universalOptions.nameOrPid) {
      try dumpGenericMetadata(context: $0,
                              inspector: $1,
                              backtraceStyle: backtraceOptions.style)
    }
  }
}

struct DumpCacheNodes: ParsableCommand {
  static let configuration = CommandConfiguration(
    abstract: "Print the target's metadata cache nodes.")

  @OptionGroup()
  var options: UniversalOptions

  func run() throws {
    try withReflectionContext(nameOrPid: options.nameOrPid) {
      try dumpMetadataCacheNodes(context: $0,
                                 inspector: $1)
    }
  }
}

SwiftInspect.main()
