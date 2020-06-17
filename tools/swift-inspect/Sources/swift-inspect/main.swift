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
    let tagNameC = swift_reflection_metadataAllocationTagName(context, allocation.tag)
    let tagName = tagNameC.map(String.init) ?? "<unknown>"
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
    ])
}

struct DumpConformanceCache: ParsableCommand {
  static let configuration = CommandConfiguration(
    abstract: "Print the contents of the target's protocol conformance cache.")

  @Argument(help: "The pid or partial name of the target process")
  var nameOrPid: String

  func run() throws {
    try withReflectionContext(nameOrPid: nameOrPid) { context, _ in
      try dumpConformanceCache(context: context)
    }
  }
}

struct DumpRawMetadata: ParsableCommand {
  static let configuration = CommandConfiguration(
    abstract: "Print the target's metadata allocations.")
  @Argument(help: "The pid or partial name of the target process")

  var nameOrPid: String

  @Flag(help: "Show the backtrace for each allocation")
  var backtrace: Bool

  @Flag(help: "Show a long-form backtrace for each allocation")
  var backtraceLong: Bool

  func run() throws {
    let style = backtrace ? Backtrace.Style.oneLine :
                backtraceLong ? Backtrace.Style.long :
                nil
    try withReflectionContext(nameOrPid: nameOrPid) {
      try dumpRawMetadata(context: $0, inspector: $1, backtraceStyle: style)
    }
  }
}

struct DumpGenericMetadata: ParsableCommand {
  static let configuration = CommandConfiguration(
    abstract: "Print the target's generic metadata allocations.")

  @Argument(help: "The pid or partial name of the target process")
  var nameOrPid: String

  @Flag(help: "Show the backtrace for each allocation")
  var backtrace: Bool

  @Flag(help: "Show a long-form backtrace for each allocation")
  var backtraceLong: Bool

  func run() throws {
    let style = backtrace ? Backtrace.Style.oneLine :
                backtraceLong ? Backtrace.Style.long :
                nil
    try withReflectionContext(nameOrPid: nameOrPid) {
      try dumpGenericMetadata(context: $0,
                              inspector: $1,
                              backtraceStyle: style)
    }
  }
}

SwiftInspect.main()
