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

func dumpRawMetadata(context: SwiftReflectionContextRef) throws {
  for allocation in context.allocations {
    print("Metadata allocation at: \(hex: allocation.ptr) " +
          "size: \(allocation.size) tag: \(allocation.tag)")
  }
}

func dumpGenericMetadata(context: SwiftReflectionContextRef) throws {
  let allocations = context.allocations.sorted()
  let metadatas = allocations.findGenericMetadata(in: context)

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
  _ body: (SwiftReflectionContextRef) throws -> Void
) throws {
  let (inspector, context) = makeReflectionContext(nameOrPid: nameOrPid)
  defer {
    swift_reflection_destroyReflectionContext(context)
    inspector.destroyContext()
  }
  try body(context)
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
    try withReflectionContext(nameOrPid: nameOrPid) {
      try dumpConformanceCache(context: $0)
    }
  }
}

struct DumpRawMetadata: ParsableCommand {
  static let configuration = CommandConfiguration(
    abstract: "Print the target's metadata allocations.")
  @Argument(help: "The pid or partial name of the target process")

  var nameOrPid: String

  func run() throws {
    try withReflectionContext(nameOrPid: nameOrPid) {
      try dumpRawMetadata(context: $0)
    }
  }
}

struct DumpGenericMetadata: ParsableCommand {
  static let configuration = CommandConfiguration(
    abstract: "Print the target's metadata allocations.")
  @Argument(help: "The pid or partial name of the target process")

  var nameOrPid: String

  func run() throws {
    try withReflectionContext(nameOrPid: nameOrPid) {
      try dumpGenericMetadata(context: $0)
    }
  }
}

SwiftInspect.main()
