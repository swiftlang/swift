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

func dumpMetadataAllocations(context: SwiftReflectionContextRef) throws {
  var allocations = context.allocations
  for allocation in allocations {
    print("Metadata allocation at: \(hex: allocation.ptr) " +
          "size: \(allocation.size) tag: \(allocation.tag)")
  }
  
  allocations.sort()
  let metadatas = allocations.compactMap { $0.metadata(in: context) }

  for metadata in metadatas {
    print("Metadata \(hex: metadata.ptr)")
    print("    Name: \(metadata.name)")

    if let allocation = allocations.last(where: { metadata.ptr >= $0.ptr }) {
      let offset = metadata.ptr - allocation.ptr
      print("    In allocation \(hex: allocation.ptr) " +
            "size \(allocation.size) at offset \(offset)")
    } else {
      print("    Not in any known metadata allocation. How strange.")
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
  _ body: (SwiftReflectionContextRef) throws -> Void
) throws {
  let (inspector, context) = makeReflectionContext(nameOrPid: nameOrPid)
  defer {
    swift_reflection_destroyReflectionContext(context)
    inspector.destroyContext()
  }
  try body(context)
}

struct Swiftdt: ParsableCommand {
  static let configuration = CommandConfiguration(
    abstract: "Swift runtime debug tool",
    subcommands: [
      DumpConformanceCache.self,
      DumpMetadataAllocations.self,
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

struct DumpMetadataAllocations: ParsableCommand {
  static let configuration = CommandConfiguration(
    abstract: "Print the target's metadata allocations.")
  @Argument(help: "The pid or partial name of the target process")

  var nameOrPid: String

  func run() throws {
    try withReflectionContext(nameOrPid: nameOrPid) {
      try dumpMetadataAllocations(context: $0)
    }
  }
}

Swiftdt.main()
