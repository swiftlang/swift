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
  try context.iterateConformanceCache(call: { type, proto in
    let typeName = context.name(metadata: type) ?? "<unknown>"
    let protoName = context.name(proto: proto) ?? "<unknown>"
    print("Conformance: \(typeName): \(protoName)")
  })
}

func dumpMetadataAllocations(context: SwiftReflectionContextRef) throws {
  var allocations: [swift_metadata_allocation_t] = []
  var metadatas: [swift_reflection_ptr_t] = []
  try context.iterateMetadataAllocations(call: { allocation in
    allocations.append(allocation)
    print("Metadata allocation at: \(hex: allocation.Ptr) " + 
          "size: \(allocation.Size) tag: \(allocation.Tag)")
    let ptr = context.metadataPointer(allocation: allocation)
    if ptr != 0 {
      metadatas.append(ptr)
    }
  })

  allocations.sort(by: { $0.Ptr < $1.Ptr })
  for metadata in metadatas {
    let name = context.name(metadata: metadata) ?? "<unknown>"
    print("Metadata \(hex: metadata)")
    print("    Name: \(name)")

    if let allocation = allocations.last(where: { metadata >= $0.Ptr }) {
      let offset = metadata - allocation.Ptr
      print("    In allocation \(hex: allocation.Ptr) " +
            "size \(allocation.Size) at offset \(offset)")
    } else {
      print("    Not in any known metadata allocation. How strange.")
    }
  }
}

func makeReflectionContext(nameOrPid: String)
  -> (Inspector, SwiftReflectionContextRef) {
  guard let pid = pidFromHint(nameOrPid) else {
    argFail("Cannot find pid/process \(nameOrPid)")
  }

  guard let inspector = Inspector(pid: pid) else {
    argFail("Failed to inspect pid \(pid) (are you running as root?)")
  }

  guard let reflectionContext
    = swift_reflection_createReflectionContextWithDataLayout(
    inspector.passContext(),
    Inspector.Callbacks.QueryDataLayout,
    Inspector.Callbacks.Free,
    Inspector.Callbacks.ReadBytes,
    Inspector.Callbacks.GetStringLength,
    Inspector.Callbacks.GetSymbolAddress) else {
    argFail("Failed to create reflection context")
  }
  return (inspector, reflectionContext)
}

func withReflectionContext(
  nameOrPid: String,
  call: (SwiftReflectionContextRef) throws -> Void) throws {
  let (inspector, context) = makeReflectionContext(nameOrPid: nameOrPid)
  defer {
    swift_reflection_destroyReflectionContext(context)
    inspector.destroyContext()
  }
  try call(context)
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
