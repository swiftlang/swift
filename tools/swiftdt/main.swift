import Foundation
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


var argv = ArraySlice(CommandLine.arguments)
guard let executableName = argv.popFirst() else {
  argFail("Command line arguments are completely empty!")
}

struct Command {
  var name: String
  var help: String
  var call: (inout ArraySlice<String>) -> Void
  
  init(name: String, help: String,
       call: @escaping (inout ArraySlice<String>) -> Void) {
    self.name = name
    self.help = help
    self.call = call
  }
  
  init(name: String, help: String,
       call: @escaping (SwiftReflectionContextRef) -> Void) {
    self.name = name
    self.help = help
    self.call = { withReflectionContext(args: &$0, call: call) }
  }
}

let commands = [
  Command(
    name: "dump-conformance-cache",
    help: "Print the contents of the target's protocol conformance cache.",
    call: dumpConformanceCache),
  Command(
    name: "dump-metadata-allocations",
    help: "Print the target's metadata allocations.",
    call: dumpMetadataAllocations),
  Command(
    name: "help",
    help: "Print this help.",
    call: printUsage),
]

func dumpConformanceCache(context: SwiftReflectionContextRef) {
  let success = context.iterateConformanceCache(call: { type, proto in
    let typeName = context.name(metadata: type) ?? "<unknown>"
    let protoName = context.name(proto: proto) ?? "<unknown>"
    print("Conformance: \(typeName): \(protoName)")
  })
  if !success {
    print("Error!")
  }
}

func dumpMetadataAllocations(context: SwiftReflectionContextRef) {
  var allocations: [swift_metadata_allocation_t] = []
  var metadatas: [swift_reflection_ptr_t] = []
  let success = context.iterateMetadataAllocations(call: { allocation in
    allocations.append(allocation)
    print("Metadata allocation at: \(hex: allocation.Ptr) " + 
          "size: \(allocation.Size) tag: \(allocation.Tag)")
    let ptr = context.metadataPointer(allocation: allocation)
    if ptr != 0 {
      metadatas.append(ptr)
    }
  })
  if !success {
    print("Error!")
    return
  }

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

func printUsage(args: inout ArraySlice<String>) {
  print("Usage: \(executableName) <command>", to: &Std.err)
  print("", to: &Std.err)
  print("Available commands:", to: &Std.err)
  
  let maxWidth = commands.map({ $0.name.count }).max() ?? 0
  for command in commands {
    let paddedName = command.name.padding(toLength: maxWidth,
                                          withPad: " ", startingAt: 0)
    print("  \(paddedName) - \(command.help)", to: &Std.err)
  }
}

func makeReflectionContext(args: inout ArraySlice<String>)
  -> (Inspector, SwiftReflectionContextRef) {
  guard let pidStr = args.popFirst() else {
    argFail("Must specify a pid or process name")
  }

  guard let pid = pidFromHint(pidStr) else {
    argFail("Cannot find pid/process \(pidStr)")
  }

  guard let inspector = Inspector(pid: pid) else {
    argFail("Failed to inspect pid \(pid)")
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

func withReflectionContext(args: inout ArraySlice<String>,
                           call: (SwiftReflectionContextRef) -> Void) {
  let (inspector, context) = makeReflectionContext(args: &args)
  call(context)
  swift_reflection_destroyReflectionContext(context)
  inspector.destroyContext()
}

let commandName = argv.popFirst()
for command in commands {
  if command.name == commandName {
    command.call(&argv)
    exit(0)
  }
}

if let commandName = commandName {
  print("error: \(executableName): unknown command \(commandName)", to: &Std.err)
} else {
  print("error: \(executableName): missing command", to: &Std.err)
}
print("", to: &Std.err)
printUsage(args: &argv)
exit(EX_USAGE)
