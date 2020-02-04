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
guard let commandName = argv.popFirst() else {
  argFail("Command line arguments are completely empty!")
}

guard let pidStr = argv.popFirst() else {
  argFail("Specify a pid")
}

guard let pid = pidFromHint(pidStr) else {
  argFail("Cannot find pid/process \(pidStr)")
}

guard let inspector = Inspector(pid: pid) else {
  argFail("Failed to inspect pid \(pid)")
}

let addr = inspector.getAddr(symbolName: "__ZL12Conformances")
print(addr)

let reflectionContext = swift_reflection_createReflectionContextWithDataLayout(
  inspector.passContext(),
  Inspector.Callbacks.QueryDataLayout,
  Inspector.Callbacks.Free,
  Inspector.Callbacks.ReadBytes,
  Inspector.Callbacks.GetStringLength,
  Inspector.Callbacks.GetSymbolAddress)

swift_reflection_dumpConformances(reflectionContext)

swift_reflection_destroyReflectionContext(reflectionContext)
inspector.destroyContext()
