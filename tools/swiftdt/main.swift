import Foundation


func argFail(_ message: String) -> Never {
  print(message, to: &Std.err)
  exit(EX_USAGE)
}

func machErrStr(_ kr: kern_return_t) -> String {
    let errStr = String(cString: mach_error_string(kr))
    let errHex = String(kr, radix: 16)
    return "\(errStr) (0x\(errHex))"
}

func findTask(_ pid: pid_t, tryForkCorpse: Bool) -> task_t {
  var task = task_t()
  var kr = task_for_pid(mach_task_self_, pid, &task)
  if kr != KERN_SUCCESS {
    print("Unable to get task for pid \(pid): \(machErrStr(kr))", to: &Std.err)
    return 0
  }

  if !tryForkCorpse {
    return task
  }
  
  var corpse = task_t()
  kr = task_generate_corpse(task, &corpse)
  if kr == KERN_SUCCESS {
    task_resume(task)
    mach_port_deallocate(mach_task_self_, task)
    return corpse
  } else {
    print("warning: unable to generate corpse for pid \(pid): \(machErrStr(kr))", to: &Std.err)
    return task
  }
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

let task = findTask(pid, tryForkCorpse: false)
let symbolicator = CSSymbolicatorCreateWithTask(task)

let swiftCore = CSSymbolicatorGetSymbolOwnerWithNameAtTime(
  symbolicator, "libswiftCore.dylib", kCSNow)

_ = task_start_peeking(task)

let conformances = CSSymbolOwnerGetSymbolWithMangledName(swiftCore, "__ZL12Conformances")
let range = CSSymbolGetRange(conformances)
if let p = task_peek(task, mach_vm_address_t(range.location), mach_vm_size_t(range.length)) {
  let d = NSData(bytes: p, length: range.length)
  print(d)
}

CSSymbolOwnerForeachSymbol(swiftCore, {
 if !CSSymbolIsFunction($0),
    let name = CSSymbolGetName($0),
    let mangledName = CSSymbolGetMangledName($0) {
   print("\(name) -- \(mangledName)")
 }
})
