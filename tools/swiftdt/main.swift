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

guard let pid = pid_t(pidStr) else {
  argFail("Invalid pid \(pidStr)")
}

print(findTask(pid, tryForkCorpse: false))

