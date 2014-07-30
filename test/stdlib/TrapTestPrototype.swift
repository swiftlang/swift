// RUN: mkdir -p %t
// RUN: %target-build-swift %s -o %t/a.out
// RUN: %target-run %t/a.out | FileCheck %s

import StdlibUnittest
import Foundation

@asmname("swift_stdlib_installTrapInterceptor")
func _stdlib_installTrapInterceptor()

//
// Functions missing in `Darwin` module.
//
func _WSTATUS(status: CInt) -> CInt {
  return status & 0x7f
}

var _WSTOPPED: CInt {
  return 0x7f
}

func WIFEXITED(status: CInt) -> Bool {
  return _WSTATUS(status) == 0
}

func WIFSIGNALED(status: CInt) -> Bool {
  return _WSTATUS(status) != _WSTOPPED && _WSTATUS(status) != 0
}

func WEXITSTATUS(status: CInt) -> CInt {
  return (status >> 8) & 0xff
}

func WTERMSIG(status: CInt) -> CInt {
  return _WSTATUS(status)
}

@asmname("_NSGetEnviron")
func _NSGetEnviron() -> UnsafeMutablePointer<UnsafeMutablePointer<UnsafeMutablePointer<CChar>>>

/// Compute the prefix sum of `seq`.
func scan<
  S : SequenceType, U
>(seq: S, initial: U, combine: (U, S.Generator.Element) -> U) -> [U] {
  var result = [U]()
  result.reserveCapacity(underestimateCount(seq))
  var runningResult = initial
  for element in seq {
    runningResult = combine(runningResult, element)
    result.append(runningResult)
  }
  return result
}

func withArrayOfCStrings<R>(
  args: [String], body: (Array<UnsafeMutablePointer<CChar>>) -> R) -> R {

  let argsLengths = Array(map(args) { countElements($0.utf8) + 1 })
  let argsOffsets = [ 0 ] + scan(argsLengths, 0, +)
  let argsBufferSize = argsOffsets.last!

  var argsBuffer = [UInt8]()
  argsBuffer.reserveCapacity(argsBufferSize)
  for arg in args {
    argsBuffer += arg.utf8
    argsBuffer += [ 0 ]
  }

  return argsBuffer.withUnsafeBufferPointer {
    (argsBuffer) in
    let ptr = UnsafeMutablePointer<CChar>(argsBuffer.baseAddress)
    var cStrings = Array(map(argsOffsets) { ptr + $0 })
    cStrings.append(nil)
    return body(cStrings)
  }
}

/// Calls POSIX `pipe()`.
func posixPipe() -> (readFD: CInt, writeFD: CInt) {
  var fds: [CInt] = [ -1, -1 ]
  var _: Void = fds.withUnsafeMutableBufferPointer {
    (fds) in
    let ptr = fds.baseAddress
    if pipe(ptr) != 0 {
      trap("pipe() failed")
    }
  }
  return (fds[0], fds[1])
}

/// Start the same executable as a child process, redirecting its stdout and
/// stderr.
func spawnChild(args: [String])
  -> (pid: pid_t, stdoutFD: CInt, stderrFD: CInt) {
  var fileActions = posix_spawn_file_actions_t()
  if posix_spawn_file_actions_init(&fileActions) != 0 {
    trap("posix_spawn_file_actions_init() failed")
  }

  let childStdout = posixPipe()
  // Close the read end of the pipe on the child side.
  if posix_spawn_file_actions_addclose(
    &fileActions, childStdout.readFD) != 0 {
    trap("posix_spawn_file_actions_addclose() failed")
  }

  // Remap child's stdout.
  if posix_spawn_file_actions_adddup2(
    &fileActions, childStdout.writeFD, STDOUT_FILENO) != 0 {
    trap("posix_spawn_file_actions_adddup2() failed")
  }

  let childStderr = posixPipe()
  // Close the read end of the pipe on the child side.
  if posix_spawn_file_actions_addclose(
    &fileActions, childStderr.readFD) != 0 {
    trap("posix_spawn_file_actions_addclose() failed")
  }

  // Remap child's stderr.
  if posix_spawn_file_actions_adddup2(
    &fileActions, childStderr.writeFD, STDERR_FILENO) != 0 {
    trap("posix_spawn_file_actions_adddup2() failed")
  }

  var pid: pid_t = -1
  let spawnResult = withArrayOfCStrings([ Process.arguments[0] ] + args) {
    posix_spawn(
      &pid, Process.arguments[0], &fileActions, nil, $0, _NSGetEnviron().memory)
  }
  if spawnResult != 0 {
    println(String.fromCString(strerror(spawnResult)))
    trap("posix_spawn() failed")
  }

  if posix_spawn_file_actions_destroy(&fileActions) != 0 {
    trap("posix_spawn_file_actions_destroy() failed")
  }

  // Close the write end of the pipe on the parent side.
  if close(childStdout.writeFD) != 0 {
    trap("close() failed")
  }

  // Close the write end of the pipe on the parent side.
  if close(childStderr.writeFD) != 0 {
    trap("close() failed")
  }

  return (pid, childStdout.readFD, childStderr.readFD)
}

func readAll(fd: CInt) -> String {
  var buffer = [UInt8](count: 1024, repeatedValue: 0)
  var usedBytes = 0
  while true {
    let readResult: ssize_t = buffer.withUnsafeMutableBufferPointer {
      (buffer) in
      let ptr = UnsafeMutablePointer<Void>(buffer.baseAddress + usedBytes)
      return read(fd, ptr, size_t(buffer.count - usedBytes))
    }
    if readResult > 0 {
      usedBytes += readResult
      continue
    }
    if readResult == 0 {
      break
    }
    trap("read() failed")
  }
  return String._fromCodeUnitSequenceWithRepair(
    UTF8.self, input: buffer[0..<usedBytes]).0
}

func signalToString(signal: Int) -> String {
  switch CInt(signal) {
  case SIGILL:  return "SIGILL"
  case SIGTRAP: return "SIGTRAP"
  case SIGABRT: return "SIGABRT"
  case SIGFPE:  return "SIGFPE"
  case SIGBUS:  return "SIGBUS"
  case SIGSEGV: return "SIGSEGV"
  case SIGSYS:  return "SIGSYS"
  default:      return "SIG???? (\(signal))"
  }
}

enum ProcessTerminationStatus : Printable {
  case Exit(Int)
  case Signal(Int)

  var description: String {
    switch self {
    case .Exit(var status):
      return "Exit(\(status))"
    case .Signal(var signal):
      return "Signal(\(signalToString(signal)))"
    }
  }

  var isSwiftTrap: Bool {
    switch self {
    case .Exit(var status):
      return false
    case .Signal(var signal):
      return CInt(signal) == SIGILL || CInt(signal) == SIGTRAP
    }
  }
}

func posixWaitpid(pid: pid_t) -> ProcessTerminationStatus {
  var status: CInt = 0
  if waitpid(pid, &status, 0) < 0 {
    trap("waitpid() failed")
  }
  if (WIFEXITED(status)) {
    return .Exit(Int(WEXITSTATUS(status)))
  }
  if (WIFSIGNALED(status)) {
    return .Signal(Int(WTERMSIG(status)))
  }
  trap("did not understand what happened to child process")
}

func runChild(args: [String])
  -> (stdout: String, stderr: String, status: ProcessTerminationStatus) {
  let (pid, stdoutFD, stderrFD) = spawnChild(args)

  // FIXME: reading stdout and stderr sequentially can block.  Should use
  // select().  This is not so simple to implement because of:
  // <rdar://problem/17828358> Darwin module is missing fd_set-related macros
  let stdout = readAll(stdoutFD)
  let stderr = readAll(stderrFD)

  if close(stdoutFD) != 0 {
    trap("close() failed")
  }
  if close(stderrFD) != 0 {
    trap("close() failed")
  }
  let status = posixWaitpid(pid)
  return (stdout, stderr, status)
}

/* This does not work on modern OS X.

func mach_task_self() -> mach_port_t {
  return mach_task_self_
}

var EXC_MASK_CRASH: exception_mask_t { return 1 << 10 }
var EXC_MASK_ALL: exception_mask_t { return 0x1bfe }

func disableCrashReporting() {
  var count: mach_msg_type_number_t = 0
  var originalMasks = [exception_mask_t](count: Int(EXC_TYPES_COUNT), repeatedValue: 0)
  var originalPorts = [exception_port_t](count: Int(EXC_TYPES_COUNT), repeatedValue: 0)
  var originalBehaviors = [exception_behavior_t](count: Int(EXC_TYPES_COUNT), repeatedValue: 0)
  var originalFlavors = [thread_state_flavor_t](count: Int(EXC_TYPES_COUNT), repeatedValue: 0)
  let err = task_get_exception_ports(
    mach_task_self(), EXC_MASK_CRASH, &originalMasks, &count, &originalPorts,
    &originalBehaviors, &originalFlavors)

  if (err == KERN_SUCCESS) {
    // replace each with MACH_PORT_NULL.
    for i in 0..<Int(count) {
      let err = task_set_exception_ports(
        mach_task_self(), originalMasks[i], mach_port_t(MACH_PORT_NULL),
        originalBehaviors[i], originalFlavors[i])
    }
  }
}
disableCrashReporting()
*/

if Process.arguments.count == 1 {
  // Parent

  if true {
    let (stdout, stderr, status) = runChild([ "testTrap" ])
    println("stdout: [\(stdout)]")
    println("stderr: [\(stderr)]")
    println("status: \(status)")
    println("isSwiftTrap: \(status.isSwiftTrap)")
// CHECK: stdout: [child started
// CHECK: stderr: [fatal error: this should crash: file
// CHECK: status: Signal({{SIGILL|SIGTRAP}})
// CHECK: isSwiftTrap: true
  }

  if true {
    let (stdout, stderr, status) = runChild([ "testInterceptedTrap" ])
    println("stdout: [\(stdout)]")
    println("stderr: [\(stderr)]")
    println("status: \(status)")
    println("isSwiftTrap: \(status.isSwiftTrap)")
// CHECK: stdout: [child started
// CHECK: stderr: [fatal error: this should crash: file
// CHECK: CRASHED: {{SIGILL|SIGTRAP}}
// CHECK: ]
// CHECK: status: Exit(0)
// CHECK: isSwiftTrap: false
  }

  if true {
    let (stdout, stderr, status) = runChild([ "testNoTrap" ])
    println("stdout: [\(stdout)]")
    println("stderr: [\(stderr)]")
    println("status: \(status)")
    println("isSwiftTrap: \(status.isSwiftTrap)")
// CHECK: stdout: [child started
// CHECK: no crash
// CHECK: ]
// CHECK: stderr: []
// CHECK: status: Exit(5)
// CHECK: isSwiftTrap: false
  }

} else {
  // Child

  // Interpret the command line arguments.
  var arg = Process.arguments[1]
  if arg == "testTrap" {
    println("child started")
    fflush(stdout)
    fflush(stderr)
    trap("this should crash")
  }

  if arg == "testInterceptedTrap" {
    println("child started")
    _stdlib_installTrapInterceptor()
    fflush(stdout)
    fflush(stderr)
    trap("this should crash")
  }

  if arg == "testNoTrap" {
    println("child started")
    println("no crash")
    exit(5)
  }
}

