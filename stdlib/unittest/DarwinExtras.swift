//===--- DarwinExtras.swift -----------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import Darwin

var _stdlib_FD_SETSIZE: CInt {
  return 1024
}

struct _stdlib_fd_set {
  var _data: _UnitTestArray<UInt32>
  static var _wordBits: Int {
    return sizeof(UInt32) * 8
  }

  init() {
    _data = _UnitTestArray<UInt32>(
      count: Int(_stdlib_FD_SETSIZE) / _stdlib_fd_set._wordBits,
      repeatedValue: 0)
  }

  func isset(fd: CInt) -> Bool {
    let fdInt = Int(fd)
    return (
        _data[fdInt / _stdlib_fd_set._wordBits] &
          UInt32(1 << (fdInt % _stdlib_fd_set._wordBits))
      ) != 0
  }

  mutating func set(fd: CInt) {
    let fdInt = Int(fd)
    _data[fdInt / _stdlib_fd_set._wordBits] |=
      UInt32(1 << (fdInt % _stdlib_fd_set._wordBits))
  }

  mutating func clear(fd: CInt) {
    let fdInt = Int(fd)
    _data[fdInt / _stdlib_fd_set._wordBits] &=
      ~UInt32(1 << (fdInt % _stdlib_fd_set._wordBits))
  }

  mutating func zero() {
    let count = _data.count
    return _data.withUnsafeMutableBufferPointer {
      (_data) in
      for i in 0..<count {
        _data[i] = 0
      }
      return
    }
  }
}

func _stdlib_select(
  inout readfds: _stdlib_fd_set, inout writefds: _stdlib_fd_set,
  inout errorfds: _stdlib_fd_set, timeout: UnsafeMutablePointer<timeval>
) -> CInt {
  return readfds._data.withUnsafeMutableBufferPointer {
    (readfds) in
    writefds._data.withUnsafeMutableBufferPointer {
      (writefds) in
      errorfds._data.withUnsafeMutableBufferPointer {
        (errorfds) in
        let readAddr = readfds.baseAddress
        let writeAddr = writefds.baseAddress
        let errorAddr = errorfds.baseAddress
        return select(
          _stdlib_FD_SETSIZE,
          UnsafeMutablePointer(readAddr),
          UnsafeMutablePointer(writeAddr),
          UnsafeMutablePointer(errorAddr),
          timeout)
      }
    }
  }
}

/// Calls POSIX `pipe()`.
func posixPipe() -> (readFD: CInt, writeFD: CInt) {
  var fds: _UnitTestArray<CInt> = [ -1, -1 ]
  var _: Void = fds.withUnsafeMutableBufferPointer {
    (fds) in
    let ptr = fds.baseAddress
    if pipe(ptr) != 0 {
      preconditionFailure("pipe() failed")
    }
  }
  return (fds[0], fds[1])
}

/// Start the same executable as a child process, redirecting its stdout and
/// stderr.
func spawnChild(args: _UnitTestArray<String>)
  -> (pid: pid_t, stdinFD: CInt, stdoutFD: CInt, stderrFD: CInt) {
  var fileActions = posix_spawn_file_actions_t()
  if posix_spawn_file_actions_init(&fileActions) != 0 {
    preconditionFailure("posix_spawn_file_actions_init() failed")
  }

  let childStdin = posixPipe()
  // Close the write end of the pipe on the child side.
  if posix_spawn_file_actions_addclose(
    &fileActions, childStdin.writeFD) != 0 {
    preconditionFailure("posix_spawn_file_actions_addclose() failed")
  }

  // Remap child's stdin.
  if posix_spawn_file_actions_adddup2(
    &fileActions, childStdin.readFD, STDIN_FILENO) != 0 {
    preconditionFailure("posix_spawn_file_actions_adddup2() failed")
  }

  let childStdout = posixPipe()
  // Close the read end of the pipe on the child side.
  if posix_spawn_file_actions_addclose(
    &fileActions, childStdout.readFD) != 0 {
    preconditionFailure("posix_spawn_file_actions_addclose() failed")
  }

  // Remap child's stdout.
  if posix_spawn_file_actions_adddup2(
    &fileActions, childStdout.writeFD, STDOUT_FILENO) != 0 {
    preconditionFailure("posix_spawn_file_actions_adddup2() failed")
  }

  let childStderr = posixPipe()
  // Close the read end of the pipe on the child side.
  if posix_spawn_file_actions_addclose(
    &fileActions, childStderr.readFD) != 0 {
    preconditionFailure("posix_spawn_file_actions_addclose() failed")
  }

  // Remap child's stderr.
  if posix_spawn_file_actions_adddup2(
    &fileActions, childStderr.writeFD, STDERR_FILENO) != 0 {
    preconditionFailure("posix_spawn_file_actions_adddup2() failed")
  }

  var pid: pid_t = -1
  let spawnResult = withArrayOfCStrings([ Process.arguments[0] ] as _UnitTestArray + args) {
    posix_spawn(
      &pid, Process.arguments[0], &fileActions, nil, $0, _NSGetEnviron().memory)
  }
  if spawnResult != 0 {
    println(String.fromCString(strerror(spawnResult)))
    preconditionFailure("posix_spawn() failed")
  }

  if posix_spawn_file_actions_destroy(&fileActions) != 0 {
    preconditionFailure("posix_spawn_file_actions_destroy() failed")
  }

  // Close the read end of the pipe on the parent side.
  if close(childStdin.readFD) != 0 {
    preconditionFailure("close() failed")
  }

  // Close the write end of the pipe on the parent side.
  if close(childStdout.writeFD) != 0 {
    preconditionFailure("close() failed")
  }

  // Close the write end of the pipe on the parent side.
  if close(childStderr.writeFD) != 0 {
    preconditionFailure("close() failed")
  }

  return (pid, childStdin.writeFD, childStdout.readFD, childStderr.readFD)
}

func readAll(fd: CInt) -> String {
  var buffer = _UnitTestArray<UInt8>(count: 1024, repeatedValue: 0)
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
    preconditionFailure("read() failed")
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
    preconditionFailure("waitpid() failed")
  }
  if (WIFEXITED(status)) {
    return .Exit(Int(WEXITSTATUS(status)))
  }
  if (WIFSIGNALED(status)) {
    return .Signal(Int(WTERMSIG(status)))
  }
  preconditionFailure("did not understand what happened to child process")
}

func runChild(args: _UnitTestArray<String>)
  -> (stdout: String, stderr: String, status: ProcessTerminationStatus) {
  let (pid, _, stdoutFD, stderrFD) = spawnChild(args)

  // FIXME: reading stdout and stderr sequentially can block.  Should use
  // select().  This is not so simple to implement because of:
  // <rdar://problem/17828358> Darwin module is missing fd_set-related macros
  let stdout = readAll(stdoutFD)
  let stderr = readAll(stderrFD)

  if close(stdoutFD) != 0 {
    preconditionFailure("close() failed")
  }
  if close(stderrFD) != 0 {
    preconditionFailure("close() failed")
  }
  let status = posixWaitpid(pid)
  return (stdout, stderr, status)
}

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


