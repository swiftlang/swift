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

import SwiftPrivate
#if os(OSX) || os(iOS) || os(watchOS) || os(tvOS)
import Darwin
#elseif os(Linux)
import Glibc
#endif

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
public func spawnChild(args: _UnitTestArray<String>)
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
      &pid, Process.arguments[0], &fileActions, nil, $0, _getEnviron())
  }
  if spawnResult != 0 {
    print(String.fromCString(strerror(spawnResult)))
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

internal func _readAll(fd: CInt) -> String {
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


internal func _signalToString(signal: Int) -> String {
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

public enum ProcessTerminationStatus : CustomStringConvertible {
  case Exit(Int)
  case Signal(Int)

  public var description: String {
    switch self {
    case .Exit(let status):
      return "Exit(\(status))"
    case .Signal(let signal):
      return "Signal(\(_signalToString(signal)))"
    }
  }
}

public func posixWaitpid(pid: pid_t) -> ProcessTerminationStatus {
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

public func runChild(args: _UnitTestArray<String>)
  -> (stdout: String, stderr: String, status: ProcessTerminationStatus) {
  let (pid, _, stdoutFD, stderrFD) = spawnChild(args)

  // FIXME: reading stdout and stderr sequentially can block.  Should use
  // select().  This is not so simple to implement because of:
  // <rdar://problem/17828358> Darwin module is missing fd_set-related macros
  let stdout = _readAll(stdoutFD)
  let stderr = _readAll(stderrFD)

  if close(stdoutFD) != 0 {
    preconditionFailure("close() failed")
  }
  if close(stderrFD) != 0 {
    preconditionFailure("close() failed")
  }
  let status = posixWaitpid(pid)
  return (stdout, stderr, status)
}

#if os(OSX) || os(iOS) || os(watchOS) || os(tvOS)
@asmname("_NSGetEnviron")
func _NSGetEnviron() -> UnsafeMutablePointer<UnsafeMutablePointer<UnsafeMutablePointer<CChar>>>
#endif

internal func _getEnviron() -> UnsafeMutablePointer<UnsafeMutablePointer<CChar>> {
#if os(OSX) || os(iOS) || os(watchOS) || os(tvOS)
  return _NSGetEnviron().memory
#else
  return __environ
#endif
}
