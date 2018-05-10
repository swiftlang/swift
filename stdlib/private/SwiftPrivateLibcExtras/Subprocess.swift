//===--- Subprocess.swift -------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import SwiftPrivate
#if os(macOS) || os(iOS) || os(watchOS) || os(tvOS)
import Darwin
#elseif os(Linux) || os(FreeBSD) || os(PS4) || os(Android) || os(Cygwin) || os(Haiku)
import Glibc
#endif


#if !os(Windows)
// posix_spawn is not available on Windows.
// posix_spawn is not available on Android.
// posix_spawn is not available on Haiku.
#if !os(Android) && !os(Haiku)
// posix_spawn isn't available in the public watchOS SDK, we sneak by the
// unavailable attribute declaration here of the APIs that we need.

// FIXME: Come up with a better way to deal with APIs that are pointers on some
// platforms but not others.
#if os(Linux)
typealias _stdlib_posix_spawn_file_actions_t = posix_spawn_file_actions_t
#else
typealias _stdlib_posix_spawn_file_actions_t = posix_spawn_file_actions_t?
#endif

@_silgen_name("_stdlib_posix_spawn_file_actions_init")
internal func _stdlib_posix_spawn_file_actions_init(
  _ file_actions: UnsafeMutablePointer<_stdlib_posix_spawn_file_actions_t>
) -> CInt

@_silgen_name("_stdlib_posix_spawn_file_actions_destroy")
internal func _stdlib_posix_spawn_file_actions_destroy(
  _ file_actions: UnsafeMutablePointer<_stdlib_posix_spawn_file_actions_t>
) -> CInt

@_silgen_name("_stdlib_posix_spawn_file_actions_addclose")
internal func _stdlib_posix_spawn_file_actions_addclose(
  _ file_actions: UnsafeMutablePointer<_stdlib_posix_spawn_file_actions_t>,
  _ filedes: CInt) -> CInt

@_silgen_name("_stdlib_posix_spawn_file_actions_adddup2")
internal func _stdlib_posix_spawn_file_actions_adddup2(
  _ file_actions: UnsafeMutablePointer<_stdlib_posix_spawn_file_actions_t>,
  _ filedes: CInt,
  _ newfiledes: CInt) -> CInt

@_silgen_name("_stdlib_posix_spawn")
internal func _stdlib_posix_spawn(
  _ pid: UnsafeMutablePointer<pid_t>?,
  _ file: UnsafePointer<Int8>,
  _ file_actions: UnsafePointer<_stdlib_posix_spawn_file_actions_t>?,
  _ attrp: UnsafePointer<posix_spawnattr_t>?,
  _ argv: UnsafePointer<UnsafeMutablePointer<Int8>?>,
  _ envp: UnsafePointer<UnsafeMutablePointer<Int8>?>?) -> CInt
#endif

/// Calls POSIX `pipe()`.
func posixPipe() -> (readFD: CInt, writeFD: CInt) {
  var fds: [CInt] = [ -1, -1 ]
  if pipe(&fds) != 0 {
    preconditionFailure("pipe() failed")
  }
  return (fds[0], fds[1])
}

/// Start the same executable as a child process, redirecting its stdout and
/// stderr.
public func spawnChild(_ args: [String])
  -> (pid: pid_t, stdinFD: CInt, stdoutFD: CInt, stderrFD: CInt) {
  // The stdout, stdin, and stderr from the child process will be redirected
  // to these pipes.
  let childStdout = posixPipe()
  let childStdin = posixPipe()
  let childStderr = posixPipe()

#if os(Android) || os(Haiku)
  // posix_spawn isn't available on Android. Instead, we fork and exec.
  // To correctly communicate the exit status of the child process to this
  // (parent) process, we'll use this pipe.
  let childToParentPipe = posixPipe()

  let pid = fork()
  precondition(pid >= 0, "fork() failed")
  if pid == 0 {
    // pid of 0 means we are now in the child process.
    // Capture the output before executing the program.
    dup2(childStdout.writeFD, STDOUT_FILENO)
    dup2(childStdin.readFD, STDIN_FILENO)
    dup2(childStderr.writeFD, STDERR_FILENO)

    // Set the "close on exec" flag on the parent write pipe. This will
    // close the pipe if the execve() below successfully executes a child
    // process.
    let closeResult = fcntl(childToParentPipe.writeFD, F_SETFD, FD_CLOEXEC)
    let closeErrno = errno
    precondition(
      closeResult == 0,
      "Could not set the close behavior of the child-to-parent pipe; " +
      "errno: \(closeErrno)")

    // Start the executable. If execve() does not encounter an error, the
    // code after this block will never be executed, and the parent write pipe
    // will be closed.
    withArrayOfCStrings([CommandLine.arguments[0]] + args) {
      execve(CommandLine.arguments[0], $0, environ)
    }

    // If execve() encountered an error, we write the errno encountered to the
    // parent write pipe.
    let errnoSize = MemoryLayout.size(ofValue: errno)
    var execveErrno = errno
    let writtenBytes = withUnsafePointer(to: &execveErrno) {
      write(childToParentPipe.writeFD, UnsafePointer($0), errnoSize)
    }

    let writeErrno = errno
    if writtenBytes > 0 && writtenBytes < errnoSize {
      // We were able to write some of our error, but not all of it.
      // FIXME: Retry in this case.
      preconditionFailure("Unable to write entire error to child-to-parent " +
                          "pipe.")
    } else if writtenBytes == 0 {
      preconditionFailure("Unable to write error to child-to-parent pipe.")
    } else if writtenBytes < 0 {
      preconditionFailure("An error occurred when writing error to " +
                          "child-to-parent pipe; errno: \(writeErrno)")
    }

    // Close the pipe when we're done writing the error.
    close(childToParentPipe.writeFD)
  }
#else
  var fileActions = _make_posix_spawn_file_actions_t()
  if _stdlib_posix_spawn_file_actions_init(&fileActions) != 0 {
    preconditionFailure("_stdlib_posix_spawn_file_actions_init() failed")
  }

  // Close the write end of the pipe on the child side.
  if _stdlib_posix_spawn_file_actions_addclose(
    &fileActions, childStdin.writeFD) != 0 {
    preconditionFailure("_stdlib_posix_spawn_file_actions_addclose() failed")
  }

  // Remap child's stdin.
  if _stdlib_posix_spawn_file_actions_adddup2(
    &fileActions, childStdin.readFD, STDIN_FILENO) != 0 {
    preconditionFailure("_stdlib_posix_spawn_file_actions_adddup2() failed")
  }

  // Close the read end of the pipe on the child side.
  if _stdlib_posix_spawn_file_actions_addclose(
    &fileActions, childStdout.readFD) != 0 {
    preconditionFailure("_stdlib_posix_spawn_file_actions_addclose() failed")
  }

  // Remap child's stdout.
  if _stdlib_posix_spawn_file_actions_adddup2(
    &fileActions, childStdout.writeFD, STDOUT_FILENO) != 0 {
    preconditionFailure("_stdlib_posix_spawn_file_actions_adddup2() failed")
  }

  // Close the read end of the pipe on the child side.
  if _stdlib_posix_spawn_file_actions_addclose(
    &fileActions, childStderr.readFD) != 0 {
    preconditionFailure("_stdlib_posix_spawn_file_actions_addclose() failed")
  }

  // Remap child's stderr.
  if _stdlib_posix_spawn_file_actions_adddup2(
    &fileActions, childStderr.writeFD, STDERR_FILENO) != 0 {
    preconditionFailure("_stdlib_posix_spawn_file_actions_adddup2() failed")
  }

  var pid: pid_t = -1
  var childArgs = args
  childArgs.insert(CommandLine.arguments[0], at: 0)
  let interpreter = getenv("SWIFT_INTERPRETER")
  if interpreter != nil {
    if let invocation = String(validatingUTF8: interpreter!) {
      childArgs.insert(invocation, at: 0)
    }
  }
  let spawnResult = withArrayOfCStrings(childArgs) {
    _stdlib_posix_spawn(
      &pid, childArgs[0], &fileActions, nil, $0, environ)
  }
  if spawnResult != 0 {
    print(String(cString: strerror(spawnResult)))
    preconditionFailure("_stdlib_posix_spawn() failed")
  }

  if _stdlib_posix_spawn_file_actions_destroy(&fileActions) != 0 {
    preconditionFailure("_stdlib_posix_spawn_file_actions_destroy() failed")
  }
#endif

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

#if !os(Android) && !os(Haiku)
#if os(Linux)
internal func _make_posix_spawn_file_actions_t()
  -> _stdlib_posix_spawn_file_actions_t {
  return posix_spawn_file_actions_t()
}
#else
internal func _make_posix_spawn_file_actions_t()
  -> _stdlib_posix_spawn_file_actions_t {
  return nil
}
#endif
#endif

internal func _signalToString(_ signal: Int) -> String {
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
  case exit(Int)
  case signal(Int)

  public var description: String {
    switch self {
    case .exit(let status):
      return "Exit(\(status))"
    case .signal(let signal):
      return "Signal(\(_signalToString(signal)))"
    }
  }
}

public func posixWaitpid(_ pid: pid_t) -> ProcessTerminationStatus {
  var status: CInt = 0
#if os(Cygwin)
  withUnsafeMutablePointer(to: &status) {
    statusPtr in
    let statusPtrWrapper = __wait_status_ptr_t(__int_ptr: statusPtr)
    while waitpid(pid, statusPtrWrapper, 0) < 0 {
      if errno != EINTR {
        preconditionFailure("waitpid() failed")
      }
    }
  }
#else
  while waitpid(pid, &status, 0) < 0 {
    if errno != EINTR {
      preconditionFailure("waitpid() failed")
    }
  }
#endif
  if WIFEXITED(status) {
    return .exit(Int(WEXITSTATUS(status)))
  }
  if WIFSIGNALED(status) {
    return .signal(Int(WTERMSIG(status)))
  }
  preconditionFailure("did not understand what happened to child process")
}

// !os(Windows)
#endif

