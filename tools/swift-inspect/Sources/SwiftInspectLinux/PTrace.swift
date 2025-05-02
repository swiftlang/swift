//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import Foundation
import LinuxSystemHeaders

// Provides scoped access to a PTrace object.
public func withPTracedProcess(pid: pid_t, _ closure: (consuming PTrace) throws -> Void) throws {
  let ptrace = try PTrace(pid)
  try closure(ptrace)
}

public struct PTrace: ~Copyable {
  enum PTraceError: Error {
    case operationFailure(_ command: CInt, pid: pid_t, errno: CInt = get_errno())
    case waitFailure(pid: pid_t, errno: CInt = get_errno())
    case unexpectedWaitStatus(pid: pid_t, status: CInt, sigInfo: siginfo_t? = nil)
  }

  let pid: pid_t

  // Initializing a PTrace instance attaches to the target process, waits for
  // it to stop, and leaves it in a stopped state. The caller may resume the
  // process by calling cont().
  // NOTE: clients must use withPTracedProcess instead of direct initialization.
  fileprivate init(_ pid: pid_t) throws {
    self.pid = pid

    guard ptrace_attach(self.pid) != -1 else {
      throw PTraceError.operationFailure(PTRACE_ATTACH, pid: self.pid)
    }

    while true {
      var status: CInt = 0
      let result = waitpid(self.pid, &status, 0)
      if result == -1 {
        if get_errno() == EINTR { continue }
        throw PTraceError.waitFailure(pid: self.pid)
      }

      precondition(self.pid == result,
                   "waitpid returned unexpected value \(result)")

      if wIfStopped(status) { break }
    }
  }

  deinit { _ = ptrace_detach(self.pid) }

  public func cont() throws {
    if ptrace_cont(self.pid) == -1 {
      throw PTraceError.operationFailure(PTRACE_CONT, pid: self.pid)
    }
  }

  public func interrupt() throws {
    if ptrace_interrupt(self.pid) == -1 {
      throw PTraceError.operationFailure(PTRACE_INTERRUPT, pid: self.pid)
    }
  }

  public func getSigInfo() throws -> siginfo_t {
    var sigInfo = siginfo_t()
    if ptrace_getsiginfo(self.pid, &sigInfo) == -1 {
      throw PTraceError.operationFailure(PTRACE_GETSIGINFO, pid: self.pid)
    }

    return sigInfo
  }

  public func pokeData(addr: UInt64, value: UInt64) throws {
    if ptrace_pokedata(self.pid, UInt(addr), UInt(value)) == -1 {
      throw PTraceError.operationFailure(PTRACE_POKEDATA, pid: self.pid)
    }
  }

  public func getRegSet() throws -> RegisterSet {
    var regSet = RegisterSet()
    try withUnsafeMutableBytes(of: &regSet) {
      var vec = iovec(iov_base: $0.baseAddress!, iov_len: MemoryLayout<RegisterSet>.size)
      if ptrace_getregset(self.pid, NT_PRSTATUS, &vec) == -1 {
        throw PTraceError.operationFailure(PTRACE_GETREGSET, pid: self.pid)
      }
    }

    return regSet
  }

  public func setRegSet(regSet: RegisterSet) throws {
    var regSetCopy = regSet
    try withUnsafeMutableBytes(of: &regSetCopy) {
      var vec = iovec(iov_base: $0.baseAddress!, iov_len: MemoryLayout<RegisterSet>.size)
      if ptrace_setregset(self.pid, NT_PRSTATUS, &vec) == -1 {
        throw PTraceError.operationFailure(PTRACE_SETREGSET, pid: self.pid)
      }
    }
  }

  // Call the function at the specified address in the attached process with the
  // provided argument array. The optional callback is invoked when the process
  // is stopped with a SIGTRAP signal. In this case, the caller is responsible
  // for taking action on the signal.
  public func jump(
    to address: UInt64, with args: [UInt64] = [],
    _ callback: ((borrowing PTrace) throws -> Void)? = nil
  ) throws -> UInt64 {
    let origRegs = try self.getRegSet()
    defer { try? self.setRegSet(regSet: origRegs) }

    // Set the return address to 0. This forces the function to return to 0 on
    // completion, resulting in a SIGSEGV with address 0 which will interrupt
    // the process and notify us (the tracer) via waitpid(). At that point, we
    // will restore the original state and continue the process.
    let returnAddr: UInt64 = 0

    var newRegs = try origRegs.setupCall(self, to: address, with: args, returnTo: returnAddr)
    try self.setRegSet(regSet: newRegs)
    try self.cont()

    var status: CInt = 0
    while true {
      let result = waitpid(self.pid, &status, 0)
      guard result != -1 else {
        if get_errno() == EINTR { continue }
        throw PTraceError.waitFailure(pid: self.pid)
      }

      precondition(self.pid == result, "waitpid returned unexpected value \(result)")

      guard wIfStopped(status) && !wIfExited(status) && !wIfSignaled(status) else {
        throw PTraceError.unexpectedWaitStatus(pid: self.pid, status: status)
      }

      guard wStopSig(status) == SIGTRAP, let callback = callback else { break }

      // give the caller the opportunity to handle SIGTRAP
      try callback(self)
    }

    let sigInfo = try self.getSigInfo()
    newRegs = try self.getRegSet()

    guard wStopSig(status) == SIGSEGV, siginfo_si_addr(sigInfo) == nil else {
      throw PTraceError.unexpectedWaitStatus(pid: self.pid, status: status, sigInfo: sigInfo)
    }

    return UInt64(newRegs.returnValue())
  }
}
