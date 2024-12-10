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

public class PTrace {
  enum PTraceError: Error {
    case ptraceFailure(_ command: Int32, pid: pid_t, errno: Int32 = get_errno())
    case waitFailure(pid: pid_t, errno: Int32 = get_errno())
    case unexpectedWaitStatus(pid: pid_t, status: Int32, sigInfo: siginfo_t? = nil)
  }

  let pid: pid_t

  // Initializing a PTrace instance attaches to the target process, waits for
  // it to stop, and leaves it in a stopped state. The caller may resume the
  // process by calling cont().
  public init(process pid: pid_t) throws {
    guard ptrace_attach(pid) != -1 else {
      throw PTraceError.ptraceFailure(PTRACE_ATTACH, pid: pid)
    }

    while true {
      var status: Int32 = 0
      let result = waitpid(pid, &status, 0)
      guard result != -1 else {
        if get_errno() == EINTR { continue }
        throw PTraceError.waitFailure(pid: pid)
      }

      precondition(pid == result, "waitpid returned unexpected value \(result)")

      if wIfStopped(status) { break }
    }

    self.pid = pid
  }

  deinit { _ = ptrace_detach(self.pid) }

  public func cont() throws {
    guard ptrace_cont(self.pid) != -1 else {
      throw PTraceError.ptraceFailure(PTRACE_CONT, pid: self.pid)
    }
  }

  public func interrupt() throws {
    guard ptrace_interrupt(self.pid) != -1 else {
      throw PTraceError.ptraceFailure(PTRACE_INTERRUPT, pid: self.pid)
    }
  }

  public func getSigInfo() throws -> siginfo_t {
    var sigInfo = siginfo_t()
    guard ptrace_getsiginfo(self.pid, &sigInfo) != -1 else {
      throw PTraceError.ptraceFailure(PTRACE_GETSIGINFO, pid: self.pid)
    }

    return sigInfo
  }

  public func pokeData(addr: UInt64, value: UInt64) throws {
    guard ptrace_pokedata(self.pid, UInt(addr), UInt(value)) != -1 else {
      throw PTraceError.ptraceFailure(PTRACE_POKEDATA, pid: self.pid)
    }
  }

  public func getRegSet() throws -> RegisterSet {
    var regSet = RegisterSet()
    try withUnsafeMutableBytes(of: &regSet) {
      var vec = iovec(iov_base: $0.baseAddress!, iov_len: MemoryLayout<RegisterSet>.size)
      guard ptrace_getregset(self.pid, NT_PRSTATUS, &vec) != -1 else {
        throw PTraceError.ptraceFailure(PTRACE_GETREGSET, pid: self.pid)
      }
    }

    return regSet
  }

  public func setRegSet(regSet: RegisterSet) throws {
    var regSetCopy = regSet
    try withUnsafeMutableBytes(of: &regSetCopy) {
      var vec = iovec(iov_base: $0.baseAddress!, iov_len: MemoryLayout<RegisterSet>.size)
      guard ptrace_setregset(self.pid, NT_PRSTATUS, &vec) != -1 else {
        throw PTraceError.ptraceFailure(PTRACE_SETREGSET, pid: self.pid)
      }
    }
  }

  // Call the function at the specified address in the attached process. Caller
  // may pass up to six 8-byte arguments. The optional callback is invoked when
  // the process is stopped with a SIGTRAP signal. In this case, the caller is
  // responsible for taking action on the signal.
  public func callRemoteFunction(
    at address: UInt64, with args: [UInt64] = [], onTrap callback: (() throws -> Void)? = nil
  ) throws -> UInt64 {
    precondition(args.count <= 6, "callRemoteFunction supports max of 6 arguments")

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

    var status: Int32 = 0
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
      try callback()
    }

    let sigInfo = try self.getSigInfo()
    newRegs = try self.getRegSet()

    guard wStopSig(status) == SIGSEGV, siginfo_si_addr(sigInfo) == nil else {
      throw PTraceError.unexpectedWaitStatus(pid: self.pid, status: status, sigInfo: sigInfo)
    }

    return UInt64(newRegs.returnValue())
  }
}
