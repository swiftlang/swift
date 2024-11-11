import Foundation
import LinuxSystemHeaders

public class PTrace {
  enum Error: Swift.Error {
    case PTraceFailure(_ command: Int32, pid: pid_t, errno: Int32 = get_errno())
    case WaitFailure(pid: pid_t, errno: Int32 = get_errno())
    case IllegalArgument(description: String)
    case UnexpectedWaitStatus(pid: pid_t, status: Int32, sigInfo: siginfo_t? = nil)
  }

  let pid: pid_t

  public init(process pid: pid_t) throws {
    if ptrace_attach(pid) == -1 { throw Error.PTraceFailure(PTRACE_ATTACH, pid: pid) }

    while true {
      var status: Int32 = 0
      let result = waitpid(pid, &status, 0)
      if result == -1 {
        if get_errno() == EINTR { continue }
        throw Error.WaitFailure(pid: pid)
      }

      if result == pid && wIfStopped(status) { break }
    }

    self.pid = pid
  }

  deinit { ptrace_detach(self.pid) }

  public func cont() throws {
    if ptrace_continue(self.pid) == -1 { throw Error.PTraceFailure(PTRACE_CONT, pid: self.pid) }
  }

  public func getSigInfo() throws -> siginfo_t {
    var sigInfo = siginfo_t()
    if ptrace_getsiginfo(self.pid, &sigInfo) == -1 {
      throw Error.PTraceFailure(PTRACE_GETSIGINFO, pid: self.pid)
    }
    return sigInfo
  }

  public func pokeData(addr: UInt64, value: UInt64) throws {
    if ptrace_pokedata(self.pid, UInt(addr), UInt(value)) == -1 {
      throw Error.PTraceFailure(PTRACE_POKEDATA, pid: self.pid)
    }
  }

  public func getRegSet() throws -> RegisterSet {
    var regSet = RegisterSet()
    try withUnsafeMutableBytes(of: &regSet) {
      var vec = iovec(iov_base: $0.baseAddress!, iov_len: MemoryLayout<RegisterSet>.size)
      if ptrace_getregset(self.pid, NT_PRSTATUS, &vec) == -1 {
        throw Error.PTraceFailure(PTRACE_GETREGSET, pid: self.pid)
      }
    }
    return regSet
  }

  public func setRegSet(regSet: RegisterSet) throws {
    var regSetCopy = regSet
    try withUnsafeMutableBytes(of: &regSetCopy) {
      var vec = iovec(iov_base: $0.baseAddress!, iov_len: MemoryLayout<RegisterSet>.size)
      if ptrace_setregset(self.pid, NT_PRSTATUS, &vec) == -1 {
        throw Error.PTraceFailure(PTRACE_SETREGSET, pid: self.pid)
      }
    }
  }

  public func callRemoteFunction(
    at address: UInt64, with args: [UInt64] = [], onTrap callback: (() throws -> Void)? = nil
  ) throws -> UInt64 {

    guard args.count <= 6 else {
      throw Error.IllegalArgument(description: "max of 6 arguments allowed")
    }

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
      if result == -1 {
        if get_errno() == EINTR { continue }
        throw Error.WaitFailure(pid: self.pid)
      }

      if wIfExited(status) || wIfSignaled(status) {
        throw Error.UnexpectedWaitStatus(pid: self.pid, status: status)
      }

      if wIfStopped(status) {
        guard wStopSig(status) == SIGTRAP, let callback = callback else { break }

        // give the caller the opportunity to handle SIGTRAP
        try callback()
        try self.cont()
        continue
      }
    }

    let sigInfo = try self.getSigInfo()
    newRegs = try self.getRegSet()

    guard wStopSig(status) == SIGSEGV, siginfo_si_addr(sigInfo) == nil else {
      print("WSTOPSIG(status):\(wStopSig(status)), si_addr:\(siginfo_si_addr(sigInfo)!)")
      throw Error.UnexpectedWaitStatus(pid: self.pid, status: status, sigInfo: sigInfo)
    }

    return UInt64(newRegs.returnValue())
  }
}
