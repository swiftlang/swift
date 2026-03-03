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

#if arch(arm64)
public typealias RegisterSet = user_pt_regs

extension RegisterSet {
  public static var trapInstructionSize: UInt { return 4 }  // brk #0x0

  public func setupCall(
    _ ptrace: borrowing PTrace, to funcAddr: UInt64, with args: [UInt64],
    returnTo returnAddr: UInt64
  ) throws -> RegisterSet {
    // The first 8 arguments are passed in regsters. Any additional arguments
    // must be pushed on the stack, which is not implemented.
    precondition(args.count <= 8)

    var registers = self
    registers.regs.0 = args.count > 0 ? args[0] : 0
    registers.regs.1 = args.count > 1 ? args[1] : 0
    registers.regs.2 = args.count > 2 ? args[2] : 0
    registers.regs.3 = args.count > 3 ? args[3] : 0
    registers.regs.4 = args.count > 4 ? args[4] : 0
    registers.regs.5 = args.count > 5 ? args[5] : 0
    registers.regs.6 = args.count > 6 ? args[6] : 0
    registers.regs.7 = args.count > 7 ? args[7] : 0
    registers.pc = funcAddr
    registers.regs.30 = returnAddr  // link register (x30)
    return registers
  }

  public func returnValue() -> UInt64 {
    return self.regs.0
  }

  public mutating func step(_ bytes: UInt) {
    self.pc += UInt64(bytes)
  }
}

#elseif arch(x86_64)
public typealias RegisterSet = pt_regs

extension RegisterSet {
  public static var trapInstructionSize: UInt { return 1 }  // int3

  public func setupCall(
    _ ptrace: borrowing PTrace, to funcAddr: UInt64, with args: [UInt64],
    returnTo returnAddr: UInt64
  ) throws -> RegisterSet {
    // The first 6 arguments are passed in registers. Any additional arguments
    // must be pushed on the stack, which is not implemented.
    precondition(args.count <= 6)

    var registers = self
    registers.rdi = UInt(args.count > 0 ? args[0] : 0)
    registers.rsi = UInt(args.count > 1 ? args[1] : 0)
    registers.rdx = UInt(args.count > 2 ? args[2] : 0)
    registers.rcx = UInt(args.count > 3 ? args[3] : 0)
    registers.r8 = UInt(args.count > 4 ? args[4] : 0)
    registers.r9 = UInt(args.count > 5 ? args[5] : 0)
    registers.rip = UInt(funcAddr)
    registers.rax = 0  // rax is the number of args in a va_args function

    // push the return address onto the stack
    registers.rsp -= UInt(MemoryLayout<UInt64>.size)
    try ptrace.pokeData(addr: UInt64(registers.rsp), value: returnAddr)

    return registers
  }

  public func returnValue() -> UInt64 {
    return UInt64(self.rax)
  }

  public mutating func step(_ bytes: UInt) {
    self.rip += UInt(bytes)
  }
}
#else
#error("Only arm64 and x86_64 architectures are supported")
#endif
