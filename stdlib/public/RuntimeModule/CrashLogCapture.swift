//===--- CrashLogCapture.swift --------------------------------*- swift -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  Captures registers for a CrashLog from a Context.
//
//===----------------------------------------------------------------------===//

import Swift

@_spi(CrashLog)
@available(Backtracing 6.2, *)
public class CrashLogCapture<Address: FixedWidthInteger> {
  public typealias Thread = CrashLog<Address>.Thread

  private(set) public var capturedMemory: [String:String] = [:]

  private let memoryReader: MemoryReader

  public init(memoryReader: MemoryReader) {
    self.memoryReader = memoryReader
  }

  private func captureMemory(value: Address) -> Void {
    if let bytes = try? memoryReader.fetch(
        from: MemoryReader.Address(value),
        count: 16,
        as: UInt8.self)
    {
      let formattedBytes = bytes
        .map{ hex($0, prefix: false) }
        .joined(separator: "")

      let memoryAddress = hex(UInt64(truncatingIfNeeded: value), prefix: true)
      capturedMemory[memoryAddress] = formattedBytes
    }
  }

  func captureRegister(name: String,
  value: Address,
  into thread: inout Thread) {
    thread.registers?[name] = hex(value)
    captureMemory(value: value)
  }

  func captureRegister<C: Context>(
    name: String,
    context: C,
    register: C.Register,
    into thread: inout Thread)
    where Address == C.GPRValue
  {
    captureRegister(name: name,
    value: context.getRegister(register)!, into: &thread)
  }

  func captureGPRs<C: Context, Rs: Sequence>(
    _ context: C, range: Rs, into thread: inout Thread)
    where Rs.Element == C.Register, Address == C.GPRValue
  {
    for reg in range {
      captureRegister(name: "\(reg)",
      context: context,
      register: reg,
      into: &thread)
    }
  }

  public func captureRegisterDump(_ context: X86_64Context,
  into thread: inout Thread)
  where Address == X86_64Context.GPRValue
  {
    captureGPRs(context, range: .rax ... .r15, into: &thread)

    captureRegister(name: "rip",
    value: context.programCounter,
    into: &thread)

    captureRegister(name: "rflags",
    context: context,
    register: .rflags,
    into: &thread)

    captureRegister(name: "cs",
    context: context,
    register: .cs,
    into: &thread)

    captureRegister(name: "fs",
    context: context,
    register: .fs,
    into: &thread)

    captureRegister(name: "gs",
    context: context,
    register: .gs,
    into: &thread)
  }

  public func captureRegisterDump(_ context: I386Context,
  into thread: inout Thread)
  where Address == I386Context.GPRValue 
  {
    captureGPRs(context, range: .eax ... .edi, into: &thread)

    captureRegister(name: "eip", value: context.programCounter, into: &thread)

    captureRegister(name: "eflags",
    context: context,
    register: .eflags,
    into: &thread)

    captureRegister(name: "es", context: context, register: .es, into: &thread)
    captureRegister(name: "cs", context: context, register: .cs, into: &thread)
    captureRegister(name: "ss", context: context, register: .ss, into: &thread)
    captureRegister(name: "ds", context: context, register: .ds, into: &thread)
    captureRegister(name: "fs", context: context, register: .fs, into: &thread)
    captureRegister(name: "gs", context: context, register: .gs, into: &thread)
  }

  public func captureRegisterDump(_ context: ARM64Context,
  into thread: inout Thread)
  where Address == ARM64Context.GPRValue 
  {
    captureGPRs(context, range: .x0 ..< .x29, into: &thread)
    captureRegister(name: "fp", context: context, register: .x29, into: &thread)
    captureRegister(name: "lr", context: context, register: .x30, into: &thread)
    captureRegister(name: "sp", context: context, register: .sp, into: &thread)
    captureRegister(name: "pc", context: context, register: .pc, into: &thread)
  }

  public func captureRegisterDump(_ context: ARMContext,
  into thread: inout Thread)
  where Address == ARMContext.GPRValue 
  {
    captureGPRs(context, range: .r0 ... .r10, into: &thread)
    captureRegister(name: "fp", context: context, register: .r11, into: &thread)
    captureRegister(name: "ip", context: context, register: .r12, into: &thread)
    captureRegister(name: "sp", context: context, register: .r13, into: &thread)
    captureRegister(name: "lr", context: context, register: .r14, into: &thread)
    captureRegister(name: "pc", context: context, register: .r15, into: &thread)
  }  
}
