//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import Foundation
import SymbolicationShims

private let symbolicationPath =
  "/System/Library/PrivateFrameworks/Symbolication.framework/Symbolication"
private let symbolicationHandle = dlopen(symbolicationPath, RTLD_LAZY)!

private let coreSymbolicationPath =
  "/System/Library/PrivateFrameworks/CoreSymbolication.framework/CoreSymbolication"
private let coreSymbolicationHandle = dlopen(coreSymbolicationPath, RTLD_LAZY)!

private func symbol<T>(_ handle: UnsafeMutableRawPointer, _ name: String) -> T {
  guard let result = dlsym(handle, name) else {
    fatalError("Unable to look up \(name) in Symbolication")
  }
  return unsafeBitCast(result, to: T.self)
}

enum Sym {
  static let pidFromHint: @convention(c) (AnyObject) -> pid_t =
    symbol(symbolicationHandle, "pidFromHint")
  static let CSSymbolicatorCreateWithTask: @convention(c) (task_t) -> CSTypeRef =
    symbol(coreSymbolicationHandle, "CSSymbolicatorCreateWithTask")
  static let CSSymbolicatorGetSymbolOwnerWithNameAtTime:
    @convention(c) (CSTypeRef, UnsafePointer<CChar>, CSMachineTime) -> CSTypeRef =
      symbol(coreSymbolicationHandle, "CSSymbolicatorGetSymbolOwnerWithNameAtTime")
  static let CSSymbolOwnerForeachSymbol:
    @convention(c) (CSTypeRef, @convention(block) (CSTypeRef) -> Void) -> UInt =
      symbol(coreSymbolicationHandle, "CSSymbolOwnerForeachSymbol")
  static let CSSymbolOwnerGetSymbolWithMangledName: @convention(c)
    (CSTypeRef, UnsafePointer<CChar>) -> CSTypeRef =
      symbol(coreSymbolicationHandle, "CSSymbolOwnerGetSymbolWithMangledName")
  static let CSSymbolGetName: @convention(c) (CSTypeRef) -> UnsafePointer<CChar>? =
    symbol(coreSymbolicationHandle, "CSSymbolGetName")
  static let CSSymbolGetMangledName: @convention(c) (CSTypeRef) -> UnsafePointer<CChar>? =
    symbol(coreSymbolicationHandle, "CSSymbolGetMangledName")
  static let CSSymbolGetSymbolOwner: @convention(c)
    (CSSymbolRef) -> CSSymbolOwnerRef =
    symbol(coreSymbolicationHandle, "CSSymbolGetSymbolOwner")
  static let CSSymbolIsFunction: @convention(c) (CSTypeRef) -> CBool =
    symbol(coreSymbolicationHandle, "CSSymbolIsFunction")
  static let CSSymbolGetRange: @convention(c) (CSTypeRef) -> Range =
    symbol(coreSymbolicationHandle, "CSSymbolGetRange")
  static let CSSymbolOwnerGetName: @convention(c) (CSSymbolOwnerRef) -> UnsafePointer<CChar>? =
    symbol(coreSymbolicationHandle, "CSSymbolOwnerGetName")
  static let CSSymbolicatorGetSymbolWithAddressAtTime: @convention(c)
    (CSSymbolicatorRef, mach_vm_address_t, CSMachineTime) -> CSSymbolRef =
    symbol(coreSymbolicationHandle, "CSSymbolicatorGetSymbolWithAddressAtTime")
  static let task_start_peeking: @convention(c) (task_t) -> kern_return_t =
    symbol(symbolicationHandle, "task_start_peeking")
  static let task_peek: @convention(c) (task_t, mach_vm_address_t, mach_vm_size_t,
                                        UnsafeMutablePointer<UnsafeRawPointer?>) ->
                                         kern_return_t =
    symbol(symbolicationHandle, "task_peek")
  static let task_peek_string: @convention(c) (task_t, mach_vm_address_t) ->
                                              UnsafeMutablePointer<CChar>? =
    symbol(symbolicationHandle, "task_peek_string")
  static let task_stop_peeking: @convention(c) (task_t) -> kern_return_t =
    symbol(symbolicationHandle, "task_stop_peeking")
}

typealias CSMachineTime = UInt64
let kCSNow = CSMachineTime(Int64.max) + 1

typealias CSSymbolicatorRef = CSTypeRef
typealias CSSymbolRef = CSTypeRef
typealias CSSymbolOwnerRef = CSTypeRef

func pidFromHint(_ hint: String) -> pid_t? {
  let result = Sym.pidFromHint(hint as NSString)
  return result == 0 ? nil : result
}

func CSSymbolicatorCreateWithTask(_ task: task_t) -> CSTypeRef {
  Sym.CSSymbolicatorCreateWithTask(task)
}

func CSSymbolicatorGetSymbolOwnerWithNameAtTime(
	_ symbolicator: CSTypeRef,
  _ name: String,
  _ time: CSMachineTime
) -> CSTypeRef {
  Sym.CSSymbolicatorGetSymbolOwnerWithNameAtTime(symbolicator, name, time)
}

@discardableResult
func CSSymbolOwnerForeachSymbol(
  _ symbolOwner: CSTypeRef,
  _ iterator: (CSTypeRef) -> Void
) -> UInt {
  Sym.CSSymbolOwnerForeachSymbol(symbolOwner, iterator)
}

func CSSymbolOwnerGetSymbolWithMangledName(
  _ owner: CSTypeRef, 
  _ name: String
) -> CSTypeRef {
  Sym.CSSymbolOwnerGetSymbolWithMangledName(owner, name)
}

func CSSymbolGetName(_ sym: CSTypeRef) -> String? {
  let name = Sym.CSSymbolGetName(sym)
  return name.map{ String(cString: $0) }
}

func CSSymbolGetMangledName(_ sym: CSTypeRef) -> String? {
  let name = Sym.CSSymbolGetMangledName(sym)
  return name.map{ String(cString: $0) }
}

func CSSymbolIsFunction(_ sym: CSTypeRef) -> Bool {
  Sym.CSSymbolIsFunction(sym)
}

func CSSymbolGetRange(_ sym: CSTypeRef) -> Range {
  Sym.CSSymbolGetRange(sym)
}

func CSSymbolGetSymbolOwner(_ sym: CSTypeRef) -> CSSymbolOwnerRef {
  Sym.CSSymbolGetSymbolOwner(sym)
}

func CSSymbolOwnerGetName(_ sym: CSTypeRef) -> String? {
  Sym.CSSymbolOwnerGetName(sym)
    .map(String.init(cString:))
}

func CSSymbolicatorGetSymbolWithAddressAtTime(
  _ symbolicator: CSSymbolicatorRef,
  _ address: mach_vm_address_t,
  _ time: CSMachineTime
) -> CSSymbolRef {
  Sym.CSSymbolicatorGetSymbolWithAddressAtTime(symbolicator, address, time)
}

func task_start_peeking(_ task: task_t) -> Bool {
  let result = Sym.task_start_peeking(task)
  if result == KERN_SUCCESS {
    return true
  }
  
  print("task_start_peeking failed: \(machErrStr(result))", to: &Std.err)
  return false
}

func task_peek(
  _ task: task_t, _ start: mach_vm_address_t, _ size: mach_vm_size_t
) -> UnsafeRawPointer? {
  var ptr: UnsafeRawPointer? = nil
  let result = Sym.task_peek(task, start, size, &ptr)
  if result != KERN_SUCCESS {
    print("Unable to read (\(start), \(size)): \(machErrStr(result))", to: &Std.err)
    return nil
  }
  return ptr
}

func task_peek_string(
  _ task: task_t, _ addr: mach_vm_address_t
) -> UnsafeMutablePointer<CChar>? {
  Sym.task_peek_string(task, addr)
}

func task_stop_peeking(_ task: task_t) {
  _ = Sym.task_stop_peeking(task)
}
