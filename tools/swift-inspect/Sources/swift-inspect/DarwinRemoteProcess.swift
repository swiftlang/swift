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

#if os(iOS) || os(macOS) || os(tvOS) || os(watchOS)

import SwiftRemoteMirror
import SymbolicationShims

internal final class DarwinRemoteProcess: RemoteProcess {
  public typealias ProcessIdentifier = pid_t
  public typealias ProcessHandle = task_t

  private var task = Cleanup<task_t> {
    // task_stop_peeking does nothing if we didn't task_start_peeking first, so
    // we can call it unconditionally.
    task_stop_peeking($0)
    mach_port_deallocate(mach_task_self_, $0)
  }

  internal var processIdentifier: ProcessIdentifier
  internal lazy var processName = getProcessName(processId: processIdentifier) ?? "<unknown process>"

  public var process: ProcessHandle { task.value }
  private var _context = Cleanup<SwiftReflectionContextRef> {
    swift_reflection_destroyReflectionContext($0)
  }
  public var context: SwiftReflectionContextRef! { _context.value }

  private var symbolicator = Cleanup<CSSymbolicatorRef> {
    CSRelease($0)
  }

  private var swiftCore: CSTypeRef
  private let swiftConcurrency: CSTypeRef

  private lazy var threadInfos = getThreadInfos()

  static var QueryDataLayout: QueryDataLayoutFunction {
    return { (context, type, _, output) in
      guard let output = output else { return 0 }

      switch type {
      case DLQ_GetPointerSize, DLQ_GetSizeSize:
        let size = UInt8(MemoryLayout<UnsafeRawPointer>.stride)
        output.storeBytes(of: size, toByteOffset: 0, as: UInt8.self)
        return 1

      case DLQ_GetPtrAuthMask:
        let mask = GetPtrauthMask()
        output.storeBytes(of: mask, toByteOffset: 0, as: UInt.self)
        return 1

      case DLQ_GetObjCReservedLowBits:
        var size: UInt8 = 0
#if os(macOS)
        // Only 64-bit macOS reserves pointer bit-packing.
        if MemoryLayout<UnsafeRawPointer>.stride == 8 { size = 1 }
#endif
        output.storeBytes(of: size, toByteOffset: 0, as: UInt8.self)
        return 1

      case DLQ_GetLeastValidPointerValue:
        var value: UInt64 = 0x1000
#if os(iOS) || os(macOS) || os(tvOS) || os(watchOS)
        // 64-bit Apple platforms reserve the low 4GiB.
        if MemoryLayout<UnsafeRawPointer>.stride == 8 { value = 0x1_0000_0000 }
#endif
        output.storeBytes(of: value, toByteOffset: 0, as: UInt64.self)
        return 1

      default:
        return 0
      }
    }
  }

  func read(address: swift_addr_t, size: Int) -> UnsafeRawPointer? {
    return task_peek(task.value, address, mach_vm_size_t(size))
  }

  func getAddr(symbolName: String) -> swift_addr_t {
    // FIXME: use `__USER_LABEL_PREFIX__` instead of the hardcoded `_`.
    let fullName = "_\(symbolName)"
    var symbol = CSSymbolOwnerGetSymbolWithMangledName(swiftCore, fullName)
    if CSIsNull(symbol) {
      symbol = CSSymbolOwnerGetSymbolWithMangledName(swiftConcurrency, fullName)
    }
    let range = CSSymbolGetRange(symbol)
    return swift_addr_t(range.location)
  }

  static var Free: FreeFunction? { return nil }

  static var ReadBytes: ReadBytesFunction {
    return { (context, address, size, _) in
      let process: DarwinRemoteProcess = DarwinRemoteProcess.fromOpaque(context!)
      return process.read(address: address, size: Int(size))
    }
  }

  static var GetStringLength: GetStringLengthFunction {
    return { (context, address) in
      let process: DarwinRemoteProcess = DarwinRemoteProcess.fromOpaque(context!)
      if let str = task_peek_string(process.task.value, address) {
        return UInt64(strlen(str))
      }
      return 0
    }
  }

  static var GetSymbolAddress: GetSymbolAddressFunction {
    return { (context, symbol, length) in
      let process: DarwinRemoteProcess = DarwinRemoteProcess.fromOpaque(context!)
      guard let symbol = symbol else { return 0 }
      let name: String = symbol.withMemoryRebound(to: UInt8.self, capacity: Int(length)) {
        let buffer = UnsafeBufferPointer(start: $0, count: Int(length))
        return String(decoding: buffer, as: UTF8.self)
      }
      return process.getAddr(symbolName: name)
    }
  }

  init?(processId: ProcessIdentifier, forkCorpse: Bool) {
    processIdentifier = processId
    var processTask: task_t = task_t()
    let taskResult = task_for_pid(mach_task_self_, processId, &processTask)
    guard taskResult == KERN_SUCCESS else {
      print("unable to get task for pid \(processId): \(String(cString: mach_error_string(taskResult))) \(hex: taskResult)",
        to: &Std.err)
      return nil
    }
    self.task.value = processTask

    // Consult with VMUProcInfo to determine if we should force forkCorpse.
    let forceForkCorpse: Bool
    if let procInfoClass = getVMUProcInfoClass() {
      let procInfo = procInfoClass.init(task: self.task.value)
      forceForkCorpse = procInfo.shouldAnalyzeWithCorpse
    } else {
      // Default to not forcing forkCorpse.
      forceForkCorpse = false
    }

    if forkCorpse || forceForkCorpse {
      var corpse = task_t()
      let maxRetry = 6
      for retry in 0..<maxRetry {
        let corpseResult = task_generate_corpse(task.value, &corpse)
        if corpseResult == KERN_SUCCESS {
          task.value = corpse
          break
        }
        if corpseResult != KERN_RESOURCE_SHORTAGE || retry == maxRetry {
          print("unable to fork corpse for pid \(processId): \(String(cString: mach_error_string(corpseResult))) \(hex: corpseResult)",
            to: &Std.err)
          return nil
        }
        sleep(UInt32(1 << retry))
      }
    }

    self.symbolicator.value = CSSymbolicatorCreateWithTask(self.task.value)

    self.swiftCore = CSSymbolicatorGetSymbolOwnerWithNameAtTime(
        self.symbolicator.value, "libswiftCore.dylib", kCSNow)
    self.swiftConcurrency = CSSymbolicatorGetSymbolOwnerWithNameAtTime(
        self.symbolicator.value, "libswift_Concurrency.dylib", kCSNow)

    if CSIsNull(self.swiftCore) {
      print("pid \(processId) does not have libswiftCore.dylib loaded")
      return nil
    }

    _ = task_start_peeking(self.task.value)

    guard let context =
        swift_reflection_createReflectionContextWithDataLayout(self.toOpaqueRef(),
                                                               Self.QueryDataLayout,
                                                               Self.Free,
                                                               Self.ReadBytes,
                                                               Self.GetStringLength,
                                                               Self.GetSymbolAddress) else {
      return nil
    }
    self._context.value = context

    _ = CSSymbolicatorForeachSymbolOwnerAtTime(self.symbolicator.value, kCSNow, { owner in
      let address = CSSymbolOwnerGetBaseAddress(owner)
      _ = swift_reflection_addImage(self.context, address)
    })
  }

  func symbolicate(_ address: swift_addr_t) -> (module: String?, symbol: String?) {
    let symbol =
        CSSymbolicatorGetSymbolWithAddressAtTime(self.symbolicator.value, address, kCSNow)

    let module = CSSymbolGetSymbolOwner(symbol)
    return (CSSymbolOwnerGetName(module), CSSymbolGetName(symbol))
  }

  internal func iterateHeap(_ body: (swift_addr_t, UInt64) -> Void) {
    withoutActuallyEscaping(body) {
      withUnsafePointer(to: $0) {
        task_enumerate_malloc_blocks(self.task.value,
                                     UnsafeMutableRawPointer(mutating: $0),
                                     CUnsignedInt(MALLOC_PTR_IN_USE_RANGE_TYPE),
                                     { (task, context, type, ranges, count) in
          let callback: (swift_addr_t, UInt64) -> Void =
              context!.assumingMemoryBound(to: ((swift_addr_t, UInt64) -> Void).self).pointee
          for i in 0..<Int(count) {
            let range = ranges[i]
            callback(swift_addr_t(range.address), UInt64(range.size))
          }
        })
      }
    }
  }
}

extension DarwinRemoteProcess {
  private class PortList: Sequence {
    let buffer: UnsafeBufferPointer<mach_port_t>

    init?(task: task_t) {
      var threadList: UnsafeMutablePointer<mach_port_t>?
      var threadCount: mach_msg_type_number_t = 0

      let result = task_threads(task, &threadList, &threadCount)
      guard result == KERN_SUCCESS else {
        print("unable to gather threads for process: \(String(cString: mach_error_string(result))) (0x\(String(result, radix: 16)))")
        return nil
      }

      buffer = UnsafeBufferPointer(start: threadList, count: Int(threadCount))
    }

    deinit {
      // Deallocate the port rights for the threads.
      for thread in self {
        mach_port_deallocate(mach_task_self_, thread)
      }

      // Deallocate the thread list.
      let pointer = vm_address_t(truncatingIfNeeded: Int(bitPattern: buffer.baseAddress))
      let size = vm_size_t(MemoryLayout<mach_port_t>.size) * vm_size_t(buffer.count)

      vm_deallocate(mach_task_self_, pointer, size)
    }

    func makeIterator() -> UnsafeBufferPointer<thread_t>.Iterator {
      return buffer.makeIterator()
    }
  }

  private struct ThreadInfo {
    var threadID: UInt64
    var tlsStart: UInt64
    var kernelObject: UInt32?
  }

  private func getThreadInfos() -> [ThreadInfo] {
    guard let threads = PortList(task: self.task.value) else {
      return []
    }
    return threads.compactMap { t -> ThreadInfo? in
      guard let info = getThreadInfo(thread: t) else {
        return nil
      }
      guard let kernelObj = getKernelObject(task: mach_task_self_, port: t) else {
        return nil
      }
      return ThreadInfo(threadID: info.thread_id,
                        tlsStart: info.thread_handle,
                        kernelObject: kernelObj)
    }
  }

  private func getKernelObject(task: task_t, port: mach_port_t) -> UInt32? {
    var object: UInt32 = 0
    var type: UInt32 = 0
    let result = mach_port_kernel_object(task, port, &type, &object)
    guard result == KERN_SUCCESS else {
      return nil
    }
    return object
  }

  private func getThreadInfo(thread: thread_t) -> thread_identifier_info_data_t? {
    let THREAD_IDENTIFIER_INFO_COUNT =
        MemoryLayout<thread_identifier_info_data_t>.size / MemoryLayout<natural_t>.size
    var info = thread_identifier_info_data_t()
    var infoCount = mach_msg_type_number_t(THREAD_IDENTIFIER_INFO_COUNT)
    var result: kern_return_t = 0

    withUnsafeMutablePointer(to: &info) {
      $0.withMemoryRebound(to: integer_t.self, capacity: THREAD_IDENTIFIER_INFO_COUNT) {
        result = thread_info(thread, thread_flavor_t(THREAD_IDENTIFIER_INFO),
                        $0, &infoCount)
      }
    }
    guard result == KERN_SUCCESS else {
      print("unable to get info for thread port \(thread): \(String(cString: mach_error_string(result))) (0x\(String(result, radix: 16)))")
      return nil
    }
    return info
  }
}

extension DarwinRemoteProcess {
  internal var currentTasks: [(threadID: UInt64, currentTask: swift_addr_t)] {
    return threadInfos.compactMap {
      let tlsStart = $0.tlsStart
      if tlsStart == 0 { return nil }

      let SWIFT_CONCURRENCY_TASK_KEY = 103
      let currentTaskPointer = tlsStart + UInt64(SWIFT_CONCURRENCY_TASK_KEY * MemoryLayout<UnsafeRawPointer>.size)
      guard let pointer = read(address: currentTaskPointer, size: MemoryLayout<UnsafeRawPointer>.size) else {
        return nil
      }
      let currentTask = pointer.load(as: UInt.self)
      return (threadID: $0.threadID, currentTask: swift_addr_t(currentTask))
    }
  }

  internal func getThreadID(remotePort: thread_t) -> UInt64? {
    guard let remoteThreadObj = getKernelObject(task: self.task.value, port: remotePort) else {
      return nil
    }
    return threadInfos.first{ $0.kernelObject == remoteThreadObj }?.threadID
  }
}

#endif
