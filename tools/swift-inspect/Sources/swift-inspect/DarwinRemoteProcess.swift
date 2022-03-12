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

  private var task: task_t

  public var process: ProcessHandle { task }
  public private(set) var context: SwiftReflectionContextRef!
  private var symbolicator: CSSymbolicatorRef

  private var swiftCore: CSTypeRef
  private let swiftConcurrency: CSTypeRef

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
    return task_peek(task, address, mach_vm_size_t(size))
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

  static var ReadBytes: ReadBytesFunction {
    return { (context, address, size, _) in
      let process: DarwinRemoteProcess = DarwinRemoteProcess.fromOpaque(context!)
      return process.read(address: address, size: Int(size))
    }
  }

  static var GetStringLength: GetStringLengthFunction {
    return { (context, address) in
      let process: DarwinRemoteProcess = DarwinRemoteProcess.fromOpaque(context!)
      if let str = task_peek_string(process.task, address) {
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

  init?(processId: ProcessIdentifier) {
    var task: task_t = task_t()
    let result = task_for_pid(mach_task_self_, processId, &task)
    guard result == KERN_SUCCESS else {
      print("unable to get task for pid \(processId): \(String(cString: mach_error_string(result))) (0x\(String(result, radix: 16)))")
      return nil
    }
    self.task = task

    self.symbolicator = CSSymbolicatorCreateWithTask(self.task)
    self.swiftCore =
        CSSymbolicatorGetSymbolOwnerWithNameAtTime(self.symbolicator,
                                                   "libswiftCore.dylib", kCSNow)
    self.swiftConcurrency = CSSymbolicatorGetSymbolOwnerWithNameAtTime(
      symbolicator, "libswift_Concurrency.dylib", kCSNow)
    _ = task_start_peeking(self.task)

    guard let context =
        swift_reflection_createReflectionContextWithDataLayout(self.toOpaqueRef(),
                                                               Self.QueryDataLayout,
                                                               Self.Free,
                                                               Self.ReadBytes,
                                                               Self.GetStringLength,
                                                               Self.GetSymbolAddress) else {
      return nil
    }
    self.context = context

    _ = CSSymbolicatorForeachSymbolOwnerAtTime(self.symbolicator, kCSNow, { owner in
      let address = CSSymbolOwnerGetBaseAddress(owner)
      _ = swift_reflection_addImage(self.context, address)
    })
  }

  deinit {
    task_stop_peeking(self.task)
    mach_port_deallocate(mach_task_self_, self.task)
  }

  func symbolicate(_ address: swift_addr_t) -> (module: String?, symbol: String?) {
    let symbol =
        CSSymbolicatorGetSymbolWithAddressAtTime(self.symbolicator, address, kCSNow)

    let module = CSSymbolGetSymbolOwner(symbol)
    return (CSSymbolOwnerGetName(module), CSSymbolGetName(symbol))
  }

  internal func iterateHeap(_ body: (swift_addr_t, UInt64) -> Void) {
    withoutActuallyEscaping(body) {
      withUnsafePointer(to: $0) {
        task_enumerate_malloc_blocks(self.task,
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
  internal var currentTasks: [(threadID: UInt64, currentTask: swift_addr_t)] {
    var threadList: UnsafeMutablePointer<thread_t>?
    var threadCount: mach_msg_type_number_t = 0

    let result = task_threads(self.task, &threadList, &threadCount)
    guard result == KERN_SUCCESS else {
      print("unable to gather threads for process: \(String(cString: mach_error_string(result))) (0x\(String(result, radix: 16)))")
      return []
    }

    defer {
      // Deallocate the port rights for the threads.
      for i in 0 ..< Int(threadCount) {
        mach_port_deallocate(mach_task_self_, threadList![i])
      }

      // Deallocate the thread list.
      let pointer = vm_address_t(truncatingIfNeeded: Int(bitPattern: threadList))
      let size = vm_size_t(MemoryLayout<thread_t>.size) * vm_size_t(threadCount)

      vm_deallocate(mach_task_self_, pointer, size)
    }

    var results: [(threadID: UInt64, currentTask: swift_addr_t)] = []
    for i in 0 ..< Int(threadCount) {
      let THREAD_IDENTIFIER_INFO_COUNT =
          MemoryLayout<thread_identifier_info_data_t>.size / MemoryLayout<natural_t>.size
      var info = thread_identifier_info_data_t()
      var infoCount = mach_msg_type_number_t(THREAD_IDENTIFIER_INFO_COUNT)

      withUnsafeMutablePointer(to: &info) {
        $0.withMemoryRebound(to: integer_t.self, capacity: THREAD_IDENTIFIER_INFO_COUNT) {
          let result =
              thread_info(threadList![i], thread_flavor_t(THREAD_IDENTIFIER_INFO),
                          $0, &infoCount)
          guard result == KERN_SUCCESS else {
            print("unable to get info for thread \(i): \(String(cString: mach_error_string(result))) (0x\(String(result, radix: 16)))")
            return
          }
        }
      }

      let tlsStart = info.thread_handle
      if tlsStart == 0 { continue }

      let SWIFT_CONCURRENCY_TASK_KEY = 103
      let currentTaskPointer = tlsStart + UInt64(SWIFT_CONCURRENCY_TASK_KEY * MemoryLayout<UnsafeRawPointer>.size)
      if let pointer = read(address: currentTaskPointer, size: MemoryLayout<UnsafeRawPointer>.size) {
        let currentTask = pointer.load(as: UInt.self)
        results.append((threadID: info.thread_id, currentTask: swift_addr_t(currentTask)))
      }
    }
    return results
  }
}

#endif
