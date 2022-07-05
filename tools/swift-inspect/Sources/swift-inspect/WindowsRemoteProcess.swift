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

#if os(Windows)

import WinSDK
import SwiftRemoteMirror

internal final class WindowsRemoteProcess: RemoteProcess {
  public typealias ProcessIdentifier = DWORD
  public typealias ProcessHandle = HANDLE

  public private(set) var process: ProcessHandle
  public private(set) var context: SwiftReflectionContextRef!

  private var hSwiftCore: HMODULE = HMODULE(bitPattern: -1)!

  static var QueryDataLayout: QueryDataLayoutFunction {
    return { (context, type, _, output) in
      let _ = WindowsRemoteProcess.fromOpaque(context!)

      switch type {
      case DLQ_GetPointerSize:
        let size = UInt8(MemoryLayout<UnsafeRawPointer>.stride)
        output?.storeBytes(of: size, toByteOffset: 0, as: UInt8.self)
        return 1

      case DLQ_GetSizeSize:
        // FIXME(compnerd) support 32-bit processes
        let size = UInt8(MemoryLayout<UInt64>.stride)
        output?.storeBytes(of: size, toByteOffset: 0, as: UInt8.self)
        return 1

      case DLQ_GetLeastValidPointerValue:
        let value: UInt64 = 0x1000
        output?.storeBytes(of: value, toByteOffset: 0, as: UInt64.self)
        return 1

      default:
        return 0
      }
    }
  }

  static var Free: FreeFunction {
    return { (_, bytes, _) in
      free(UnsafeMutableRawPointer(mutating: bytes))
    }
  }

  static var ReadBytes: ReadBytesFunction {
    return { (context, address, size, _) in
      let process: WindowsRemoteProcess =
          WindowsRemoteProcess.fromOpaque(context!)

      guard let buffer = malloc(Int(size)) else { return nil }
      if !ReadProcessMemory(process.process, LPVOID(bitPattern: UInt(address)),
                            buffer, size, nil) {
        free(buffer)
        return nil
      }
      return UnsafeRawPointer(buffer)
    }
  }

  static var GetStringLength: GetStringLengthFunction {
    return { (context, address) in
      let process: WindowsRemoteProcess =
          WindowsRemoteProcess.fromOpaque(context!)

      var information: WIN32_MEMORY_REGION_INFORMATION =
          WIN32_MEMORY_REGION_INFORMATION()
      if !QueryVirtualMemoryInformation(process.process,
                                        LPVOID(bitPattern: UInt(address)),
                                        MemoryRegionInfo, &information,
                                        SIZE_T(MemoryLayout.size(ofValue: information)),
                                        nil) {
        return 0
      }

      // FIXME(compnerd) mapping in the memory region from the remote process
      // would be ideal to avoid a round-trip for each byte.  This seems to work
      // well enough for now in practice, but we should fix this to provide a
      // proper remote `strlen` implementation.
      //
      // Read 64-bytes, though limit it to the size of the memory region.
      let length: Int = Int(min(UInt(information.RegionSize) - (UInt(address) - UInt(bitPattern: information.AllocationBase)), 64))
      let string: String = Array<CChar>(unsafeUninitializedCapacity: length) {
        $1 = 0
        var NumberOfBytesRead: SIZE_T = 0
        if ReadProcessMemory(process.process, LPVOID(bitPattern: UInt(address)),
                             $0.baseAddress, SIZE_T($0.count), &NumberOfBytesRead) {
          $1 = Int(NumberOfBytesRead)
        }
      }.withUnsafeBufferPointer {
        String(cString: $0.baseAddress!)
      }

      return UInt64(string.count)
    }
  }

  static var GetSymbolAddress: GetSymbolAddressFunction {
    return { (context, symbol, length) in
      let process: WindowsRemoteProcess =
          WindowsRemoteProcess.fromOpaque(context!)

      guard let symbol = symbol else { return 0 }
      let name: String = symbol.withMemoryRebound(to: UInt8.self, capacity: Int(length)) {
        let buffer = UnsafeBufferPointer(start: $0, count: Int(length))
        return String(decoding: buffer, as: UTF8.self)
      }

      return unsafeBitCast(GetProcAddress(process.hSwiftCore, name), to: swift_addr_t.self)
    }
  }

  init?(processId: ProcessIdentifier) {
    // Get process handle.
    self.process =
        OpenProcess(DWORD(PROCESS_QUERY_INFORMATION | PROCESS_VM_READ), false,
                    processId)

    // Locate swiftCore.dll in the target process
    let hSnapshot: HANDLE =
        CreateToolhelp32Snapshot(DWORD(TH32CS_SNAPMODULE), processId)
    if hSnapshot == INVALID_HANDLE_VALUE {
      // FIXME(compnerd) log error
      return nil
    }
    defer { CloseHandle(hSnapshot) }

    // Initialize SwiftReflectionContextRef
    guard let context =
        swift_reflection_createReflectionContextWithDataLayout(self.toOpaqueRef(),
                                                               Self.QueryDataLayout,
                                                               Self.Free,
                                                               Self.ReadBytes,
                                                               Self.GetStringLength,
                                                               Self.GetSymbolAddress) else {
      // FIXME(compnerd) log error
      return nil
    }
    self.context = context

    // Load modules.
    var entry: MODULEENTRY32W = MODULEENTRY32W()
    entry.dwSize = DWORD(MemoryLayout<MODULEENTRY32W>.size)

    if !Module32FirstW(hSnapshot, &entry) {
      // FIXME(compnerd) log error
      return nil
    }

    repeat {
      let module: String = withUnsafePointer(to: entry.szModule) {
        $0.withMemoryRebound(to: WCHAR.self,
                             capacity: MemoryLayout.size(ofValue: $0) / MemoryLayout<WCHAR>.size) {
          String(decodingCString: $0, as: UTF16.self)
        }
      }
      // FIXME(compnerd) support static linking at some point
      if module == "swiftCore.dll" {
        self.hSwiftCore = entry.hModule
      }
      _ = swift_reflection_addImage(context, unsafeBitCast(entry.modBaseAddr, to: swift_addr_t.self))
    } while Module32NextW(hSnapshot, &entry)

    // Initialize DbgHelp.
    if !SymInitialize(self.process, nil, true) {
      // FIXME(compnerd) log error
      return nil
    }
  }

  deinit {
    swift_reflection_destroyReflectionContext(self.context)
    _ = SymCleanup(self.process)
    _ = CloseHandle(self.process)
    self.release()
  }

  func symbolicate(_ address: swift_addr_t) -> (module: String?, symbol: String?) {
    let kMaxSymbolNameLength: Int = 1024

    let byteCount = MemoryLayout<SYMBOL_INFO>.size + kMaxSymbolNameLength + 1

    let buffer: UnsafeMutableRawPointer =
        UnsafeMutableRawPointer.allocate(byteCount: byteCount, alignment: 1)
    defer { buffer.deallocate() }

    let pSymbolInfo: UnsafeMutablePointer<SYMBOL_INFO> =
        buffer.bindMemory(to: SYMBOL_INFO.self, capacity: 1)
    pSymbolInfo.pointee.SizeOfStruct = ULONG(MemoryLayout<SYMBOL_INFO>.size)
    pSymbolInfo.pointee.MaxNameLen = ULONG(kMaxSymbolNameLength)

    guard SymFromAddr(self.process, DWORD64(address), nil, pSymbolInfo) else {
      return (nil, nil)
    }

    let symbol: String = withUnsafePointer(to: &pSymbolInfo.pointee.Name) {
      String(cString: $0)
    }

    var context: (DWORD64, String?) = (pSymbolInfo.pointee.ModBase, nil)
    _ = SymEnumerateModules64(self.process, { ModuleName, BaseOfDll, UserContext in
      let pContext: UnsafeMutablePointer<(DWORD64, String?)> =
          UserContext!.bindMemory(to: (DWORD64, String?).self, capacity: 1)

      if BaseOfDll == pContext.pointee.0 {
        pContext.pointee.1 = String(cString: ModuleName!)
        return false
      }
      return true
    }, &context)

    return (context.1, symbol)
  }

  internal func iterateHeap(_ body: (swift_addr_t, UInt64) -> Void) {
    let dwProcessId: DWORD = GetProcessId(self.process)
    if dwProcessId == 0 {
      // FIXME(compnerd) log error
      return
    }

    let hSnapshot: HANDLE =
        CreateToolhelp32Snapshot(DWORD(TH32CS_SNAPHEAPLIST), dwProcessId)
    if hSnapshot == INVALID_HANDLE_VALUE {
      // FIXME(compnerd) log error
      return
    }
    defer { CloseHandle(hSnapshot) }

    var heap: HEAPLIST32 = HEAPLIST32()
    heap.dwSize = SIZE_T(MemoryLayout<HEAPLIST32>.size)
    if !Heap32ListFirst(hSnapshot, &heap) {
      // FIXME(compnerd) log error
      return
    }

    repeat {
      var entry: HEAPENTRY32 = HEAPENTRY32()
      entry.dwSize = SIZE_T(MemoryLayout<HEAPENTRY32>.size)

      if !Heap32First(&entry, dwProcessId, heap.th32HeapID) {
        // FIXME(compnerd) log error
        continue
      }

      repeat {
        if entry.dwFlags & DWORD(LF32_FREE) == DWORD(LF32_FREE) { continue }
        body(swift_addr_t(entry.dwAddress), UInt64(entry.dwBlockSize))
      } while Heap32Next(&entry)

      heap.dwSize = SIZE_T(MemoryLayout<HEAPLIST32>.size)
    } while Heap32ListNext(hSnapshot, &heap)
  }
}

#endif
