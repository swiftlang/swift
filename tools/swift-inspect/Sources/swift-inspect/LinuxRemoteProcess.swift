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

#if os(Linux) || os(Android)
  import Foundation
  import SwiftInspectLinux
  import SwiftRemoteMirror

  internal class LinuxRemoteProcess: RemoteProcess {
    public typealias ProcessIdentifier = pid_t
    public typealias ProcessHandle = SwiftInspectLinux.Process

    public private(set) var process: ProcessHandle
    public private(set) var context: SwiftReflectionContextRef!
    public private(set) var processIdentifier: ProcessIdentifier
    public private(set) var processName: String = "<unknown process>"

    let memoryMap: SwiftInspectLinux.MemoryMap
    let symbolCache: SwiftInspectLinux.SymbolCache

    static var QueryDataLayout: QueryDataLayoutFunction {
      return { (context, type, _, output) in
        guard let output = output else { return 0 }

        switch type {
        case DLQ_GetPointerSize:
          let size = UInt8(MemoryLayout<UnsafeRawPointer>.stride)
          output.storeBytes(of: size, toByteOffset: 0, as: UInt8.self)
          return 1

        case DLQ_GetSizeSize:
          let size = UInt8(MemoryLayout<UInt>.stride)  // UInt is word-size like size_t
          output.storeBytes(of: size, toByteOffset: 0, as: UInt8.self)
          return 1

        case DLQ_GetLeastValidPointerValue:
          let value: UInt64 = 0x1000
          output.storeBytes(of: value, toByteOffset: 0, as: UInt64.self)
          return 1

        default: return 0
        }
      }
    }

    static var Free: FreeFunction? {
      return { (_, bytes, _) in free(UnsafeMutableRawPointer(mutating: bytes)) }
    }

    static var ReadBytes: ReadBytesFunction {
      return { (context, address, size, _) in
        let process: LinuxRemoteProcess = LinuxRemoteProcess.fromOpaque(context!)

        guard
          let byteArray: [UInt8] = try? process.process.readArray(
            address: address, upToCount: UInt(size)), let buffer = malloc(byteArray.count)
        else { return nil }

        byteArray.withUnsafeBytes {
          buffer.copyMemory(from: $0.baseAddress!, byteCount: byteArray.count)
        }

        return UnsafeRawPointer(buffer)
      }
    }

    static var GetStringLength: GetStringLengthFunction {
      return { (context, address) in
        let process: LinuxRemoteProcess = LinuxRemoteProcess.fromOpaque(context!)

        // copy the string from the remote proces to get its length
        guard let bytes = try? process.process.readRawString(address: address),
          let len = UInt64(exactly: bytes.count)
        else { return 0 }
        return len
      }
    }

    static var GetSymbolAddress: GetSymbolAddressFunction {
      return { (context, symbol, length) in
        let process: LinuxRemoteProcess = LinuxRemoteProcess.fromOpaque(context!)

        guard let symbol = symbol else { return 0 }
        let name: String = symbol.withMemoryRebound(to: UInt8.self, capacity: Int(length)) {
          let buffer = UnsafeBufferPointer(start: $0, count: Int(length))
          return String(decoding: buffer, as: UTF8.self)
        }

        guard let (startAddr, _) = process.symbolCache.address(of: name) else { return 0 }
        return startAddr
      }
    }

    init?(processId: ProcessIdentifier) {
      self.processIdentifier = processId

      if let processName = SwiftInspectLinux.ProcFS.loadFileAsString(for: processId, "cmdline") {
        self.processName = processName
      }

      do {
        self.process = try SwiftInspectLinux.Process(processId)
        self.symbolCache = try SwiftInspectLinux.SymbolCache(for: process)
        self.memoryMap = try SwiftInspectLinux.MemoryMap(for: processId)
      } catch {
        fatalError("failed initialization for process \(processId): \(error)")
        return nil
      }

      guard
        let context = swift_reflection_createReflectionContextWithDataLayout(
          self.toOpaqueRef(), Self.QueryDataLayout, Self.Free, Self.ReadBytes, Self.GetStringLength,
          Self.GetSymbolAddress)
      else { return nil }
      self.context = context
    }

    func symbolicate(_ address: swift_addr_t) -> (module: String?, symbol: String?) {
      let moduleName: String?
      let symbolName: String?
      if let symbol = self.symbolCache.symbol(for: address) {
        moduleName = symbol.module
        symbolName = symbol.name
      } else if let mapEntry = memoryMap.findEntry(containing: address) {
        // found no name for the symbol, but there is a memory region containing
        // the address so use its name as the module name
        moduleName = mapEntry.pathname
        symbolName = nil
      } else {
        moduleName = nil
        symbolName = nil
      }

      // return only the basename of the module path to keep callstacks brief
      let moduleBaseName: String?
      if let filePath = moduleName, let url = URL(string: filePath) {
        moduleBaseName = url.lastPathComponent
      } else {
        moduleBaseName = moduleName
      }

      return (moduleBaseName, symbolName)
    }

    internal func iterateHeap(_ body: (swift_addr_t, UInt64) -> Void) {
      fatalError("heap iteration is not supported on Linux")
    }
  }
#endif  // os(Linux)
