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
  #if canImport(FoundationEssentials)
  import FoundationEssentials
  #else
  import Foundation
  #endif
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

    /// A symbol resolved (lazily) against the target process's symbol cache.
    /// `addr` is `nil` when the symbol isn't found.
    struct RemoteSymbol {
      let addr: UInt64?
      let name: String
      init(_ name: String, _ symbolCache: SwiftInspectLinux.SymbolCache) {
        self.name = name
        if let symbolRange = symbolCache.address(of: name) {
          self.addr = symbolRange.start
        } else {
          self.addr = nil
        }
      }
    }

    lazy var swiftTaskGetCurrentSymbol: RemoteSymbol =
      RemoteSymbol("swift_task_getCurrent", self.symbolCache)

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
        if case SwiftInspectLinux.Process.ProcessError.processVmReadFailure = error {
          Self.warnAboutMissingPtraceCapabilityOnce()
        }
        warn("failed to attach to process \(processId): \(error)")
        return nil
      }

      guard
        let context = swift_reflection_createReflectionContextWithDataLayout(
          self.toOpaqueRef(), Self.QueryDataLayout, Self.Free, Self.ReadBytes, Self.GetStringLength,
          Self.GetSymbolAddress)
      else { return nil }
      self.context = context
    }

    func symbolicate(_ address: swift_addr_t) -> (module: String?, symbol: String?, offset: Int?) {
      let moduleName: String?
      let symbolName: String?
      let symbolOffset: Int?
      if let symbol = self.symbolCache.symbol(for: address) {
        moduleName = symbol.module
        symbolName = symbol.name
        symbolOffset = Int(address &- symbol.start)
      } else if let mapEntry = memoryMap.findEntry(containing: address) {
        // found no name for the symbol, but there is a memory region containing
        // the address so use its name as the module name
        moduleName = mapEntry.pathname
        symbolName = nil
        symbolOffset = Int(address &- mapEntry.startAddr)
      } else {
        moduleName = nil
        symbolName = nil
        symbolOffset = nil
      }

      // return only the basename of the module path to keep callstacks brief
      let moduleBaseName: String?
      if let filePath = moduleName, let url = URL(string: filePath) {
        moduleBaseName = url.lastPathComponent
      } else {
        moduleBaseName = moduleName
      }

      return (moduleBaseName, symbolName, symbolOffset)
    }

    internal func iterateHeap(_ body: (swift_addr_t, UInt64) -> Void) {
      // Conservative, pointer-aligned scan of every anonymous, readable +
      // writable, private mapping in the target's address space. 
      // 
      // There is no glibc API equivalent to Darwin's `task_enumerate_malloc_blocks`
      // or Android's `malloc_iterate`, so we cannot ask the allocator for
      // the live-allocation list like those platforms do.
      //
      // Instead, we scan the entire heap where Swift heap objects might possibly be located.
      //
      // Cheap pre-filter: read each region locally in 1 MiB chunks, and
      // only pass through those where the first word looks like a userspace
      // pointer (>= 0x1000 - all metadata pointers live well above).
      // This drops body() calls for words holding 0, small ints, or other
      // non-pointer payloads.
      let pointerSize = MemoryLayout<UInt>.size
      let chunkBytes: UInt64 = 1 << 20  // 1 MiB
      let leastValid: UInt = 0x1000

      for entry in memoryMap.entries where entry.isLikelyHeapRegion() {
        var addr = entry.startAddr
        while addr < entry.endAddr {
          let toRead = min(chunkBytes, entry.endAddr - addr)
          guard
            let words: [UInt] = try? process.readArray(
              address: addr, upToCount: UInt(toRead) / UInt(pointerSize)),
            !words.isEmpty
          else {
            // The mapping vanished or contains a hole we can't read, move on.
            break
          }
          for (i, firstWord) in words.enumerated() {
            // The word *value* is what its metadata pointer would be.
            guard firstWord >= leastValid else {
              // The word definitely won't contain a metadata pointer
              continue
            }

            // There's chance this contains a metadata pointer.
            let candidateObject = addr + UInt64(i * pointerSize)
            body(swift_addr_t(candidateObject), 0)
          }
          addr += UInt64(words.count * pointerSize)
        }
      }
    }

    internal func iteratePotentialMetadataPages(_ body: (swift_addr_t, UInt64) -> Void) {
      func readWord(_ name: String) -> UInt? {
        guard let (addr, _) = symbolCache.address(of: name),
              let bytes: [UInt] = try? process.readArray(address: addr, upToCount: 1)
        else { return nil }
        return bytes.first
      }
      guard let initialPoolPointer = readWord("_swift_debug_allocationPoolPointer"),
            let initialPoolSize = readWord("_swift_debug_allocationPoolSize")
      else { return }
      body(swift_addr_t(initialPoolPointer), UInt64(initialPoolSize))
    }

    var currentTasks: [(threadID: UInt64, currentTask: swift_addr_t)] {
      guard let getCurrentAddr = swiftTaskGetCurrentSymbol.addr else {
        return []
      }

      // ptrace.jump which we're about to use needs PTRACE_ATTACH.
      // That requires either CAP_SYS_PTRACE, OR ptrace_scope=0 (which
      // makes same-UID enough) -- if we don't have either, bail out.
      guard Self.hasPtraceCapability || Self.yamaPtraceScope == 0 else {
        Self.warnAboutMissingPtraceCapabilityOnce()
        return []
      }

      // Each kernel thread of the target has its own directory under
      // /proc/<pid>/task/<tid>. Linux ptrace operates per-thread, and a tid
      // can be passed everywhere a `pid_t` is accepted.
      let taskDir = "/proc/\(self.processIdentifier)/task"
      guard let entries = try? FileManager.default.contentsOfDirectory(atPath: taskDir) else {
        return []
      }
      let tids = entries.compactMap { pid_t($0) }

      var result: [(threadID: UInt64, currentTask: swift_addr_t)] = []
      for tid in tids {
        do {
          // We ptrace the thread and invoke `swift_task_getCurrent`
          // to easily get the "current" task executing on it.
          try withPTracedProcess(pid: tid) { ptrace in
            let value = try ptrace.jump(to: getCurrentAddr)
            result.append((threadID: UInt64(tid), currentTask: swift_addr_t(value)))
          }
        } catch {
          // Best-effort: a thread may be exiting, may already be ptraced
          // (e.g. by a debugger), or may be in an uninterruptible state.
          // Skip it rather than fail the whole dump.
          continue
        }
      }
      return result
    }

    /// True iff this swift-inspect process has CAP_SYS_PTRACE capability.
    static let hasPtraceCapability: Bool = {
      guard let status = try? String(
        contentsOfFile: "/proc/self/status", encoding: .utf8)
      else { return false }
      for line in status.split(whereSeparator: { $0.isNewline }) {
        let CapEff = "CapEff:"
        guard line.hasPrefix(CapEff) else { continue }

        let hex = line.dropFirst(CapEff.count)
          .trimmingCharacters(in: .whitespaces)
        guard let mask = UInt64(hex, radix: 16) else { return false }

        // CAP_SYS_PTRACE is 19.
        return (mask & (UInt64(1) << 19)) != 0
      }
      return false
    }()

    /// `kernel.yama.ptrace_scope`:
    ///   - 0 = anyone same-UID,
    ///   - 1 = ancestors only,
    ///   - 2 = CAP_SYS_PTRACE only,
    ///   - 3 = ptrace disabled.
    /// Defaults to 1 on most distributions.
    static let yamaPtraceScope: Int? = {
      guard let raw = try? String(
        contentsOfFile: "/proc/sys/kernel/yama/ptrace_scope", encoding: .utf8)
      else { return nil }
      return Int(raw.trimmingCharacters(in: .whitespacesAndNewlines))
    }()

    /// Access this property to print the missing-capability warning exactly once.
    /// Any use of process_vm_readv or ptrace needs these permissions.
    private static let _warnAboutMissingPtraceCapabilityOnce: Void = {
      let cap = hasPtraceCapability ? "yes" : "no"
      let yama = yamaPtraceScope.map { String($0) } ?? "?"
      warn("""
        cannot inspect target - insufficient ptrace permission.
          CAP_SYS_PTRACE: \(cap)
          kernel.yama.ptrace_scope: \(yama)  (0=any same-UID, 1=ancestors only, 2=CAP_SYS_PTRACE only)

        To grant necessary permissions, you can:
          * run swift-inspect as root, or via sudo
          * sudo setcap cap_sys_ptrace+ep <path-to-swift-inspect>
          * sudo sysctl -w kernel.yama.ptrace_scope=0 (not recommended, system-wide)

        When using containers, you may need to pass:
          --cap-add=SYS_PTRACE --security-opt seccomp=unconfined
        and run as root inside the container, OR grant file caps with
        setcap. (Docker's --cap-add only adds to the bounding/permitted
        sets; effective caps for non-root container UIDs need file caps.)
        """)
    }()

    private static func warnAboutMissingPtraceCapabilityOnce() {
      _ = _warnAboutMissingPtraceCapabilityOnce
    }
  }

  extension SwiftInspectLinux.MemoryMap.Entry {
    /// Heuristic for "this region might contain Swift heap objects".
    ///
    /// We require `r` and `w` permissions and `p` (private). Heap pages are
    /// never executable on modern Linux, but we don't filter `x` away here -
    /// some allocators (jemalloc with sampling, etc.) may legitimately
    /// mmap rwx pages, and in the conservative-scan model an extra region
    /// just adds cost, not incorrect results.
    public func isLikelyHeapRegion() -> Bool {
      // Permissions string is "rwxp" / "rw-p" / etc.
      guard permissions.count >= 4 else {
        return false
      }

      let i = permissions.startIndex
      guard permissions[i] == "r",
            permissions[permissions.index(after: i)] == "w",
            permissions[permissions.index(i, offsetBy: 3)] == "p"
      else { return false }

      switch pathname {
      case nil:
        // Pure anonymous mapping - likely allocator-backed.
        return true
      case "[heap]"?:
        return true
      case let p? where p.hasPrefix("[anon:"):
        // glibc-named anonymous arenas, e.g. jemalloc.
        return true
      default:
        // Ignore other kinds, Swift heap objects won't be in them.
        return false
      }
    }
  }
#endif  // os(Linux)
