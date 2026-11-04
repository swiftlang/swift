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
import Foundation
import SwiftInspectClientInterface

internal var WAIT_TIMEOUT_MS: DWORD {
  DWORD(SwiftInspectClientInterface.WAIT_TIMEOUT_MS)
}

internal final class WindowsRemoteProcess: RemoteProcess {
  public typealias ProcessIdentifier = DWORD
  public typealias ProcessHandle = HANDLE

  public private(set) var process: ProcessHandle
  public private(set) var context: SwiftReflectionContextRef!
  public private(set) var processIdentifier: ProcessIdentifier
  public private(set) var processName: String = "<unknown process>"

  private var hSwiftCore: HMODULE = HMODULE(bitPattern: -1)!
  private var hSwiftConcurrency: HMODULE = HMODULE(bitPattern: -1)!

  /// The reference passed to the reflection context, which the context holds
  /// until `releaseContextRef()` clears it.
  private var contextRef: OpaqueRef?

  func releaseContextRef() {
    contextRef = nil
  }

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

  static var Free: FreeFunction? {
    return { (_, bytes, _) in
      free(UnsafeMutableRawPointer(mutating: bytes))
    }
  }

  static var ReadBytes: ReadBytesFunction {
    return { (context, address, size, _) in
      let process: WindowsRemoteProcess =
        WindowsRemoteProcess.fromOpaque(context!)

      return process.read(address: address, size: Int(size))
    }
  }

  static var GetStringLength: GetStringLengthFunction {
    return { (context, address) in
      let process: WindowsRemoteProcess =
        WindowsRemoteProcess.fromOpaque(context!)

      var information: WIN32_MEMORY_REGION_INFORMATION =
        WIN32_MEMORY_REGION_INFORMATION()
      if !QueryVirtualMemoryInformation(
        process.process,
        LPVOID(bitPattern: UInt(address)),
        MemoryRegionInfo, &information,
        SIZE_T(MemoryLayout.size(ofValue: information)),
        nil)
      {
        return 0
      }

      // FIXME(compnerd) mapping in the memory region from the remote process
      // would be ideal to avoid a round-trip for each byte.  This seems to work
      // well enough for now in practice, but we should fix this to provide a
      // proper remote `strlen` implementation.
      //
      // Read 64-bytes, though limit it to the size of the memory region.
      let length: Int = Int(
        min(
          UInt(information.RegionSize)
            - (UInt(address) - UInt(bitPattern: information.AllocationBase)), 64))
      let string: String = [CChar](unsafeUninitializedCapacity: length) {
        $1 = 0
        var NumberOfBytesRead: SIZE_T = 0
        if ReadProcessMemory(
          process.process, LPVOID(bitPattern: UInt(address)),
          $0.baseAddress, SIZE_T($0.count), &NumberOfBytesRead)
        {
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

      return unsafeBitCast(GetProcAddress(process.hSwiftCore, name) ?? GetProcAddress(process.hSwiftConcurrency, name), to: swift_addr_t.self)
    }
  }

  init?(processId: ProcessIdentifier) {
    self.processIdentifier = processId
    // Get process handle.
    guard let process =
      OpenProcess(PROCESS_QUERY_INFORMATION | PROCESS_VM_READ | PROCESS_VM_WRITE | PROCESS_VM_OPERATION,
                  false, processId) else {
      return nil
    }
    self.process = process

    // Initialize SwiftReflectionContextRef
    self.contextRef = OpaqueRef(self)
    guard
      let context =
        swift_reflection_createReflectionContextWithDataLayout(
          self.contextRef?.pointer,
          Self.QueryDataLayout,
          Self.Free,
          Self.ReadBytes,
          Self.GetStringLength,
          Self.GetSymbolAddress)
    else {
      // FIXME(compnerd) log error
      return nil
    }
    self.context = context

    // Locate swiftCore.dll in the target process and load modules.
    modules(of: processId) { (entry, module) in
      // FIXME(compnerd) support static linking at some point
      if module == "swiftCore.dll" { self.hSwiftCore = entry.hModule }
      if module == "swift_Concurrency.dll" { self.hSwiftConcurrency = entry.hModule }
      _ = swift_reflection_addImage(context,
                                    unsafeBitCast(entry.modBaseAddr,
                                                  to: swift_addr_t.self))
    }
    if self.hSwiftCore == HMODULE(bitPattern: -1) || self.hSwiftConcurrency == HMODULE(bitPattern: -1) {
      // FIXME(compnerd) log error
      return nil
    }

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
  }

  func symbolicate(_ address: swift_addr_t) -> (module: String?, symbol: String?, offset: Int?) {
    // ModBase in the SYMBOL_INFOW returned from SymFromAddrW is always zero, so use SymGetModuleBase64
    let modBase = SymGetModuleBase64(self.process, DWORD64(address))
    if modBase == 0 {
      return (nil, nil, nil)
    }
  
    let kMaxSymbolNameLength: Int = 1024

    let byteCount = MemoryLayout<SYMBOL_INFOW>.size + kMaxSymbolNameLength + 1

    let buffer: UnsafeMutableRawPointer =
      UnsafeMutableRawPointer.allocate(byteCount: byteCount, alignment: 1)
    defer { buffer.deallocate() }

    let pSymbolInfo: UnsafeMutablePointer<SYMBOL_INFOW> =
      buffer.bindMemory(to: SYMBOL_INFOW.self, capacity: 1)
    pSymbolInfo.pointee.SizeOfStruct = ULONG(MemoryLayout<SYMBOL_INFOW>.size)
    pSymbolInfo.pointee.MaxNameLen = ULONG(kMaxSymbolNameLength)

    var displacement: DWORD64 = 0
    guard SymFromAddrW(self.process, DWORD64(address), &displacement, pSymbolInfo) else {
      return (nil, nil, nil)
    }

    let symbol: String = withUnsafePointer(to: &pSymbolInfo.pointee.Name) {
      String(utf16CodeUnits: $0, count: Int(pSymbolInfo.pointee.NameLen))
    }

    var context: (DWORD64, String?) = (modBase, nil)
    _ = withUnsafeMutablePointer(to: &context) {
      SymEnumerateModules64(self.process, { (ModuleName, BaseOfDll, UserContext) -> WindowsBool in
        if let pContext = UserContext?.bindMemory(to: (DWORD64, String?).self, capacity: 1) {
          if pContext.pointee.0 == BaseOfDll {
            pContext.pointee.1 = String(cString: ModuleName!)
            return false
          }
        }
        return true
      }, $0)
    }

    return (context.1, symbol, Int(displacement))
  }

  internal func iterateHeap(_ body: (swift_addr_t, UInt64) -> Void) {
    let dwProcessId: DWORD = GetProcessId(self.process)
    if dwProcessId == 0 {
      // FIXME(compnerd) log error
      return
    }

    // We use a shared memory and two event objects to send heap entries data
    // from the remote process to this process. A high-level structure looks
    // like below:
    //
    // Swift inspect (this process):
    //
    // Setup the shared memory and event objects
    // Create a remote thread to invoke the heap walk on the remote process
    // Loop {
    //   Wait on ReadEvent to wait for heap entries in the shared memory
    //   If no entries, break
    //   Inspect and dump heap entries from the shared memory
    //   Notify (SetEvent) on WriteEvent to have more heap entries written
    // }
    //
    // Remote process:
    //
    // Open the shared memory and event objects
    // Heap walk loop {
    //   Write heap entries in the shared memory until full or done
    //   Notify (SetEvent) ReadEvent to have them read
    //   Wait on WriteEvent until they are read
    // }
    //

    // Exclude the self-inspect case. We use IPC + HeapWalk in the remote
    // process, which doesn't work on itself.
    if dwProcessId == GetCurrentProcessId() {
      print("Cannot inspect the process itself")
      return
    }

    // The size of the shared memory buffer and the names of shared
    // memory and event objects
    let bufSize = Int(BUF_SIZE)
    let sharedMemoryName = "\(SHARED_MEM_NAME_PREFIX)-\(String(dwProcessId))"

    // Set up the shared memory
    let hMapFile =
        CreateFileMappingA(INVALID_HANDLE_VALUE, nil, PAGE_READWRITE, 0,
                           DWORD(bufSize), sharedMemoryName)
    if hMapFile == HANDLE(bitPattern: 0) {
      print("CreateFileMapping failed \(GetLastError())")
      return
    }
    defer { CloseHandle(hMapFile) }
    let buf: LPVOID =
        MapViewOfFile(hMapFile, FILE_MAP_ALL_ACCESS, 0, 0, SIZE_T(bufSize))
    if buf == LPVOID(bitPattern: 0) {
      print("MapViewOfFile failed \(GetLastError())")
      return
    }
    defer { UnmapViewOfFile(buf) }

    // Set up the event objects
    guard let (hReadEvent, hWriteEvent) = createEventPair(dwProcessId) else {
      return
    }
    defer {
      CloseHandle(hReadEvent)
      CloseHandle(hWriteEvent)
    }

    // Allocate the dll path string in the remote process.
    guard let dllPathRemote = allocateDllPathRemote() else {
      return
    }

    guard let aEntryPoints = find(module: "KERNEL32.DLL",
                                  symbols: ["LoadLibraryW", "FreeLibrary"],
                                  in: dwProcessId)?.map({
      unsafeBitCast($0, to: LPTHREAD_START_ROUTINE.self)
    }) else {
      print("Failed to find remote LoadLibraryW/FreeLibrary addresses")
      return
    }

    let (pfnLoadLibraryW, pfnFreeLibrary) = (aEntryPoints[0], aEntryPoints[1])
    let hThread: HANDLE = CreateRemoteThread(
      self.process, nil, 0, pfnLoadLibraryW, dllPathRemote, 0, nil)
    if hThread == HANDLE(bitPattern: 0) {
      print("CreateRemoteThread failed \(GetLastError())")
      return
    }
    defer { CloseHandle(hThread) }

    defer {
      // Always perform the code ejection process even if the heap walk fails.
      // The module cannot re-execute the heap walk and will leave a retain
      // count behind, preventing the module from being unlinked on the file
      // system as well as leave code in the inspected process.  This will
      // eventually be an issue for treating the injected code as a resource
      // which is extracted temporarily.
      if !eject(module: dllPathRemote, from: dwProcessId, pfnFreeLibrary) {
        print("Failed to unload the remote dll")
      }
    }

    // The main heap iteration loop.
    outer: while true {
      let wait = WaitForSingleObject(hReadEvent, WAIT_TIMEOUT_MS)
      if wait != WAIT_OBJECT_0 {
        print("WaitForSingleObject on ReadEvent failed \(wait)")
        return
      }

      let entryCount = bufSize / MemoryLayout<HeapEntry>.size

      for entry in UnsafeMutableBufferPointer(
        start: buf.bindMemory(
          to: HeapEntry.self,
          capacity: entryCount),
        count: entryCount)
      {
        if entry.Address == UInt.max {
          // The buffer containing zero entries, indicated by the first entry
          // contains -1, means, we are done. Break out of loop.
          if !SetEvent(hWriteEvent) {
            print("SetEvent failed: \(GetLastError())")
            return
          }
          break outer
        }
        if entry.Address == 0 {
          // Done. Break out of loop.
          break
        }
        body(swift_addr_t(entry.Address), UInt64(entry.Size))
      }

      if !SetEvent(hWriteEvent) {
        print("SetEvent failed \(GetLastError())")
        return
      }
    }

    let wait = WaitForSingleObject(hThread, WAIT_TIMEOUT_MS)
    if wait != WAIT_OBJECT_0 {
      print("WaitForSingleObject on LoadLibrary failed \(wait)")
      return
    }

    var threadExitCode: DWORD = 0
    GetExitCodeThread(hThread, &threadExitCode)
    if threadExitCode == 0 {
      print("LoadLibraryW failed \(threadExitCode)")
      return
    }
  }

  internal func iteratePotentialMetadataPages(_ body: (swift_addr_t, UInt64) -> Void) {
    fatalError("metadata page iteration is not supported on Windows")
  }

  private struct ThreadInfo {
    var threadID: UInt64
    var tlsStart: UInt64
    var tlsSize: UInt64
  }

  private lazy var threadInfos: Result<[ThreadInfo], Error> = Result { try getThreadInfos() }

  private func getThreadInfos() throws -> [ThreadInfo] {
    guard let ntdll = "ntdll.dll".withLPWSTR({ GetModuleHandleW($0) }) else {
      throw _Win32Error(functionName: "GetModuleHandleW", error: GetLastError())
    }

    guard let ntQueryInformationThread = GetProcAddress(ntdll, "NtQueryInformationThread") else {
      throw _Win32Error(functionName: "GetProcAddress", error: GetLastError())
    }

    func getTlsDirectoryIndex(module: HMODULE) throws -> (index: DWORD, size: UInt64) {
      let base = UInt64(UInt(bitPattern: module))
      let directory = try dataDirectory(IMAGE_DIRECTORY_ENTRY_TLS, of: module)
      guard directory.VirtualAddress != 0 else {
        throw InvalidPEError.emptyDataDirectory(index: Int(IMAGE_DIRECTORY_ENTRY_TLS))
      }

      let tls = try pointee(base + UInt64(directory.VirtualAddress), as: IMAGE_TLS_DIRECTORY64.self)

      // The loader initializes each thread's TLS block with the raw data
      // followed by SizeOfZeroFill zero bytes.
      let size = tls.EndAddressOfRawData - tls.StartAddressOfRawData + UInt64(tls.SizeOfZeroFill)
      return try (pointee(tls.AddressOfIndex, as: DWORD.self), size)
    }

    let (tlsIndex, tlsSize) = try getTlsDirectoryIndex(module: self.hSwiftConcurrency)

    var tasks: [ThreadInfo] = []

    try enumerateThreads(processIdentifier: self.processIdentifier, dwDesiredAccess: DWORD(THREAD_QUERY_INFORMATION)) { hThread in
      let threadBasicInformation = try THREAD_BASIC_INFORMATION(hThread, unsafeBitCast(ntQueryInformationThread, to: NtQueryInformationThreadFunction.self))

      let tlsPointer = try pointee(threadBasicInformation.TebBaseAddress_ThreadLocalStoragePointer, as: UInt64.self)
      if tlsPointer == 0 {
        return
      }

      let tlsStart = tlsPointer + UInt64(MemoryLayout<UnsafeRawPointer>.size * Int(tlsIndex))

      tasks.append(ThreadInfo(threadID: UInt64(GetThreadId(hThread)), tlsStart: tlsStart, tlsSize: tlsSize))
    }

    return tasks
  }

  private enum InvalidPEError: Error, CustomStringConvertible {
    case invalidDosSignature(WORD)
    case invalidNtSignature(DWORD)
    case missingDataDirectory(index: Int, numberOfRvaAndSizes: DWORD)
    case emptyDataDirectory(index: Int)

    var description: String {
      switch self {
      case .invalidDosSignature(let m):
        return "invalid DOS signature 0x\(String(m, radix: 16)), expected IMAGE_DOS_SIGNATURE (\"MZ\")"
      case .invalidNtSignature(let s):
        return "invalid NT signature 0x\(String(s, radix: 16)), expected IMAGE_NT_SIGNATURE (\"PE00\")"
      case .missingDataDirectory(let index, let n):
        return "PE has no data directory entry \(index) (NumberOfRvaAndSizes=\(n))"
      case .emptyDataDirectory(let index):
        return "PE data directory entry \(index) is empty (VirtualAddress=0)"
      }
    }
  }

  /// Reads the NT headers of `module`, which is loaded in the remote process.
  private func ntHeaders(of module: HMODULE) throws -> IMAGE_NT_HEADERS {
    let base = UInt64(UInt(bitPattern: module))
    let dos = try pointee(base, as: IMAGE_DOS_HEADER.self)

    guard dos.e_magic == IMAGE_DOS_SIGNATURE else {
      throw InvalidPEError.invalidDosSignature(dos.e_magic)
    }

    let nt = try pointee(base + UInt64(dos.e_lfanew), as: IMAGE_NT_HEADERS.self)

    guard nt.Signature == IMAGE_NT_SIGNATURE else {
      let err = InvalidPEError.invalidNtSignature(nt.Signature)
      warn("\(err)")
      throw err
    }

    return nt
  }

  /// Reads data directory entry `index` (an `IMAGE_DIRECTORY_ENTRY_*` value)
  /// of `module`, which is loaded in the remote process.
  private func dataDirectory<Index: BinaryInteger>(_ index: Index, of module: HMODULE) throws -> IMAGE_DATA_DIRECTORY {
    let nt = try ntHeaders(of: module)
    let index = Int(index)
    guard index < Int(nt.OptionalHeader.NumberOfRvaAndSizes) else {
      throw InvalidPEError.missingDataDirectory(
        index: index, numberOfRvaAndSizes: nt.OptionalHeader.NumberOfRvaAndSizes)
    }
    return withUnsafeBytes(of: nt.OptionalHeader.DataDirectory) {
      $0.load(fromByteOffset: index * MemoryLayout<IMAGE_DATA_DIRECTORY>.stride,
              as: IMAGE_DATA_DIRECTORY.self)
    }
  }

  /// Finds the address of the export `name` in `module` by reading the export
  /// directory from the remote process's memory.
  ///
  /// `GetProcAddress` can't be used here: it searches modules in *this*
  /// process, which might not have the same module loaded, or might have it
  /// loaded at a different base address or in a different version.
  ///
  /// - Returns: The address of the export in the remote process, or `nil` if
  ///   `module` has no such export.
  private func remoteExportAddress(of name: String, in module: HMODULE) throws -> swift_addr_t? {
    let base = UInt64(UInt(bitPattern: module))
    let directory = try dataDirectory(IMAGE_DIRECTORY_ENTRY_EXPORT, of: module)
    guard directory.VirtualAddress != 0 else { return nil }

    let exports = try readRemoteMemory(
      address: base + UInt64(directory.VirtualAddress), as: IMAGE_EXPORT_DIRECTORY.self)

    // Compares the NUL-terminated string at `address` with `target` like strcmp.
    let target = Array(name.utf8) + [0]
    func compare(remoteStringAt address: swift_addr_t) throws -> Int {
      try withRemoteMemory(address: address, size: target.count) { remote in
        for (r, t) in zip(remote, target) {
          if r != t { return r < t ? -1 : 1 }
          if r == 0 { break }
        }
        return 0
      }
    }

    // The loader requires the name pointer table to be sorted, so binary search it.
    var low = 0
    var high = Int(exports.NumberOfNames)
    while low < high {
      let mid = low + (high - low) / 2
      let nameRVA = try readRemoteMemory(
        address: base + UInt64(exports.AddressOfNames) + UInt64(mid * MemoryLayout<DWORD>.size),
        as: DWORD.self)
      let order = try compare(remoteStringAt: base + UInt64(nameRVA))
      if order < 0 {
        low = mid + 1
      } else if order > 0 {
        high = mid
      } else {
        let ordinal = try readRemoteMemory(
          address: base + UInt64(exports.AddressOfNameOrdinals) + UInt64(mid * MemoryLayout<WORD>.size),
          as: WORD.self)
        guard DWORD(ordinal) < exports.NumberOfFunctions else { return nil }
        let rva = try readRemoteMemory(
          address: base + UInt64(exports.AddressOfFunctions) + UInt64(Int(ordinal) * MemoryLayout<DWORD>.size),
          as: DWORD.self)
        // An RVA inside the export directory is a forwarder string, not an
        // address within this module.
        if rva >= directory.VirtualAddress && rva - directory.VirtualAddress < directory.Size {
          return nil
        }
        return base + UInt64(rva)
      }
    }
    return nil
  }

  private lazy var currentTaskTLSOffset: Result<Int, Error> = Result { try getCurrentTaskTLSOffset() }

  /// Returns the offset of the current-task pointer within each thread's
  /// swift_Concurrency.dll TLS block.
  private func getCurrentTaskTLSOffset() throws -> Int {
    // The runtime exports the offset, resolved by the linker from the same
    // SECREL relocation the runtime uses to access the thread-local variable.
    // See `_swift_concurrency_debug_current_task_tls_offset` in
    // stdlib/public/Concurrency/Debug.h.
    if let address = try remoteExportAddress(
        of: "_swift_concurrency_debug_current_task_tls_offset", in: hSwiftConcurrency) {
      return Int(try readRemoteMemory(address: address, as: UInt32.self))
    }

    // Older runtimes don't export the offset. In those, the current-task
    // pointer is the first variable in the TLS block after the 8-byte-aligned
    // `_tls_start` byte from the CRT, so it's at offset 8.
    return 8
  }

  internal var currentTasks: [(threadID: UInt64, currentTask: swift_addr_t)] {
    do {
      return try currentTasks(offset: currentTaskTLSOffset.get())
    } catch {
      print("ERROR: \(error)")
      return []
    }
  }

  internal func currentTasks(offset: Int) throws -> [(threadID: UInt64, currentTask: swift_addr_t)] {
    return try threadInfos.get().compactMap { threadInfo -> (threadID: UInt64, currentTask: swift_addr_t)? in
      let tlsStart = threadInfo.tlsStart
      if tlsStart == 0 { return nil }

      guard offset <= Int(threadInfo.tlsSize) - MemoryLayout<UnsafeRawPointer>.size else {
        struct RangeError: Error, CustomStringConvertible {
          let threadID: UInt64
          let offset: Int
          let size: Int
          var description: String {
            "offset \(offset) would be out of range for an \(MemoryLayout<UnsafeRawPointer>.size)-byte pointer in the \(size)-byte TLS area for thread \(threadID)"
          }
        }
        throw RangeError(threadID: threadInfo.threadID, offset: offset, size: Int(threadInfo.tlsSize))
      }

      let tlsStartBase = try pointee(tlsStart, as: UInt64.self)
      if tlsStartBase == 0 { return nil }
      
      let currentTaskPointer = tlsStartBase.advanced(by: offset)
      guard let currentTask = try? readRemoteMemory(address: currentTaskPointer, as: UInt.self) else {
        return nil
      }
      return (threadID: threadInfo.threadID, currentTask: swift_addr_t(currentTask))
    }
  }

  func pointee<T>(_ pointer: ULONG_PTR, as type: T.Type = T.self) throws -> T {
    try pointee(UnsafeRawPointer(bitPattern: UInt(pointer))!, as: type)
  }

  func pointee<T>(_ pointer: UnsafePointer<T>) throws -> T {
    try pointee(UnsafeRawPointer(pointer), as: T.self)
  }

  func pointee<T>(_ pointer: UnsafeRawPointer, as type: T.Type = T.self) throws -> T {
    try readRemoteMemory(address: swift_addr_t(UInt(bitPattern: pointer)), as: type)
  }

  func readRemoteMemory<T>(address: swift_addr_t, as type: T.Type = T.self) throws -> T {
    try withRemoteMemory(address: address, size: MemoryLayout<T>.size) { buffer in
      UnsafeRawPointer(buffer.baseAddress!).load(as: type)
    }
  }

  func withRemoteMemory<T>(address: swift_addr_t, size: Int, _ block: (UnsafeBufferPointer<UInt8>) throws -> T) throws -> T {
    try withUnsafeTemporaryAllocation(of: UInt8.self, capacity: Int(size)) { buffer in
      guard ReadProcessMemory(process, LPVOID(bitPattern: UInt(address)), buffer.baseAddress, SIZE_T(size), nil) else {
        throw _Win32Error(functionName: "ReadProcessMemory", error: GetLastError())
      }
      return try block(UnsafeBufferPointer(buffer))
    }
  }

  func read(address: swift_addr_t, size: Int) -> UnsafeRawPointer? {
    guard let buffer = malloc(Int(size)) else { return nil }
    if !ReadProcessMemory(
      process, LPVOID(bitPattern: UInt(address)),
      buffer, SIZE_T(size), nil)
    {
      free(buffer)
      return nil
    }
    return UnsafeRawPointer(buffer)
  }

  private func allocateDllPathRemote() -> UnsafeMutableRawPointer? {
    URL(fileURLWithPath: ProcessInfo.processInfo.arguments[0])
      .deletingLastPathComponent()
      .appendingPathComponent("SwiftInspectClient.dll")
      .path
      .withCString(encodedAs: UTF16.self) { pwszPath in
        let dwLength = GetFullPathNameW(pwszPath, 0, nil, nil)
        return withUnsafeTemporaryAllocation(of: WCHAR.self, capacity: Int(dwLength)) {
          guard GetFullPathNameW(pwszPath, dwLength, $0.baseAddress, nil) == dwLength - 1 else { return nil }

          var faAttributes: WIN32_FILE_ATTRIBUTE_DATA = .init()
          guard GetFileAttributesExW($0.baseAddress, GetFileExInfoStandard, &faAttributes) else {
            print("\(String(decodingCString: $0.baseAddress!, as: UTF16.self)) doesn't exist")
            return nil
          }
          guard faAttributes.dwFileAttributes & FILE_ATTRIBUTE_REPARSE_POINT == 0 else {
            print("\(String(decodingCString: $0.baseAddress!, as: UTF16.self)) doesn't exist")
            return nil
          }

          let szLength = SIZE_T(Int(dwLength) * MemoryLayout<WCHAR>.size)
          guard let pAllocation =
              VirtualAllocEx(self.process, nil, szLength, MEM_COMMIT, PAGE_READWRITE) else {
            print("VirtualAllocEx failed \(GetLastError())")
            return nil
          }

          if !WriteProcessMemory(self.process, pAllocation, $0.baseAddress, szLength, nil) {
            print("WriteProcessMemory failed \(GetLastError())")
            _ = VirtualFreeEx(self.process, pAllocation, 0, MEM_RELEASE)
            return nil
          }

          return pAllocation
        }
      }
  }

  /// Eject the injected code from the instrumented process.
  ///
  /// Performs the necessary clean up to remove the injected code from the
  /// instrumented process once the heap walk is complete.
  private func eject(module pwszModule: UnsafeMutableRawPointer,
                     from process: DWORD,
                     _ pfnFunction: LPTHREAD_START_ROUTINE) -> Bool {
    // Deallocate the dll path string in the remote process
    if !VirtualFreeEx(self.process, pwszModule, 0, MEM_RELEASE) {
      print("VirtualFreeEx failed: \(GetLastError())")
    }

    // Get the dll module handle in the remote process to use it to unload it
    // below.  `GetExitCodeThread` returns a `DWORD` (32-bit) but the `HMODULE`
    // pointer-sized and may be truncated, so, search for it using the snapshot
    // instead.
    guard let hModule = find(module: "SwiftInspectClient.dll", in: process) else {
      print("Failed to find the client dll")
      return false
    }

    // Unload the dll from the remote process
    guard let hThread = CreateRemoteThread(self.process, nil, 0, pfnFunction,
                                           hModule, 0, nil) else {
      print("CreateRemoteThread for unload failed \(GetLastError())")
      return false
    }
    defer { CloseHandle(hThread) }

    guard WaitForSingleObject(hThread, WAIT_TIMEOUT_MS) == WAIT_OBJECT_0 else {
      print("WaitForSingleObject on FreeLibrary failed \(GetLastError())")
      return false
    }

    var dwExitCode: DWORD = 0
    guard GetExitCodeThread(hThread, &dwExitCode) else {
      print("GetExitCodeThread for unload failed \(GetLastError())")
      return false
    }

    if dwExitCode == 0 {
      print("FreeLibrary failed \(dwExitCode)")
      return false
    }

    return true
  }

  private func modules(of dwProcessId: DWORD, _ closure: (MODULEENTRY32W, String) -> Void) {
    let hModuleSnapshot: HANDLE =
      CreateToolhelp32Snapshot(TH32CS_SNAPMODULE, dwProcessId)
    if hModuleSnapshot == INVALID_HANDLE_VALUE {
      print("CreateToolhelp32Snapshot failed \(GetLastError())")
      return
    }
    defer { CloseHandle(hModuleSnapshot) }
    var entry: MODULEENTRY32W = MODULEENTRY32W()
    entry.dwSize = DWORD(MemoryLayout<MODULEENTRY32W>.size)
    guard Module32FirstW(hModuleSnapshot, &entry) else {
      print("Module32FirstW failed \(GetLastError())")
      return
    }
    repeat {
      let module: String = withUnsafePointer(to: entry.szModule) {
        $0.withMemoryRebound(
          to: WCHAR.self,
          capacity: MemoryLayout.size(ofValue: $0) / MemoryLayout<WCHAR>.size
        ) {
          String(decodingCString: $0, as: UTF16.self)
        }
      }
      closure(entry, module)
    } while Module32NextW(hModuleSnapshot, &entry)
  }

  private func find(module named: String, in dwProcessId: DWORD) -> HMODULE? {
    var hModule: HMODULE?
    modules(of: dwProcessId) { (entry, module) in
      if module == named { hModule = entry.hModule }
    }
    return hModule
  }

  private func find(module: String, symbols: [String], in process: DWORD) -> [FARPROC]? {
    guard let hModule = find(module: module, in: process) else {
      print("Failed to find remote module \(module)")
      return nil
    }
    return symbols.compactMap { symbol in
      guard let addr = GetProcAddress(hModule, symbol) else {
        print("Failed to find address for symbol \(symbol) in module \(module)")
        return nil
      }
      return addr
    }
  }

  private func createEventPair(_ dwProcessId: DWORD) -> (HANDLE, HANDLE)? {
    let hReadEvent = CreateEvent("\(READ_EVENT_NAME_PREFIX)-\(dwProcessId)")
    guard let hReadEvent else { return nil }
    let hWriteEvent = CreateEvent("\(WRITE_EVENT_NAME_PREFIX)-\(dwProcessId)")
    guard let hWriteEvent else { CloseHandle(hReadEvent);  return nil }
    return (hReadEvent, hWriteEvent)
  }
}

fileprivate let ThreadBasicInformation: CInt = 0

fileprivate struct CLIENT_ID {
  var UniqueProcess: HANDLE = INVALID_HANDLE_VALUE
  var UniqueThread: HANDLE = INVALID_HANDLE_VALUE
}

fileprivate struct THREAD_BASIC_INFORMATION {
  var ExitStatus: NTSTATUS = 0
  var TebBaseAddress: ULONG_PTR = 0
  var ClientId: CLIENT_ID = .init()
  var AffinityMask: ULONG_PTR = 0
  var Priority: LONG = 0
  var BasePriority: LONG = 0
}

// https://learn.microsoft.com/en-us/windows/win32/api/winternl/nf-winternl-ntqueryinformationthread
fileprivate typealias NtQueryInformationThreadFunction = @convention(c) (_ ThreadHandle: HANDLE, _ ThreadInformationClass: CInt, _ ThreadInformation: PVOID, _ ThreadInformationLength: ULONG, _ ReturnLength: PULONG) -> NTSTATUS

extension THREAD_BASIC_INFORMATION {
  fileprivate init(_ hThread: HANDLE, _ NtQueryInformation: NtQueryInformationThreadFunction) throws {
    self.init()

    let threadBasicInformationSize = MemoryLayout.size(ofValue: self)
    #if arch(x86_64) || arch(arm64)
    precondition(threadBasicInformationSize == 48)
    #elseif arch(i386) || arch(arm)
    precondition(threadBasicInformationSize == 24)
    #else
    #error("Unsupported architecture")
    #endif

    var len: ULONG = 0
    guard NtQueryInformation(hThread, ThreadBasicInformation, &self, ULONG(threadBasicInformationSize), &len) == 0 else {
      throw _Win32Error(functionName: "NtQueryInformation", error: GetLastError())
    }
  }

  fileprivate var TebBaseAddress_ThreadLocalStoragePointer: ULONG_PTR {
    #if arch(x86_64) || arch(arm64)
    TebBaseAddress + 0x58 // https://github.com/wine-mirror/wine/blob/e1af2ae201c9853133ef3af1dafe15fe992fed92/include/winternl.h#L511 (undocumented officially)
    #elseif arch(i386) || arch(arm)
    TebBaseAddress + 0x2c // https://github.com/wine-mirror/wine/blob/e1af2ae201c9853133ef3af1dafe15fe992fed92/include/winternl.h#L511 (undocumented officially)
    #else
    #error("Unsupported architecture")
    #endif
  }
}

fileprivate struct _Win32Error: Error {
    let functionName: String
    let error: DWORD
}

extension String {
  fileprivate func withLPWSTR<T>(_ body: (UnsafeMutablePointer<WCHAR>) throws -> T) rethrows -> T {
    try withUnsafeTemporaryAllocation(of: WCHAR.self, capacity: self.utf16.count + 1, { outBuffer in
      try self.withCString(encodedAs: UTF16.self) { inBuffer in
        outBuffer.baseAddress!.initialize(from: inBuffer, count: self.utf16.count)
        outBuffer[outBuffer.count - 1] = 0
        return try body(outBuffer.baseAddress!)
      }
    })
  }
}

func enumerateThreads(processIdentifier: DWORD, dwDesiredAccess: DWORD, _ block: (HANDLE) throws -> ()) throws {
  let hThreadSnap = CreateToolhelp32Snapshot(TH32CS_SNAPTHREAD, 0)
  if hThreadSnap == INVALID_HANDLE_VALUE {
    throw _Win32Error(functionName: "CreateToolhelp32Snapshot", error: GetLastError())
  }

  defer { CloseHandle(hThreadSnap) }

  var te32 = THREADENTRY32()
  te32.dwSize = DWORD(MemoryLayout.size(ofValue: te32))

  // Retrieve information about the first thread
  if !Thread32First(hThreadSnap, &te32) {
    throw _Win32Error(functionName: "Thread32First", error: GetLastError())
  }

  // Now walk the thread list of the system
  repeat {
    if te32.th32OwnerProcessID == processIdentifier {
      let tid = te32.th32ThreadID
      guard let hThread = OpenThread(dwDesiredAccess, false, tid) else {
        throw _Win32Error(functionName: "OpenThread", error: GetLastError())
      }

      defer { CloseHandle(hThread) }

      try block(hThread)
    }
  } while Thread32Next(hThreadSnap, &te32)
}

#endif
