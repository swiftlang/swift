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

#if os(Android)

import AndroidCLib
import Foundation
import LinuxSystemHeaders
import SwiftInspectLinux
import SwiftRemoteMirror

extension MemoryMap.Entry {
  public func isHeapRegion() -> Bool {
    guard let name = self.pathname else { return false }
    // The heap region naming convention is found in AOSP's libmemunreachable at
    // android/platform/system/memory/libmemunreachable/MemUnreachable.cpp.
    if name == "[anon:libc_malloc]" { return true }
    if name.hasPrefix("[anon:scudo:") { return true }
    if name.hasPrefix("[anon:GWP-ASan") { return true }
    return false
  }
}

internal final class AndroidRemoteProcess: LinuxRemoteProcess {
  enum RemoteProcessError: Error {
    case missingSymbol(_ name: String)
    case heapIterationFailed
  }

  struct RemoteSymbol {
    let addr: UInt64?
    let name: String
    init(_ name: String, _ symbolCache: SymbolCache) {
      self.name = name
      if let symbolRange = symbolCache.address(of: name) {
        self.addr = symbolRange.start
      } else {
        self.addr = nil
      }
    }
  }

  // We call mmap/munmap in the remote process to alloc/free memory for our own
  // use without impacting existing allocations in the remote process.
  lazy var mmapSymbol: RemoteSymbol = RemoteSymbol("mmap", self.symbolCache)
  lazy var munmapSymbol: RemoteSymbol = RemoteSymbol("munmap", self.symbolCache)

  // We call malloc_iterate in the remote process to enumerate all items in the
  // remote process' heap. We use malloc_disable/malloc_enable to ensure no
  // malloc/free requests can race with malloc_iterate.
  lazy var mallocDisableSymbol: RemoteSymbol = RemoteSymbol("malloc_disable", self.symbolCache)
  lazy var mallocEnableSymbol: RemoteSymbol = RemoteSymbol("malloc_enable", self.symbolCache)
  lazy var mallocIterateSymbol: RemoteSymbol = RemoteSymbol("malloc_iterate", self.symbolCache)

  // Linux and Android have no supported method to enumerate allocations in the
  // heap of a remote process. Android does, however, support the malloc_iterate
  // API, which enumerates allocations in the current process. We leverage this
  // API by invoking it in the remote process with ptrace and using simple IPC
  // (SIGTRAP and process_vm_readv and process_vm_writev) to fetch the results.
  override internal func iterateHeap(_ body: (swift_addr_t, UInt64) -> Void) {
    var regionCount = 0
    var allocCount = 0
    do {
      try withPTracedProcess(pid: self.processIdentifier) { ptrace in
        for entry in self.memoryMap.entries {
          // Limiting malloc_iterate calls to only memory regions that are known
          // to contain heap allocations is not strictly necessary but it does
          // significantly improve the speed of heap iteration.
          guard entry.isHeapRegion() else { continue }

          // collect all of the allocations in this heap region
          let allocations: [(base: swift_addr_t, len: UInt64)]
          allocations = try self.iterateHeapRegion(ptrace, region: entry)
          regionCount += 1
          allocCount += allocations.count

          // process all of the collected allocations
          for alloc in allocations { body(alloc.base, alloc.len) }
        }
      }
    } catch {
      print("failed iterating remote heap: \(error)")
      return
    }

    if regionCount == 0 {
      // This condition most likely indicates the MemoryMap.Entry.isHeapRegion
      // filtering is needs to be modified to support a new heap region naming
      // convention in a newer Android version.
      print("WARNING: no heap regions found")
      print("swift-inspect may need to be updated for a newer Android version")
    } else if allocCount == 0 {
      print("WARNING: no heap items enumerated")
    }
  }

  // Iterate a single heap region in the remote process and return an array
  // of (base, len) pairs describing each heap allocation in the region.
  internal func iterateHeapRegion(_ ptrace: borrowing PTrace, region: MemoryMap.Entry) throws -> [(
    base: swift_addr_t, len: UInt64
  )] {
    // Allocate a page-sized buffer in the remote process that malloc_iterate
    // will populaate with metadata describing each heap entry it enumerates.
    let dataLen = sysconf(Int32(_SC_PAGESIZE))
    let remoteDataAddr = try self.mmapRemote(
      ptrace, len: dataLen, prot: PROT_READ | PROT_WRITE, flags: MAP_ANON | MAP_PRIVATE)
    defer {
      _ = try? self.munmapRemote(ptrace, addr: remoteDataAddr, len: dataLen)
    }

    // Allocate and inialize a local buffer that will be used to copy metadata
    // to/from the target process.
    let buffer = UnsafeMutableRawPointer.allocate(
      byteCount: dataLen, alignment: MemoryLayout<UInt64>.alignment)
    defer { buffer.deallocate() }
    guard heap_iterate_metadata_init(buffer, dataLen) else {
      throw RemoteProcessError.heapIterationFailed
    }
    try self.process.writeMem(remoteAddr: remoteDataAddr, localAddr: buffer, len: UInt(dataLen))

    // Allocate an rwx region to hold the malloc_iterate callback that will be
    // executed in the remote process.
    let codeLen = heap_iterate_callback_len()
    let remoteCodeAddr = try mmapRemote(
      ptrace, len: codeLen, prot: PROT_READ | PROT_WRITE | PROT_EXEC, flags: MAP_ANON | MAP_PRIVATE)
    defer {
      _ = try? self.munmapRemote(ptrace, addr: remoteCodeAddr, len: codeLen)
    }

    // Copy the malloc_iterate callback implementation to the remote process.
    let codeStart = heap_iterate_callback_start()!
    try self.process.writeMem(
      remoteAddr: remoteCodeAddr, localAddr: codeStart, len: UInt(codeLen))

    guard let mallocIterateAddr = self.mallocIterateSymbol.addr else {
      throw RemoteProcessError.missingSymbol(self.mallocIterateSymbol.name)
    }

    // Disable malloc/free while enumerating the region to get a consistent
    // snapshot of existing allocations.
    try self.mallocDisableRemote(ptrace)
    defer {
      _ = try? self.mallocEnableRemote(ptrace)
    }

    // Collects (base, len) pairs describing each heap allocation in the remote
    // process.
    var allocations: [(base: swift_addr_t, len: UInt64)] = []

    let regionLen = region.endAddr - region.startAddr
    let args = [region.startAddr, regionLen, remoteCodeAddr, remoteDataAddr]
    _ = try ptrace.jump(to: mallocIterateAddr, with: args) { ptrace in
      // This callback is invoked when a SIGTRAP is encountered in the remote
      // process. In this context, this signal indicates there is no more room
      // in the allocated metadata region (see AndroidCLib/heap.c).
      // Immediately read the heap metadata from the remote process, skip past
      // the trap/break instruction, and resume the remote process.
      try self.process.readMem(remoteAddr: remoteDataAddr, localAddr: buffer, len: UInt(dataLen))
      allocations.append(contentsOf: try self.processHeapMetadata(buffer: buffer, len: dataLen))

      guard heap_iterate_metadata_init(buffer, dataLen) else {
        throw RemoteProcessError.heapIterationFailed
      }
      try self.process.writeMem(remoteAddr: remoteDataAddr, localAddr: buffer, len: UInt(dataLen))

      var regs = try ptrace.getRegSet()
      regs.step(RegisterSet.trapInstructionSize)

      try ptrace.setRegSet(regSet: regs)
      try ptrace.cont()
    }

    try self.process.readMem(remoteAddr: remoteDataAddr, localAddr: buffer, len: UInt(dataLen))
    allocations.append(contentsOf: try self.processHeapMetadata(buffer: buffer, len: dataLen))

    return allocations
  }

  // Process heap metadata generated by our malloc_iterate callback in the
  // remote process and return an array of (base, len) pairs describing each
  // heap allocation.
  internal func processHeapMetadata(buffer: UnsafeMutableRawPointer, len: Int) throws -> [(
    base: UInt64, len: UInt64
  )] {
    let callback: @convention(c) (UnsafeMutableRawPointer?, UInt64, UInt64) -> Void = {
      let allocationsPointer = $0!.assumingMemoryBound(to: [(UInt64, UInt64)].self)
      allocationsPointer.pointee.append(($1, $2))
    }

    var allocations: [(UInt64, UInt64)] = []
    try withUnsafeMutablePointer(to: &allocations) {
      let context = UnsafeMutableRawPointer($0)
      if !heap_iterate_metadata_process(buffer, Int(len), context, callback) {
        throw RemoteProcessError.heapIterationFailed
      }
    }

    return allocations
  }

  // call mmap in the remote process with the provided arguments
  internal func mmapRemote(_ ptrace: borrowing PTrace, len: Int, prot: Int32, flags: Int32) throws
    -> UInt64
  {
    guard let sym = self.mmapSymbol.addr else {
      throw RemoteProcessError.missingSymbol(self.mmapSymbol.name)
    }
    let args = [0, UInt64(len), UInt64(prot), UInt64(flags)]
    return try ptrace.jump(to: sym, with: args)
  }

  // call munmap in the remote process with the provdied arguments
  internal func munmapRemote(_ ptrace: borrowing PTrace, addr: UInt64, len: Int) throws -> UInt64 {
    guard let sym = self.munmapSymbol.addr else {
      throw RemoteProcessError.missingSymbol(self.munmapSymbol.name)
    }
    let args: [UInt64] = [addr, UInt64(len)]
    return try ptrace.jump(to: sym, with: args)
  }

  // call malloc_disable in the remote process
  internal func mallocDisableRemote(_ ptrace: borrowing PTrace) throws {
    guard let sym = self.mallocDisableSymbol.addr else {
      throw RemoteProcessError.missingSymbol(self.mallocDisableSymbol.name)
    }
    _ = try ptrace.jump(to: sym)
  }

  // call malloc_enable in the remote process
  internal func mallocEnableRemote(_ ptrace: borrowing PTrace) throws {
    guard let sym = self.mallocEnableSymbol.addr else {
      throw RemoteProcessError.missingSymbol(self.mallocEnableSymbol.name)
    }
    _ = try ptrace.jump(to: sym)
  }
}

#endif
