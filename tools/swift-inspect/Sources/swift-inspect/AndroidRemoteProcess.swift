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

import Foundation
import AndroidCLib
import LinuxSystemHeaders
import SwiftInspectLinux
import SwiftRemoteMirror

internal final class AndroidRemoteProcess: LinuxRemoteProcess {
  enum RemoteProcessError: Error { case missingSymbol(_ name: String) }

  let ptrace: SwiftInspectLinux.PTrace

  override init?(processId: ProcessIdentifier) {
    do {
      let ptrace = try SwiftInspectLinux.PTrace(process: processId)
      self.ptrace = ptrace
    } catch {
      print("failed initialization: \(error)")
      return nil
    }
    super.init(processId: processId)
  }

  override internal func iterateHeap(_ body: (swift_addr_t, UInt64) -> Void) {
    for entry in self.memoryMap.entries {
      guard let name = entry.pathname,
        name == "[anon:libc_malloc]" || name.hasPrefix("[anon:scudo:")
          || name.hasPrefix("[anon:GWP-ASan")
      else { continue }

      // collect all of the allocations in this heap region
      let allocations: [(base: swift_addr_t, len: UInt64)]
      do {
        allocations = try self.iterateHeapRegion(
          startAddr: entry.startAddr, endAddr: entry.endAddr)
      } catch {
        print("failed iterating remote heap: \(error)")
        return
      }

      // process all of the collected allocations
      for alloc in allocations { body(alloc.base, alloc.len) }
    }
  }

  internal func iterateHeapRegion(startAddr: UInt64, endAddr: UInt64) throws -> [(
    base: swift_addr_t, len: UInt64
  )] {
    guard let (mmapAddr, _) = symbolCache.address(of: "mmap") else {
      throw RemoteProcessError.missingSymbol("mmap")
    }

    guard let (munmapAddr, _) = symbolCache.address(of: "munmap") else {
      throw RemoteProcessError.missingSymbol("mmap")
    }

    guard let (mallocIterateAddr, _) = symbolCache.address(of: "malloc_iterate") else {
      throw RemoteProcessError.missingSymbol("malloc_iterate")
    }

    /* We allocate a page-sized buffer in the remote process that malloc_iterate
     * populates with metadata describing each heap entry it enumerates.
     *
     * The buffer is interpreted as an array of 8-byte pairs. The first pair
     * contains metadata describing the buffer itself: max valid index (e.g.
     * the size of the buffer) and next index (e.g. write cursor/position).
     * Each subsequent pair describes the address and length of a heap entry in
     * the remote process.
     *
     * ------------
     * | uint64_t | max valid index (e.g. sizeof(buffer) / sizeof(uint64_t))
     * ------------
     * | uint64_t | next free index (starts at 2)
     * ------------
     * | uint64_t | heap item 1 address
     * ------------
     * | uint64_t | heap item 1 size
     * ------------
     * | uint64_t | heap item 2 address
     * ------------
     * | uint64_t | heap item 2 size
     * ------------
     * | uint64_t | ...
     * ------------
     * | uint64_t | ...
     * ------------
     * | uint64_t | heap item N address
     * ------------
     * | uint64_t | heap item N size
     * ------------
     */
    let dataLen = UInt64(sysconf(Int32(_SC_PAGESIZE)))
    var mmapArgs = [0, dataLen, UInt64(PROT_READ | PROT_WRITE), UInt64(MAP_ANON | MAP_PRIVATE)]
    let remoteDataAddr: UInt64 = try self.ptrace.callRemoteFunction(at: mmapAddr, with: mmapArgs)
    defer {
      let munmapArgs: [UInt64] = [remoteDataAddr, dataLen]
      _ = try? self.ptrace.callRemoteFunction(at: munmapAddr, with: munmapArgs)
    }

    // initialize the metadata region in the remote process
    try self.initHeapMetadata(dataAddr: remoteDataAddr, dataLen: dataLen)

    // allocate an rwx region to hold the malloc_iterate callback that will be
    // executed in the remote process
    let codeLen = UInt64(heap_callback_len())
    mmapArgs = [
      0, codeLen, UInt64(PROT_READ | PROT_WRITE | PROT_EXEC), UInt64(MAP_ANON | MAP_PRIVATE),
    ]
    let remoteCodeAddr: UInt64 = try self.ptrace.callRemoteFunction(at: mmapAddr, with: mmapArgs)
    defer {
      let munmapArgs: [UInt64] = [remoteCodeAddr, codeLen]
      _ = try? self.ptrace.callRemoteFunction(at: munmapAddr, with: munmapArgs)
    }

    // copy the malloc_iterate callback implementation to the remote process
    let codeStart = heap_callback_start()!
    try self.process.writeMem(
      remoteAddr: remoteCodeAddr, localAddr: codeStart, len: UInt(codeLen))

    // collects metadata describing each heap allocation in the remote process
    var allocations: [(base: swift_addr_t, len: UInt64)] = []

    let regionLen = endAddr - startAddr
    let args = [startAddr, regionLen, remoteCodeAddr, remoteDataAddr]
    _ = try self.ptrace.callRemoteFunction(at: mallocIterateAddr, with: args) {
      // This callback is invoked when a SIGTRAP is encountered, indicating
      // there is no more room for heap metadata in the data buffer. Process
      // all current metadata, skip the trap/break instruction, and continue
      // iterating heap items until completion.
      allocations.append(
        contentsOf: try self.processHeapAllocations(dataAddr: remoteDataAddr, dataLen: dataLen))

      try self.initHeapMetadata(dataAddr: remoteDataAddr, dataLen: dataLen)

      var regs = try self.ptrace.getRegSet()
      regs.step(RegisterSet.trapInstructionSize)

      try self.ptrace.setRegSet(regSet: regs)
    }

    allocations.append(
      contentsOf: try self.processHeapAllocations(dataAddr: remoteDataAddr, dataLen: dataLen))

    return allocations
  }

  internal func initHeapMetadata(dataAddr: UInt64, dataLen: UInt64) throws {
    // (re-)initialize the metadata region in the remote process
    let startEntry: UInt64 = UInt64(HEAP_ITERATE_DATA_HEADER_SIZE)
    let maxEntries: UInt64 = dataLen / UInt64(MemoryLayout<UInt64>.stride)
    let header: (UInt64, UInt64) = (maxEntries, startEntry)
    let headerLen = UInt(MemoryLayout.size(ofValue: header))
    try withUnsafePointer(to: header) {
      try self.process.writeMem(remoteAddr: dataAddr, localAddr: $0, len: headerLen)
    }
  }

  internal func processHeapAllocations(dataAddr: UInt64, dataLen: UInt64) throws -> [(
    base: swift_addr_t, len: UInt64
  )] {
    let count = UInt(dataLen) / UInt(MemoryLayout<UInt64>.size)
    let data: [UInt64] = try self.process.readArray(address: dataAddr, upToCount: count)
    let startEntry = Int(HEAP_ITERATE_DATA_HEADER_SIZE)
    let entryCount = Int(data[Int(HEAP_ITERATE_DATA_NEXT_FREE_IDX)])
    var items: [(base: swift_addr_t, len: UInt64)] = []
    for idx in stride(from: startEntry, to: entryCount, by: Int(HEAP_ITERATE_DATA_ENTRY_SIZE)) {
      items.append((base: data[idx], len: data[idx + 1]))
    }

    return items
  }
}

#endif
