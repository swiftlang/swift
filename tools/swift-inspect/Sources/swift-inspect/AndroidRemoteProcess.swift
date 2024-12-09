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
  enum RemoteProcessError: Error {
    case missingSymbol(_ name: String)
    case heapIterationFailed
  }

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
      guard entry.isHeapRegion() else { continue }

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

  // Linux and Android have no supported method to enumerate allocations in the
  // heap of a remote process. Android does, however, support the malloc_iterate
  // API, which enumerates allocations in the current process. We leverage this
  // API by invoking it in the remote process using ptrace and using simple IPC
  // (SIGTRAP and process_vm_readv and process_vm_writev).
  internal func iterateHeapRegion(startAddr: UInt64, endAddr: UInt64) throws -> [(
    base: swift_addr_t, len: UInt64
  )] {
    // We call mmap/munmap in the remote process to alloc/free memory for our
    // own use without impacting existing allocations in the remote process.
    guard let (mmapAddr, _) = symbolCache.address(of: "mmap") else {
      throw RemoteProcessError.missingSymbol("mmap")
    }

    guard let (munmapAddr, _) = symbolCache.address(of: "munmap") else {
      throw RemoteProcessError.missingSymbol("munmap")
    }

    // We call malloc_iterate in the remote process to enumerate all items in
    // remote process' heap.
    guard let (mallocIterateAddr, _) = symbolCache.address(of: "malloc_iterate") else {
      throw RemoteProcessError.missingSymbol("malloc_iterate")
    }

    // Allocate a page-sized buffer in the remote process that malloc_iterate
    // will populaate with metadata describing each heap entry it enumerates.
    //let dataLen = 32
    let dataLen = sysconf(Int32(_SC_PAGESIZE))
    var mmapArgs = [0, UInt64(dataLen), UInt64(PROT_READ | PROT_WRITE), UInt64(MAP_ANON | MAP_PRIVATE)]
    let remoteDataAddr: UInt64 = try self.ptrace.callRemoteFunction(at: mmapAddr, with: mmapArgs)
    defer {
      let munmapArgs: [UInt64] = [remoteDataAddr, UInt64(dataLen)]
      _ = try? self.ptrace.callRemoteFunction(at: munmapAddr, with: munmapArgs)
    }

    // Allocate and inialize a local buffer that will be used to copy metadata
    // to/from the target process.
    let buffer = UnsafeMutableRawPointer.allocate(byteCount: dataLen, alignment: MemoryLayout<UInt64>.alignment)
    defer { buffer.deallocate() }
    guard heap_iterate_metadata_init(buffer, dataLen) else {
      throw RemoteProcessError.heapIterationFailed
    }
    try self.process.writeMem(remoteAddr: remoteDataAddr, localAddr: buffer, len: UInt(dataLen))

    // Allocate an rwx region to hold the malloc_iterate callback that will be
    // executed in the remote process.
    let codeLen = UInt64(heap_iterate_callback_len())
    mmapArgs = [
      0, codeLen, UInt64(PROT_READ | PROT_WRITE | PROT_EXEC), UInt64(MAP_ANON | MAP_PRIVATE),
    ]
    let remoteCodeAddr: UInt64 = try self.ptrace.callRemoteFunction(at: mmapAddr, with: mmapArgs)
    defer {
      let munmapArgs: [UInt64] = [remoteCodeAddr, codeLen]
      _ = try? self.ptrace.callRemoteFunction(at: munmapAddr, with: munmapArgs)
    }

    // copy the malloc_iterate callback implementation to the remote process
    let codeStart = heap_iterate_callback_start()!
    try self.process.writeMem(
      remoteAddr: remoteCodeAddr, localAddr: codeStart, len: UInt(codeLen))

    var allocations: [(base: swift_addr_t, len: UInt64)] = []
    let regionLen = endAddr - startAddr
    let args = [startAddr, regionLen, remoteCodeAddr, remoteDataAddr]
    _ = try self.ptrace.callRemoteFunction(at: mallocIterateAddr, with: args) {
      // This callback is invoked when a SIGTRAP is encountered in the remote
      // process. In this context, this signal indicates there is no more room
      // in the allocated metadata region (see AndroidCLib/heap.c).
      // Immediately read and process the heap metadata from the remote process,
      // skip past the trap/break instruction and resume the remote process.
      try self.process.readMem(remoteAddr: remoteDataAddr, localAddr: buffer, len: UInt(dataLen))
      allocations.append(contentsOf: try self.processHeapAllocations(buffer: buffer, len: dataLen))

      guard heap_iterate_metadata_init(buffer, dataLen) else {
        throw RemoteProcessError.heapIterationFailed
      }
      try self.process.writeMem(remoteAddr: remoteDataAddr, localAddr: buffer, len: UInt(dataLen))

      var regs = try self.ptrace.getRegSet()
      regs.step(RegisterSet.trapInstructionSize)

      try self.ptrace.setRegSet(regSet: regs)
    }

    try self.process.readMem(remoteAddr: remoteDataAddr, localAddr: buffer, len: UInt(dataLen))
    allocations.append(contentsOf: try self.processHeapAllocations(buffer: buffer, len: dataLen))

    return allocations
  }

  internal func processHeapAllocations(buffer: UnsafeMutableRawPointer, len: Int) throws -> [(
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
}

#endif
