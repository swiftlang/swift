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

import Foundation
import LinuxSystemHeaders

class LinkMap {
  public enum LinkMapError: Error {
    case failedLoadingAuxVec(for: pid_t)
    case missingAuxVecEntry(for: pid_t, _ tag: Int32)
    case malformedELF(for: pid_t, _ description: String)
  }

  public struct Entry {
    let baseAddress: UInt64
    let moduleName: String
  }

  public let entries: [Entry]

  public init(for process: Process) throws {
    let auxVec = try Self.loadAuxVec(for: process.pid)
    guard let phdrAddr = auxVec[AT_PHDR] else {
      throw LinkMapError.missingAuxVecEntry(for: process.pid, AT_PHDR)
    }

    guard let phdrSize = auxVec[AT_PHENT] else {
      throw LinkMapError.missingAuxVecEntry(for: process.pid, AT_PHENT)
    }

    guard let phdrCount = auxVec[AT_PHNUM] else {
      throw LinkMapError.missingAuxVecEntry(for: process.pid, AT_PHNUM)
    }

    guard phdrSize == MemoryLayout<Elf64_Phdr>.size else {
      throw LinkMapError.malformedELF(for: process.pid, "AT_PHENT invalid size: \(phdrSize)")
    }

    // determine the base load address for the executable file and locate the
    // dynamic segment
    var dynamicSegment: Elf64_Phdr? = nil
    var baseLoadSegment: Elf64_Phdr? = nil
    for i in 0...phdrCount {
      let address: UInt64 = phdrAddr + i * phdrSize
      let phdr: Elf64_Phdr = try process.readStruct(address: address)

      switch phdr.p_type {
      case UInt32(PT_LOAD):
        // chose the PT_LOAD segment with the lowest p_vaddr value, which will
        // typically be zero
        if let loadSegment = baseLoadSegment {
          if phdr.p_vaddr < loadSegment.p_vaddr { baseLoadSegment = phdr }
        } else {
          baseLoadSegment = phdr
        }

      case UInt32(PT_DYNAMIC):
        guard dynamicSegment == nil else {
          throw LinkMapError.malformedELF(for: process.pid, "multiple PT_DYNAMIC segments found")
        }
        dynamicSegment = phdr

      default: continue
      }
    }

    guard let dynamicSegment = dynamicSegment else {
      throw LinkMapError.malformedELF(for: process.pid, "PT_DYNAMIC segment not found")
    }

    guard let baseLoadSegment = baseLoadSegment else {
      throw LinkMapError.malformedELF(for: process.pid, "PT_LOAD segment not found")
    }

    let ehdrSize = MemoryLayout<Elf64_Ehdr>.size
    let loadAddr: UInt64 = phdrAddr - UInt64(ehdrSize)
    let baseAddr: UInt64 = loadAddr - baseLoadSegment.p_vaddr
    let dynamicSegmentAddr: UInt64 = baseAddr + dynamicSegment.p_vaddr

    // parse through the dynamic segment to find the location of the .debug section
    var rDebugEntry: Elf64_Dyn? = nil
    let entrySize = MemoryLayout<Elf64_Dyn>.size
    let dynamicEntryCount = UInt(dynamicSegment.p_memsz / UInt64(entrySize))
    for i in 0...dynamicEntryCount {
      let address: UInt64 = dynamicSegmentAddr + UInt64(i) * UInt64(entrySize)
      let dyn: Elf64_Dyn = try process.readStruct(address: address)
      if dyn.d_tag == DT_DEBUG {
        rDebugEntry = dyn
        break
      }
    }

    guard let rDebugEntry = rDebugEntry else {
      throw LinkMapError.malformedELF(for: process.pid, "DT_DEBUG not found in dynamic segment")
    }

    let rDebugAddr: UInt64 = rDebugEntry.d_un.d_val
    let rDebug: r_debug = try process.readStruct(address: rDebugAddr)

    var entries: [Entry] = []
    var linkMapAddr = UInt(bitPattern: rDebug.r_map)
    while linkMapAddr != 0 {
      let linkMap: link_map = try process.readStruct(address: UInt64(linkMapAddr))
      let nameAddr = UInt(bitPattern: linkMap.l_name)
      let name = try process.readString(address: UInt64(nameAddr))
      entries.append(Entry(baseAddress: linkMap.l_addr, moduleName: name))

      linkMapAddr = UInt(bitPattern: linkMap.l_next)
    }

    self.entries = entries
  }

  // loads the auxiliary vector for a 64-bit process
  static func loadAuxVec(for pid: pid_t) throws -> [Int32: UInt64] {
    guard let data = ProcFS.loadFile(for: pid, "auxv") else {
      throw LinkMapError.failedLoadingAuxVec(for: pid)
    }

    return data.withUnsafeBytes {
      // in a 64-bit process, aux vector is an array of 8-byte pairs
      let count = $0.count / MemoryLayout<(UInt64, UInt64)>.stride
      let auxVec = Array($0.bindMemory(to: (UInt64, UInt64).self)[..<count])

      var entries: [Int32: UInt64] = [:]
      for (rawTag, value) in auxVec {
        // the AT_ constants defined in linux/auxv.h are imported as Int32
        guard let tag = Int32(exactly: rawTag) else { continue }
        entries[tag] = UInt64(value)
      }

      return entries
    }
  }
}
