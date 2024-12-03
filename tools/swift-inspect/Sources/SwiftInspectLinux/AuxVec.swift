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

import Foundation
import LinuxSystemHeaders

internal class AuxVec {
  enum Error: Swift.Error { case FileReadFailure(_ filePath: String) }

  // enum values must match the constants defined in usr/include/linux/auxv.h
  enum Tag: UInt64 {
    case AT_NULL = 0
    case AT_IGNORE = 1
    case AT_EXECFD = 2
    case AT_PHDR = 3
    case AT_PHENT = 4
    case AT_PHNUM = 5
    case AT_PAGESZ = 6
    case AT_BASE = 7
    case AT_FLAGS = 8
    case AT_ENTRY = 9
    case AT_NOTELF = 10
    case AT_UID = 11
    case AT_EUID = 12
    case AT_GID = 13
    case AT_EGID = 14
    case AT_PLATFORM = 15
    case AT_HWCAP = 16
    case AT_CLKTCK = 17
    case AT_SECURE = 23
    case AT_BASE_PLATFORM = 24
    case AT_RANDOM = 25
    case AT_HWCAP2 = 26
    case AT_RSEQ_FEATURE_SIZE = 27
    case AT_RSEQ_ALIGN = 28
    case AT_EXECFN = 31
    case AT_SYSINFO_EHDR = 33
    case AT_MINSIGSTKSZ = 51
  }

  static func load(for process: Process) throws -> [Tag: UInt64] {
    let filePath = "/proc/\(process.pid)/auxv"

    let fileHandle = try FileHandle(forReadingFrom: URL(fileURLWithPath: filePath))
    defer { fileHandle.closeFile() }

    guard let data = try fileHandle.readToEnd() else { throw Error.FileReadFailure(filePath) }

    // aux vector is an array of 8-byte pairs in a 64-bit process
    assert(process.elfFile.isElf64, "only 64-bit processes are supported")
    let auxVec: [(UInt64, UInt64)] = data.withUnsafeBytes {
      let count = $0.count / MemoryLayout<(UInt64, UInt64)>.stride
      return Array($0.bindMemory(to: (UInt64, UInt64).self)[..<count])
    }

    var entries: [Tag: UInt64] = [:]
    for (rawTag, value) in auxVec {
      guard let tag = Tag(rawValue: rawTag) else { continue }
      entries[tag] = value
    }

    return entries
  }
}
