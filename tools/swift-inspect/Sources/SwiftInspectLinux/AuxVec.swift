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

internal class AuxVec {
  // loads the auxiliary vector for a process
  public static func load(for process: Process) -> [Int32 : UInt64]? {
    let filePath = "/proc/\(process.pid)/auxv"
    guard let fileHandle = FileHandle(forReadingAtPath: filePath) else { return nil }
    defer { fileHandle.closeFile() }

    guard let data = try? fileHandle.readToEnd(), data.count > 0 else { return nil }

    func fromData<T: UnsignedInteger>(_ data: Data) -> [(T, T)] {
      return data.withUnsafeBytes {
        let count = $0.count / MemoryLayout<(T, T)>.stride
        return Array($0.bindMemory(to: (T, T).self)[..<count])
      }
    }

    func fromArray<T: UnsignedInteger>(_ array: [(T, T)]) -> [Int32: UInt64] {
      var entries: [Int32: UInt64] = [:]
      for (rawTag, value) in array {
        // the AT_ constants defined in linux/auxv.h are imported as Int32
        guard let tag = Int32(exactly: rawTag) else { continue }
        entries[tag] = UInt64(value)
      }
      return entries
    }

    // in a 32-bit process, aux vector is an array of 4-byte pairs
    // in a 64-bit process, aux vector is an array of 8-byte pairs
    return process.elfFile.isElf64
        ? fromArray(fromData(data) as [(UInt64, UInt64)])
        : fromArray(fromData(data) as [(UInt32, UInt32)])
  }

}
