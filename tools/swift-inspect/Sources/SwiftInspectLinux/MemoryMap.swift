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

public class MemoryMap {
  public enum MemoryMapError: Error {
    case failedLoadingMapsFile(for: pid_t)
  }

  public struct Entry {
    public let startAddr: UInt64
    public let endAddr: UInt64
    public let permissions: String
    public let offset: UInt64
    public let device: String
    public let inode: UInt64
    public let pathname: String?
  }

  public let entries: [Entry]

  public init(for pid: pid_t) throws {
    guard let content = ProcFS.loadFileAsString(for: pid, "maps") else {
      throw MemoryMapError.failedLoadingMapsFile(for: pid)
    }

    var entries: [Entry] = []

    content.enumerateLines { (line, _) in
      let components = line.split(separator: " ", omittingEmptySubsequences: true)
      guard components.count >= 5 else { return }
      let addrParts = components[0].split(separator: "-")
      let entry = Entry(
        startAddr: UInt64(addrParts[0], radix: 16) ?? 0,
        endAddr: UInt64(addrParts[1], radix: 16) ?? 0, permissions: String(components[1]),
        offset: UInt64(components[2], radix: 16) ?? 0, device: String(components[3]),
        inode: UInt64(components[4]) ?? 0,
        pathname: components.count == 6 ? String(components[5]) : nil)
      entries.append(entry)
    }

    self.entries = entries
  }

  public func findEntry(containing addr: UInt64) -> Entry? {
    // The map array returned by loadMaps will be ordered the same as the
    // contents of /proc/<pid>/maps, which is in ascending address order.
    // Binary search it to find the entry containing the target address.
    var lowerBound = 0
    var upperBound = self.entries.count
    while lowerBound < upperBound {
      let currentIndex = (lowerBound + upperBound) / 2
      let entry = self.entries[currentIndex]
      if entry.startAddr > addr {
        upperBound = currentIndex
      } else if entry.endAddr <= addr {
        lowerBound = currentIndex + 1
      } else {
        precondition(addr >= entry.startAddr)
        precondition(addr < entry.endAddr)
        return entry
      }
    }
    return nil
  }
}
