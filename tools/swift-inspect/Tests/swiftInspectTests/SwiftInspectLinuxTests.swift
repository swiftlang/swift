//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#if os(Linux)
import XCTest
import Glibc
@testable import SwiftInspectLinux

final class SwiftInspectLinuxTests: XCTestCase {

  /// A representative `/proc/<pid>/maps` excerpt.
  static let sampleMaps = """
    55c4d3f3f000-55c4d3f44000 r--p 00000000 fd:01 1234567 /usr/bin/cat
    55c4d3f44000-55c4d3f4c000 r-xp 00005000 fd:01 1234567 /usr/bin/cat
    55c4d3f4c000-55c4d3f4f000 r--p 0000d000 fd:01 1234567 /usr/bin/cat
    55c4d3f4f000-55c4d3f50000 rw-p 0000f000 fd:01 1234567 /usr/bin/cat
    55c4d4111000-55c4d4132000 rw-p 00000000 00:00 0       [heap]
    7f1234560000-7f1234570000 rw-p 00000000 00:00 0
    7f1234570000-7f1234580000 rw-p 00000000 00:00 0       [anon:libc_malloc]
    7ffe1abcd000-7ffe1abef000 rw-p 00000000 00:00 0       [stack]
    7ffe1abf0000-7ffe1abf3000 r--p 00000000 00:00 0       [vvar]
    7ffe1abf3000-7ffe1abf5000 r-xp 00000000 00:00 0       [vdso]
    """

  func testMemoryMapParse() {
    let map = MemoryMap(parsing: SwiftInspectLinuxTests.sampleMaps)
    XCTAssertEqual(map.entries.count, 10)

    let heap = map.entries.first { $0.pathname == "[heap]" }
    XCTAssertNotNil(heap)
    XCTAssertEqual(heap?.startAddr, 0x55c4_d411_1000)
    XCTAssertEqual(heap?.endAddr, 0x55c4_d413_2000)
    XCTAssertEqual(heap?.permissions, "rw-p")

    // Anonymous mapping (no pathname column) parses as nil pathname.
    let anon = map.entries.first { $0.startAddr == 0x7f12_3456_0000 }
    XCTAssertNotNil(anon)
    XCTAssertNil(anon?.pathname)

    // Labeled-anonymous mapping keeps its label.
    let anonLabeled = map.entries.first { $0.pathname == "[anon:libc_malloc]" }
    XCTAssertNotNil(anonLabeled)
  }

  func testMemoryMapFindEntry() {
    let map = MemoryMap(parsing: SwiftInspectLinuxTests.sampleMaps)

    // Inside the heap range.
    let heapHit = map.findEntry(containing: 0x55c4_d411_5000)
    XCTAssertEqual(heapHit?.pathname, "[heap]")

    // Right before the heap (still inside the rw-p data segment of /usr/bin/cat).
    let beforeHeap = map.findEntry(containing: 0x55c4_d3f4_f100)
    XCTAssertEqual(beforeHeap?.pathname, "/usr/bin/cat")

    // In a hole between mapped regions.
    let hole = map.findEntry(containing: 0x6000_0000_0000)
    XCTAssertNil(hole)

    // Boundary: endAddr is exclusive.
    let atEndAddr = map.findEntry(containing: 0x55c4_d413_2000)
    XCTAssertNotEqual(atEndAddr?.pathname, "[heap]")
  }

  // ==== -----------------------------------------------------------------
  // MARK: isLikelyHeapRegion heuristic

  /// Helper: synthesize a `MemoryMap.Entry` for an isolated unit test.
  private func entry(_ permissions: String, _ pathname: String?) -> MemoryMap.Entry {
    return MemoryMap.Entry(
      startAddr: 0x1000, endAddr: 0x2000,
      permissions: permissions, offset: 0,
      device: "00:00", inode: 0, pathname: pathname)
  }

  func testIsLikelyHeapRegion_includesHeapAndAnonymous() {
    XCTAssertTrue(entry("rw-p", "[heap]").isLikelyHeapRegion())
    XCTAssertTrue(entry("rw-p", nil).isLikelyHeapRegion())
    XCTAssertTrue(entry("rw-p", "[anon:libc_malloc]").isLikelyHeapRegion())
    XCTAssertTrue(entry("rw-p", "[anon:scudo:primary]").isLikelyHeapRegion())
    // rwxp is unusual but we keep it (jemalloc sampling, GWP-ASan, etc.).
    XCTAssertTrue(entry("rwxp", nil).isLikelyHeapRegion())
  }

  func testIsLikelyHeapRegion_excludesStackAndKernelRegions() {
    XCTAssertFalse(entry("rw-p", "[stack]").isLikelyHeapRegion())
    XCTAssertFalse(entry("rw-p", "[stack:1234]").isLikelyHeapRegion())
    XCTAssertFalse(entry("r--p", "[vvar]").isLikelyHeapRegion())
    XCTAssertFalse(entry("r-xp", "[vdso]").isLikelyHeapRegion())
    XCTAssertFalse(entry("--xp", "[vsyscall]").isLikelyHeapRegion())
  }

  func testIsLikelyHeapRegion_excludesFileBacked() {
    XCTAssertFalse(entry("rw-p", "/usr/bin/cat").isLikelyHeapRegion())
    XCTAssertFalse(entry("rw-p", "/lib/x86_64-linux-gnu/libc.so.6").isLikelyHeapRegion())
  }

  func testIsLikelyHeapRegion_requiresReadWritePrivate() {
    // No read.
    XCTAssertFalse(entry("-w-p", "[heap]").isLikelyHeapRegion())
    // No write - this is a code segment, not heap.
    XCTAssertFalse(entry("r--p", "[heap]").isLikelyHeapRegion())
    // Shared mapping - Swift's heap is private.
    XCTAssertFalse(entry("rw-s", nil).isLikelyHeapRegion())
    // Empty / malformed permissions string.
    XCTAssertFalse(entry("", nil).isLikelyHeapRegion())
    XCTAssertFalse(entry("rw", nil).isLikelyHeapRegion())
  }

}
#endif  // os(Linux)
