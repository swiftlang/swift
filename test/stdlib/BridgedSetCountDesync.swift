//===--- BridgedSetCountDesync.swift --------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//

// RUN: %target-run-simple-swift(-Onone)
// RUN: %target-run-simple-swift(-O)
// RUN: %target-run-simple-swift(-Ounchecked)

// REQUIRES: executable_test
// REQUIRES: objc_interop

import StdlibUnittest
import Foundation

/// An `NSSet` subclass whose reported `count` and fast-enumeration output can
/// be made to disagree, to simulate a corrupted object.
final class ControllableNSSet: NSSet {
  private var vend: [NSObject] = []      // objects produced by enumeration
  private var reportedCount = 0          // what -count returns (sizing hint)
  private var batchSize = 1              // elements per countByEnumerating call
  private var mutationsPtr: UnsafeMutablePointer<UInt>?
  private var storage: UnsafeMutablePointer<AnyObject?>?

  convenience init(vend: [NSObject], reportedCount: Int, batchSize: Int) {
    self.init()
    self.vend = vend
    self.reportedCount = reportedCount
    self.batchSize = Swift.max(batchSize, 1)
    let mutations = UnsafeMutablePointer<UInt>.allocate(capacity: 1)
    mutations.initialize(to: 0)
    self.mutationsPtr = mutations
    let store = UnsafeMutablePointer<AnyObject?>.allocate(
      capacity: Swift.max(vend.count, 1))
    for (i, object) in vend.enumerated() {
      (store + i).initialize(to: object)
    }
    self.storage = store
  }

  deinit {
    mutationsPtr?.deallocate()
    if let storage = storage {
      storage.deinitialize(count: vend.count)
      storage.deallocate()
    }
  }

  override var count: Int { reportedCount }

  override func member(_ object: Any) -> Any? {
    guard let obj = object as? NSObject else { return nil }
    return vend.first { $0.isEqual(obj) }
  }

  override func objectEnumerator() -> NSEnumerator {
    return NSArray(array: vend).objectEnumerator()
  }

  // Claim to be an immutable set that copies cheaply so the bridging machinery
  // doesn't turn us into a regular NSSet when it calls -copy
  override func copy(with zone: NSZone?) -> Any { return self }

  override func countByEnumerating(
    with state: UnsafeMutablePointer<NSFastEnumerationState>,
    objects buffer: AutoreleasingUnsafeMutablePointer<AnyObject?>,
    count len: Int
  ) -> Int {
    guard let storage = storage else { return 0 }
    var st = state.pointee
    let start = Int(st.state)
    if start >= vend.count { return 0 }
    let n = Swift.min(batchSize, vend.count - start)
    st.state = UInt(start + n)
    st.itemsPtr = AutoreleasingUnsafeMutablePointer(storage + start)
    st.mutationsPtr = mutationsPtr
    state.pointee = st
    return n
  }
}

// This happens to use _stdlib_NSSet_allObjects internally, so is handy for
// testing it
@inline(never)
func drainByIndex(_ s: Set<NSObject>) -> [NSObject] {
  var result: [NSObject] = []
  var i = s.startIndex
  while i != s.endIndex {
    result.append(s[i])
    i = s.index(after: i)
  }
  return result
}

var suite = TestSuite("BridgedSetCountDesync")

suite.test("single batch") {
  let elements = (0..<8).map { _ in NSObject() }
  let expected = Set(elements.map(ObjectIdentifier.init))

  let ns = ControllableNSSet(vend: elements, reportedCount: 8, batchSize: 16)
  let bridged = ns as! Set<NSObject>

  let drained = drainByIndex(bridged)
  expectEqual(8, drained.count)
  expectEqual(expected, Set(drained.map(ObjectIdentifier.init)))
  withExtendedLifetime(ns) {}
}

suite.test("small batches") {
  let elements = (0..<40).map { _ in NSObject() }
  let expected = Set(elements.map(ObjectIdentifier.init))

  // small batch size to make sure we get coverage for the batching loop
  let ns = ControllableNSSet(vend: elements, reportedCount: 40, batchSize: 3)
  let bridged = ns as! Set<NSObject>

  let drained = drainByIndex(bridged)
  expectEqual(40, drained.count)
  expectEqual(expected, Set(drained.map(ObjectIdentifier.init)))
  withExtendedLifetime(ns) {}
}

suite.test("over-vend traps") {
  let elements = (0..<20).map { _ in NSObject() }
  let ns = ControllableNSSet(vend: elements, reportedCount: 4, batchSize: 16)
  let bridged = ns as! Set<NSObject>
  expectCrashLater()
  _ = drainByIndex(bridged)
  withExtendedLifetime(ns) {}
}

suite.test("under-vend traps") {
  let elements = (0..<10).map { _ in NSObject() }
  let ns = ControllableNSSet(vend: elements, reportedCount: 100, batchSize: 16)
  let bridged = ns as! Set<NSObject>
  expectCrashLater()
  _ = drainByIndex(bridged)
  withExtendedLifetime(ns) {}
}

runAllTests()
