// RUN: %target-swift-frontend -parse-as-library -disable-availability-checking -emit-sil %s -o /dev/null

// REQUIRES: swift_stdlib_no_asserts,optimized_stdlib
// REQUIRES: synchronization

import Synchronization

// Check that atomics work in no-locks mode.

@_noLocks
func testFence() {
  atomicMemoryFence(ordering: .acquiring)
  atomicMemoryFence(ordering: .releasing)
  atomicMemoryFence(ordering: .acquiringAndReleasing)
  atomicMemoryFence(ordering: .sequentiallyConsistent)
}

@_noLocks
func testLoadStore() -> Int {
  let x = Atomic(0)
  x.store(27, ordering: .relaxed)
  x.compareExchange(expected: 27, desired: 42, successOrdering: .relaxed, failureOrdering: .relaxed)
  return x.load(ordering: .acquiring)
}

@_noLocks
func testRMW(_ b: Bool) -> (Bool, Bool) {
  let x = Atomic(false)
  return x.logicalOr(true, ordering: .relaxed)
}

