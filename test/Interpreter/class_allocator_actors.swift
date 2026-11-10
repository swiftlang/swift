// RUN: %empty-directory(%t)

// RUN: %target-build-swift-dylib(%t/%target-library-name(ActorLibrary)) -enable-experimental-feature ClassAllocators -target %target-future-triple -enable-library-evolution %S/Inputs/class_allocator_actor_library.swift -emit-module -emit-module-path %t/ActorLibrary.swiftmodule -module-name ActorLibrary -O
// RUN: %target-codesign %t/%target-library-name(ActorLibrary)

// RUN: %target-build-swift -enable-experimental-feature ClassAllocators -target %target-future-triple %import-libdispatch -parse-as-library %s -lActorLibrary -I %t -L %t -o %t/main-Onone -Onone -Xfrontend -sil-verify-all %target-rpath(%t)
// RUN: %target-codesign %t/main-Onone
// RUN: %target-run %t/main-Onone %t/%target-library-name(ActorLibrary)

// RUN: %target-build-swift -enable-experimental-feature ClassAllocators -target %target-future-triple %import-libdispatch -parse-as-library %s -lActorLibrary -I %t -L %t -o %t/main-O -O -Xfrontend -sil-verify-all %target-rpath(%t)
// RUN: %target-codesign %t/main-O
// RUN: %target-run %t/main-O %t/%target-library-name(ActorLibrary)

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime
// REQUIRES: libdispatch
// REQUIRES: swift_feature_ClassAllocators
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: back_deploy_concurrency
// UNSUPPORTED: single_threaded_concurrency

import ActorLibrary
import Dispatch
import StdlibUnittest
import Synchronization

let poolAllocations = Atomic<Int>(0)
let poolDeallocations = Atomic<Int>(0)

enum Pool: @unsafe ClassAllocator {
  static func allocate(
    byteCount: Int, alignment: Int, for type: AnyClass
  ) -> UnsafeMutableRawPointer {
    poolAllocations.add(1, ordering: .relaxed)
    return UnsafeMutableRawPointer.allocate(
      byteCount: byteCount, alignment: alignment)
  }

  static func deallocate(
    _ pointer: UnsafeMutableRawPointer, byteCount: Int, alignment: Int,
    for type: AnyClass
  ) {
    poolDeallocations.add(1, ordering: .relaxed)
    unsafe pointer.deallocate()
  }
}

struct Counts: Equatable {
  var allocations, deallocations: Int
}

/// The allocations and deallocations of `pool` during `body`, waiting up to
/// ten seconds for `expected` to be reached: the concurrency runtime may free
/// an actor after its deinit, when the actor stops running.
func counting(
  _ allocations: borrowing Atomic<Int>, _ deallocations: borrowing Atomic<Int>,
  expecting expected: Counts, _ body: () async -> Void
) async -> Counts {
  let allocationsBefore = allocations.load(ordering: .relaxed)
  let deallocationsBefore = deallocations.load(ordering: .relaxed)
  await body()
  var counts = Counts(allocations: 0, deallocations: 0)
  for _ in 0..<10_000 {
    counts = Counts(
      allocations: allocations.load(ordering: .relaxed) - allocationsBefore,
      deallocations:
        deallocations.load(ordering: .relaxed) - deallocationsBefore)
    if counts == expected { break }
    try? await Task.sleep(nanoseconds: 1_000_000)
  }
  return counts
}

func counting(
  expecting expected: Counts, _ body: () async -> Void
) async -> Counts {
  await counting(poolAllocations, poolDeallocations, expecting: expected, body)
}

nonisolated(unsafe) var escaped: AnyObject?

@inline(never)
func escape(_ object: AnyObject?) {
  escaped = object
}

let deinits = Atomic<Int>(0)

@allocator(Pool)
actor Idle {
  var value = 0
  func bump() -> Int {
    value += 1
    return value
  }
  deinit { deinits.add(1, ordering: .relaxed) }
}

/// An actor whose last reference is released while it's running a job: the
/// task that runs on it holds the only reference, and releases it when it
/// completes, so the runtime frees the actor when it stops running.
@allocator(Pool)
actor Busy {
  var value = 0
  func startWork(_ done: DispatchSemaphore) {
    Task {
      self.value += 1
      done.signal()
      // Keep the actor running for a while after its last outside reference
      // is gone.
      for _ in 0..<1_000 { self.value &+= 1 }
    }
  }
}

@allocator(Pool)
actor IsolatedDeinit {
  var value = 0
  isolated deinit {
    value += 1
    deinits.add(1, ordering: .relaxed)
  }
}

/// An actor with a custom executor. The compiled deinit frees it through the
/// allocator directly.
@allocator(Pool)
actor OnQueue {
  let queue = DispatchSerialQueue(label: "OnQueue")
  nonisolated var unownedExecutor: UnownedSerialExecutor {
    queue.asUnownedSerialExecutor()
  }
  var value = 0
  func bump() -> Int {
    value += 1
    return value
  }
}

final class UnownedHolder {
  unowned let target: Idle
  init(_ target: Idle) { self.target = target }
}

@main struct Main {
  static func main() async {
    let tests = TestSuite("ClassAllocatorActors")

    tests.test("idle default actor") {
      let expected = Counts(allocations: 1, deallocations: 1)
      let deinitsBefore = deinits.load(ordering: .relaxed)
      let counts = await counting(expecting: expected) {
        let actor = Idle()
        expectEqual(await actor.bump(), 1)
        escape(actor)
        escape(nil)
      }
      expectEqual(counts, expected)
      expectEqual(deinits.load(ordering: .relaxed) - deinitsBefore, 1)
    }

    tests.test("default actor released while running") {
      let expected = Counts(allocations: 10, deallocations: 10)
      let counts = await counting(expecting: expected) {
        for _ in 0..<10 {
          let done = DispatchSemaphore(value: 0)
          await Busy().startWork(done)
          await withCheckedContinuation { continuation in
            DispatchQueue.global().async {
              done.wait()
              continuation.resume()
            }
          }
        }
      }
      expectEqual(counts, expected)
    }

    tests.test("isolated deinit") {
      let expected = Counts(allocations: 1, deallocations: 1)
      let deinitsBefore = deinits.load(ordering: .relaxed)
      let counts = await counting(expecting: expected) {
        escape(IsolatedDeinit())
        escape(nil)
      }
      expectEqual(counts, expected)
      expectEqual(deinits.load(ordering: .relaxed) - deinitsBefore, 1)
    }

    tests.test("actor with a custom executor") {
      let expected = Counts(allocations: 1, deallocations: 1)
      let counts = await counting(expecting: expected) {
        let actor = OnQueue()
        expectEqual(await actor.bump(), 1)
        escape(actor)
        escape(nil)
      }
      expectEqual(counts, expected)
    }

    tests.test("unowned reference to an actor") {
      var holder: UnownedHolder?
      let freedAtDeinit = await counting(
        expecting: Counts(allocations: 1, deallocations: 0)
      ) {
        let actor = Idle()
        holder = UnownedHolder(actor)
        escape(holder)
      }
      expectEqual(freedAtDeinit, Counts(allocations: 1, deallocations: 0))
      let freedLater = await counting(
        expecting: Counts(allocations: 0, deallocations: 1)
      ) {
        holder = nil
        escape(nil)
      }
      expectEqual(freedLater, Counts(allocations: 0, deallocations: 1))
      _ = holder
    }

    tests.test("internal actor of a library built with library evolution") {
      let expected = Counts(allocations: 1, deallocations: 1)
      let counts = await counting(
        libraryActorAllocations, libraryActorDeallocations,
        expecting: expected
      ) {
        let counter = makeLibraryCounter()
        expectEqual(await counter.increment(), 1)
        escape(counter)
        escape(nil)
      }
      expectEqual(counts, expected)
    }

    await runAllTestsAsync()
  }
}
