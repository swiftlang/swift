// RUN: %empty-directory(%t)
// RUN: %target-build-swift -target %target-swift-5.1-abi-triple -parse-as-library %s -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: env %env-DYLD_LIBRARY_PATH=%swift-lib-dir/swift/%target-sdk-name %target-run %t/a.out
// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: freestanding

import _Concurrency
#if canImport(Darwin)
import Darwin
#elseif canImport(Glibc)
import Glibc
#elseif canImport(Musl)
import Musl
#elseif os(Windows)
import CRT
#endif
import Dispatch
import StdlibUnittest

let RTLD_DEFAULT = UnsafeMutableRawPointer(bitPattern: -2)

func registryCount() -> Int {
  typealias Fn = @convention(c) () -> Int
  guard let sym = dlsym(RTLD_DEFAULT, "__swift_concurrency_debug_task_registryCount") else { return -1 }
  return unsafeBitCast(sym, to: Fn.self)()
}

typealias GetShardHeadFn = @convention(c) (Int) -> UnsafeRawPointer?
typealias GetTaskNextFn = @convention(c) (UnsafeRawPointer) -> UnsafeRawPointer?
typealias GetTaskIdFn = @convention(c) (UnsafeRawPointer) -> UInt64

func getShardHead(index: Int) -> UnsafeRawPointer? {
  guard let sym = dlsym(RTLD_DEFAULT, "__swift_concurrency_debug_task_getShardHead") else { return nil }
  return unsafeBitCast(sym, to: GetShardHeadFn.self)(index)
}

func getTaskNext(task: UnsafeRawPointer) -> UnsafeRawPointer? {
  guard let sym = dlsym(RTLD_DEFAULT, "__swift_concurrency_debug_task_getTaskNext") else { return nil }
  return unsafeBitCast(sym, to: GetTaskNextFn.self)(task)
}

func getTaskId(task: UnsafeRawPointer) -> UInt64 {
  guard let sym = dlsym(RTLD_DEFAULT, "__swift_concurrency_debug_task_getId") else { return 0 }
  return unsafeBitCast(sym, to: GetTaskIdFn.self)(task)
}

actor Barrier {
  private var arrived = 0
  private var waiter: CheckedContinuation<Void, Never>?
  private let target: Int
  init(_ n: Int) { target = n }
  func arrive() { arrived += 1; if arrived == target { waiter?.resume() } }
  func wait() async { if arrived < target { await withCheckedContinuation { waiter = $0 } } }
}

actor ContinuationRegistry {
  private var conts: [CheckedContinuation<Void, Never>] = []
  func register(_ cont: CheckedContinuation<Void, Never>) {
    conts.append(cont)
  }
  func takeAll() -> [CheckedContinuation<Void, Never>] {
    let list = conts
    conts = []
    return list
  }
}

@main struct Main {
  static func main() async {
    let tests = TestSuite("TaskRegistry")

    // Spawn N tasks one at a time; verify the registry count rises by 1 after
    // each creation and falls by 1 after each task is fully destroyed.
    tests.test("perTaskIncrementAndDecrement") {
      let n = 20
      let baseline = registryCount()

      for i in 0..<n {
        let g = DispatchGroup()
        g.enter()

        let sem = DispatchSemaphore(value: 0)
        Task {
          sem.wait()
          g.leave()
        }

        let countAfterCreate = registryCount()
        expectGE(countAfterCreate, baseline + 1,
          "after spawning task \(i+1): expected count >= \(baseline + 1), got \(countAfterCreate)")

        sem.signal()
        g.wait()

        let countAfterDestroy = registryCount()
        expectLE(countAfterDestroy, baseline,
          "after task \(i+1) finished: expected count <= \(baseline), got \(countAfterDestroy)")
      }
    }

    // Verify that every task in the registry is placed in the correct shard (index == taskId % 16)
    tests.test("shardDistribution") {
      let n = 20
      let barrier = Barrier(n)
      let contRegistry = ContinuationRegistry()
      let done = DispatchGroup()
      done.enter()

      Task {
        for _ in 0..<n {
          Task {
            await withCheckedContinuation { (cont: CheckedContinuation<Void, Never>) in
              Task {
                await contRegistry.register(cont)
                await barrier.arrive()
              }
            }
          }
        }

        await barrier.wait()
        try? await Task.sleep(nanoseconds: 10_000_000)

        // Walk all 16 shards
        var totalFound = 0
        for shardIndex in 0..<16 {
          var curr = getShardHead(index: shardIndex)
          while let taskPtr = curr {
            let id = getTaskId(task: taskPtr)
            expectEqual(Int(id % 16), shardIndex, "Task with ID \(id) found in wrong shard \(shardIndex) (expected \(id % 16))")
            totalFound += 1
            curr = getTaskNext(task: taskPtr)
          }
        }

        expectGE(totalFound, n, "Expected to find at least \(n) tasks across the shards")

        // Resume all
        let resume = await contRegistry.takeAll()
        for c in resume { c.resume() }
        
        done.leave()
      }

      done.wait()
    }

    // Verify that concurrent additions and removals on the same shards do not corrupt lists
    tests.test("concurrentCollisions") {
      let concurrency = 200
      let barrier = Barrier(concurrency)
      let contRegistry = ContinuationRegistry()
      let done = DispatchGroup()
      done.enter()

      Task {
        await withTaskGroup(of: Void.self) { group in
          for _ in 0..<concurrency {
            group.addTask {
              await withCheckedContinuation { (cont: CheckedContinuation<Void, Never>) in
                Task {
                  await contRegistry.register(cont)
                  await barrier.arrive()
                }
              }
            }
          }

          await barrier.wait()
          try? await Task.sleep(nanoseconds: 20_000_000)

          // Verify total count
          let count = registryCount()
          expectGE(count, concurrency, "Expected count >= \(concurrency), got \(count)")

          // Resume all
          let resume = await contRegistry.takeAll()
          for c in resume { c.resume() }
        }
        done.leave()
      }

      done.wait()
    }

    await runAllTestsAsync()
  }
}
