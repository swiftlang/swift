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

func test_perTaskIncrementAndDecrement() async {
  let n = 20
  let baseline = registryCount()

  for i in 0..<n {
    let task = Task {
      try? await Task.sleep(nanoseconds: 10_000_000)
    }

    let countAfterCreate = registryCount()
    assert(countAfterCreate >= baseline + 1, "after spawning task \(i+1): expected count >= \(baseline + 1), got \(countAfterCreate)")

    _ = await task.result

    // Wait a little bit for the task to be destroyed and unregistered
    try? await Task.sleep(nanoseconds: 1_000_000)

    let countAfterDestroy = registryCount()
    assert(countAfterDestroy <= baseline, "after task \(i+1) finished: expected count <= \(baseline), got \(countAfterDestroy)")
  }
}

func test_shardDistribution() async {
  let n = 20
  let barrier = Barrier(n)
  let contRegistry = ContinuationRegistry()

  let task = Task {
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

    var totalFound = 0
    for shardIndex in 0..<16 {
      var curr = getShardHead(index: shardIndex)
      while let taskPtr = curr {
        let id = getTaskId(task: taskPtr)
        assert(Int(id % 16) == shardIndex, "Task with ID \(id) found in wrong shard \(shardIndex)")
        totalFound += 1
        curr = getTaskNext(task: taskPtr)
      }
    }
    assert(totalFound >= n, "Expected to find at least \(n) tasks across the shards")

    let resume = await contRegistry.takeAll()
    for c in resume { c.resume() }
  }

  _ = await task.result
}

func test_concurrentCollisions() async {
  let concurrency = 200
  let barrier = Barrier(concurrency)
  let contRegistry = ContinuationRegistry()

  let task = Task {
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

      let count = registryCount()
      assert(count >= concurrency, "Expected count >= \(concurrency), got \(count)")

      let resume = await contRegistry.takeAll()
      for c in resume { c.resume() }
    }
  }

  _ = await task.result
}

@main struct Main {
  static func main() async {
    await test_perTaskIncrementAndDecrement()
    await test_shardDistribution()
    await test_concurrentCollisions()
  }
}
