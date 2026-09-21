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
@_silgen_name("_swift_concurrency_debug_task_registryCount")
func registryCount() -> Int

@_silgen_name("_swift_concurrency_debug_task_getShardHead")
func getShardHead(index: Int) -> UnsafeRawPointer?

@_silgen_name("_swift_concurrency_debug_task_getTaskNext")
func getTaskNext(task: UnsafeRawPointer) -> UnsafeRawPointer?

@_silgen_name("_swift_concurrency_debug_task_getId")
func getTaskId(task: UnsafeRawPointer) -> UInt64

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
    var task: Task<Void, Never>? = Task {
      try? await Task.sleep(nanoseconds: 10_000_000)
    }

    let countAfterCreate = registryCount()
    assert(countAfterCreate >= baseline + 1, "after spawning task \(i+1): expected count >= \(baseline + 1), got \(countAfterCreate)")

    _ = await task!.result
    task = nil

    var countAfterDestroy = registryCount()
    var retries = 0
    while countAfterDestroy > baseline && retries < 10 {
      try? await Task.sleep(nanoseconds: 100_000_000)
      countAfterDestroy = registryCount()
      retries += 1
    }
    assert(countAfterDestroy <= baseline, "after task \(i+1) finished: expected count <= \(baseline), got \(countAfterDestroy)")
  }
}

func test_shardDistribution() async {
  let n = 300
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
    for shardIndex in 0..<64 {
      var curr = getShardHead(index: shardIndex)
      while let taskPtr = curr {
        let id = getTaskId(task: taskPtr)
        let expectedShard = Int((id ^ (id >> 8)) & 63)
        assert(expectedShard == shardIndex, "Task with ID \(id) found in wrong shard \(shardIndex)")
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
  let concurrency = 300
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
