// The `swift_createExecutors()` call that installs `DefaultExecutorFactory` is
// only emitted with availability checking disabled, because the
// `CustomGlobalExecutors` feature availability is still `FUTURE`.
// RUN: %target-run-simple-swift(-parse-as-library -Xfrontend -disable-availability-checking) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime
// REQUIRES: platform_executors

// The thread names are read from /proc.
// REQUIRES: OS=linux-gnu

// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: freestanding

// Uses the executors of swift-platform-executors, which the toolchain builds
// as the `PlatformExecutors` module on non-Apple platforms.

@_spi(ExperimentalCustomExecutors) import PlatformExecutors
@_spi(ExperimentalCustomExecutors) import _Concurrency

import Glibc

// Install the platform executors as the main and default executors.
typealias DefaultExecutorFactory = PlatformExecutorFactory

/// Returns the name of the current thread.
///
/// The platform executors name their threads after the executor, and append
/// the index of the thread for executors with a thread pool.
func currentThreadName() -> String {
  let fd = open("/proc/thread-self/comm", O_RDONLY)
  guard fd >= 0 else { return "<unknown>" }
  defer { close(fd) }
  var buffer = [UInt8](repeating: 0, count: 32)
  let count = buffer.withUnsafeMutableBytes { read(fd, $0.baseAddress, $0.count) }
  guard count > 0 else { return "<unknown>" }
  let name = String(decoding: buffer[..<count], as: UTF8.self)
  return name.split(separator: "\n").first.map(String.init) ?? ""
}

/// Returns the name of the current thread without the index of the thread in
/// its pool, for example `global` for the thread `global-3`.
func currentThreadPoolName() -> String {
  let name = currentThreadName()
  guard let dash = name.lastIndex(of: "-"),
        name[name.index(after: dash)...].allSatisfy(\.isNumber) else {
    return name
  }
  return String(name[..<dash])
}

/// An actor that runs on a platform serial executor.
actor Counter {
  let executor: PlatformSerialExecutor
  var value = 0
  var threadNames: Set<String> = []

  init(executor: PlatformSerialExecutor) {
    self.executor = executor
  }

  nonisolated var unownedExecutor: UnownedSerialExecutor {
    executor.asUnownedSerialExecutor()
  }

  func increment() {
    value += 1
    threadNames.insert(currentThreadName())
  }
}

@main struct Main {
  static func main() async {
    // CHECK: factory: PlatformExecutors.PlatformExecutorFactory
    print("factory: \(String(reflecting: DefaultExecutorFactory.self))")
    // CHECK: main executor installed: true
    print("main executor installed: \(MainActor.executor === DefaultExecutorFactory.mainExecutor)")
    // CHECK: default executor installed: true
    print("default executor installed: \(Task.defaultExecutor === DefaultExecutorFactory.defaultExecutor)")

    // Unstructured tasks run on the threads of the default executor.
    // CHECK: detached task thread: global
    let detachedThread = await Task.detached { currentThreadPoolName() }.value
    print("detached task thread: \(detachedThread)")

    // Sleeping schedules a timer on the default executor.
    // CHECK: slept at least 20ms: true, thread: global
    let clock = ContinuousClock()
    let (elapsed, sleepThread) = await Task.detached {
      let start = clock.now
      try? await Task.sleep(for: .milliseconds(20))
      return (clock.now - start, currentThreadPoolName())
    }.value
    print("slept at least 20ms: \(elapsed >= .milliseconds(20)), thread: \(sleepThread)")

    // Child tasks that prefer a platform task executor run on its threads.
    // CHECK: task executor sum: 5050, threads: pe-pool
    let (sum, poolThreads) = await PlatformExecutorFactory.withTaskExecutor(
      name: "pe-pool", poolSize: 4
    ) { executor in
      await withTaskGroup(of: (Int, String).self) { group in
        for i in 1...100 {
          group.addTask(executorPreference: executor) {
            (i, currentThreadPoolName())
          }
        }
        var sum = 0
        var threads: Set<String> = []
        for await (i, thread) in group {
          sum += i
          threads.insert(thread)
        }
        return (sum, threads)
      }
    }
    print("task executor sum: \(sum), threads: \(poolThreads.sorted().joined(separator: ","))")

    // An actor with a platform serial executor runs on its thread.
    // CHECK: serial executor count: 1000, threads: pe-serial
    await PlatformExecutorFactory.withSerialExecutor(name: "pe-serial") { executor in
      let counter = Counter(executor: executor)
      await withTaskGroup(of: Void.self) { group in
        for _ in 0..<1000 {
          group.addTask { await counter.increment() }
        }
      }
      let count = await counter.value
      let threads = await counter.threadNames
      print("serial executor count: \(count), threads: \(threads.sorted().joined(separator: ","))")
    }

    // CHECK: done
    print("done")
  }
}
