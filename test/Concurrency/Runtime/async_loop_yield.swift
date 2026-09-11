// RUN: %target-run-simple-swift(-Xfrontend -enable-async-loop-yield -Xfrontend -disable-availability-checking -parse-as-library) | %FileCheck %s
// RUN: %target-run-simple-swift(-Xfrontend -enable-async-loop-yield -Xfrontend -disable-availability-checking -parse-as-library -O) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: freestanding

// Exercises the executor yields that AsyncLoopYieldInsertion puts on the
// back-edges of loops containing suspension points (forced on here so the
// semantics are checked on every platform, not only on those that need the
// yields to bound native stack usage).

// Runs on the caller's executor and completes synchronously, so without the
// inserted yields the loops below never reach the executor's run loop.
@inline(never)
nonisolated(nonsending) func invoke(_ operation: () -> Int) async -> Int {
  operation()
}

struct Counting: AsyncSequence {
  typealias Element = Int
  let n: Int
  struct AsyncIterator: AsyncIteratorProtocol {
    var i = 0
    let n: Int
    mutating func next() async -> Int? {
      guard i < n else { return nil }
      defer { i += 1 }
      return i
    }
  }
  func makeAsyncIterator() -> AsyncIterator { AsyncIterator(n: n) }
}

actor Counter {
  var value = 0

  func run(iterations: Int) async -> Int {
    for _ in 0..<iterations {
      value += await invoke { 1 }
      // The yield must bring us back onto this actor.
      self.assertIsolated()
    }
    return value
  }
}

enum Locals {
  @TaskLocal static var marker: Int = 0
}

/// A serial executor that counts its enqueues and runs jobs inline.
final class CountingSerialExecutor: SerialExecutor, @unchecked Sendable {
  var enqueues = 0

  func enqueue(_ job: consuming ExecutorJob) {
    enqueues += 1
    job.runSynchronously(on: asUnownedSerialExecutor())
  }

  func asUnownedSerialExecutor() -> UnownedSerialExecutor {
    UnownedSerialExecutor(ordinary: self)
  }
}

actor CustomExecutorCounter {
  let executor = CountingSerialExecutor()
  var value = 0

  nonisolated var unownedExecutor: UnownedSerialExecutor {
    executor.asUnownedSerialExecutor()
  }

  func run(iterations: Int) async -> (value: Int, enqueues: Int) {
    let enqueuesBefore = executor.enqueues
    for _ in 0..<iterations {
      value += await invoke { 1 }
      // Each yield must have re-enqueued the task on this actor's executor
      // and resumed it there.
      self.assertIsolated()
    }
    return (value, executor.enqueues - enqueuesBefore)
  }
}

/// A task executor that counts its enqueues and runs jobs inline.
final class CountingTaskExecutor: TaskExecutor, @unchecked Sendable {
  var enqueues = 0

  func enqueue(_ job: consuming ExecutorJob) {
    enqueues += 1
    job.runSynchronously(on: asUnownedTaskExecutor())
  }

  func asUnownedTaskExecutor() -> UnownedTaskExecutor {
    UnownedTaskExecutor(ordinary: self)
  }
}

/// Nonisolated, so that under a task executor preference it runs on the task
/// executor (the main function itself is isolated to the main actor).
func loopOnTaskExecutor(_ executor: CountingTaskExecutor,
                        iterations: Int) async -> (sum: Int, enqueues: Int, stayed: Bool) {
  let enqueuesBefore = executor.enqueues
  var sum = 0
  var stayed = true
  for i in 0..<iterations {
    sum += await invoke { i }
    withUnsafeCurrentTask { task in
      stayed = stayed &&
        task!.unownedTaskExecutor == executor.asUnownedTaskExecutor()
    }
  }
  return (sum, executor.enqueues - enqueuesBefore, stayed)
}

@MainActor var otherMainActorJobRan = false

@MainActor
func mainActorLoop() async -> Int {
  // Enqueued on the main executor behind the current task; it can only run
  // once this loop yields.
  Task { @MainActor in otherMainActorJobRan = true }

  var firstInterleave = -1
  var sum = 0
  for i in 0..<100 {
    sum += await invoke { i }
    MainActor.assertIsolated()
    if otherMainActorJobRan && firstInterleave < 0 {
      firstInterleave = i
    }
  }
  print("main actor sum: \(sum), first interleave at iteration \(firstInterleave)")
  print("other job ran: \(otherMainActorJobRan)")
  return sum
}

@main struct Main {
  static func main() async {
    // The shape from swiftlang/swift#91940: a long loop of synchronously
    // completing awaits.
    var result = 0
    for index in 0..<100_000 {
      result = await invoke { index }
    }
    // CHECK: loop result: 99999
    print("loop result: \(result)")

    // A synchronous AsyncSequence through stdlib combinators.
    var elements = 0
    var last = -1
    for await x in Counting(n: 100_000).map({ $0 * 2 }).filter({ $0 % 4 == 0 }) {
      elements += 1
      last = x
    }
    // CHECK: sequence count: 50000, last: 199996
    print("sequence count: \(elements), last: \(last)")

    // A while loop with the await in its condition.
    var iterator = Counting(n: 100_000).makeAsyncIterator()
    var count = 0
    while let _ = await iterator.next() {
      count += 1
    }
    // CHECK: while count: 100000
    print("while count: \(count)")

    // Actor isolation is restored after each yield.
    let counter = Counter()
    // CHECK: actor value: 10000
    print("actor value: \(await counter.run(iterations: 10_000))")

    // Task-local values survive the yields.
    let marker = await Locals.$marker.withValue(42) {
      var seen = 0
      for _ in 0..<1_000 {
        _ = await invoke { 0 }
        seen = Locals.marker
      }
      return seen
    }
    // CHECK: task local: 42
    print("task local: \(marker)")

    // A yield inside an actor with a custom executor goes through that
    // executor, once per iteration.
    let custom = CustomExecutorCounter()
    let (customValue, customEnqueues) = await custom.run(iterations: 100)
    // CHECK: custom executor: value 100, one enqueue per iteration: true
    print("custom executor: value \(customValue), one enqueue per iteration: \(customEnqueues == 100)")

    // A yield under a task executor preference goes through the task
    // executor, once per iteration, and the loop stays on it.
    let taskExecutor = CountingTaskExecutor()
    let taskExecutorResult = await withTaskExecutorPreference(taskExecutor) {
      await loopOnTaskExecutor(taskExecutor, iterations: 100)
    }
    // CHECK: task executor: sum 4950, one enqueue per iteration: true, stayed: true
    print("task executor: sum \(taskExecutorResult.sum), one enqueue per iteration: \(taskExecutorResult.enqueues == 100), stayed: \(taskExecutorResult.stayed)")

    // Yields let other jobs on the same executor run between iterations.
    // CHECK: main actor sum: 4950, first interleave at iteration 1
    // CHECK: other job ran: true
    _ = await mainActorLoop()
  }
}
