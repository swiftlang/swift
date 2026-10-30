// RUN: %target-run-simple-swift(-parse-as-library -target %target-swift-6.2-abi-triple -enable-experimental-feature SendCalls) | %FileCheck %s

// REQUIRES: concurrency
// REQUIRES: executable_test
// REQUIRES: concurrency_runtime
// REQUIRES: swift_feature_SendCalls

// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: freestanding

// A 'send' call of a synchronous function runs it in a task whose operation
// is synchronous (see 'Builtin.createSynchronousDiscardingTask'): the task
// has no async context, and only a small task allocator slab sized for the
// copied task locals. It is still a real task: task locals, priority,
// cancellation and 'withUnsafeCurrentTask' work inside the callee, which also
// may allocate from the task, e.g. in a synchronous 'TaskLocal.withValue'

import Dispatch

struct Large: Sendable, Equatable {
  var a: (Int, Int, Int, Int, Int, Int, Int, Int) = (1, 2, 3, 4, 5, 6, 7, 8)
  var b: (Int, Int, Int, Int, Int, Int, Int, Int) = (9, 10, 11, 12, 13, 14, 15, 16)
  var c: (Int, Int, Int, Int, Int, Int, Int, Int) = (0, 0, 0, 0, 0, 0, 0, 0)
  var d: (Int, Int, Int, Int, Int, Int, Int, Int) = (0, 0, 0, 0, 0, 0, 0, 0)
  var tag: String

  static func == (lhs: Large, rhs: Large) -> Bool {
    lhs.tag == rhs.tag && lhs.a.0 == rhs.a.0 && lhs.b.7 == rhs.b.7
  }
}

enum Locals {
  @TaskLocal static var number: Int = 0
  @TaskLocal static var name: String = "<none>"
  @TaskLocal static var large: Large = Large(tag: "default")

  @TaskLocal static var l0: Int = -1
  @TaskLocal static var l1: Int = -1
  @TaskLocal static var l2: Int = -1
  @TaskLocal static var l3: Int = -1
  @TaskLocal static var l4: Int = -1
  @TaskLocal static var l5: Int = -1
  @TaskLocal static var l6: Int = -1
  @TaskLocal static var l7: Int = -1
  @TaskLocal static var l8: Int = -1
  @TaskLocal static var l9: Int = -1
}

func sumOfManyLocals() -> Int {
  Locals.l0 + Locals.l1 + Locals.l2 + Locals.l3 + Locals.l4 +
    Locals.l5 + Locals.l6 + Locals.l7 + Locals.l8 + Locals.l9
}

actor Recorder {
  var received: [Int] = []
  var lines: [String] = []

  func tell(_ n: Int) {
    received.append(n)
  }

  func record(_ line: String) {
    lines.append(line)
  }

  func observeLocals() {
    lines.append("number=\(Locals.number) name=\(Locals.name) " +
                 "large=\(Locals.large.tag) many=\(sumOfManyLocals())")
  }

  func observeTask(expectedBase: TaskPriority) {
    let line: String = withUnsafeCurrentTask { task in
      guard let task else { return "no task" }
      return "task=yes base-matches=\(task.basePriority == expectedBase) " +
             "priority-at-least-base=\(task.priority >= expectedBase)"
    }
    lines.append(line)
  }

  func cancelSelf() {
    let before = Task.isCancelled
    withUnsafeCurrentTask { $0?.cancel() }
    lines.append("cancelled before=\(before) after=\(Task.isCancelled)")
  }

  func bindLocally() {
    // A synchronous 'withValue' pushes a task-local item on the task's
    // allocator, which does not fit into the task's inline slab
    let outer = Locals.$large.withValue(Large(tag: "inner")) {
      Locals.$number.withValue(4242) {
        Locals.$name.withValue(String(repeating: "x", count: 200)) {
          "\(Locals.large.tag) \(Locals.number) \(Locals.name.count)"
        }
      }
    }
    lines.append("bound \(outer) restored=\(Locals.large.tag)")
  }

  func drain() -> ([Int], [String]) {
    let result = (received, lines)
    lines = []
    return result
  }
}

final class CountingSerialExecutor: SerialExecutor, @unchecked Sendable {
  let queue = DispatchSerialQueue(label: "counting")
  var enqueued = 0

  func enqueue(_ job: consuming ExecutorJob) {
    enqueued += 1
    let unowned = UnownedJob(job)
    queue.async {
      unowned.runSynchronously(on: self.asUnownedSerialExecutor())
    }
  }
}

actor CustomExecutorActor {
  let executor: CountingSerialExecutor
  var seen: [Int] = []

  init(executor: CountingSerialExecutor) {
    self.executor = executor
  }

  nonisolated var unownedExecutor: UnownedSerialExecutor {
    executor.asUnownedSerialExecutor()
  }

  func tell(_ n: Int) {
    executor.preconditionIsolated()
    seen.append(n)
  }

  func drain() -> [Int] { seen }
}

@MainActor var mainActorReceived: [Int] = []

@MainActor func tellMain(_ n: Int) {
  MainActor.preconditionIsolated()
  mainActorReceived.append(n)
}

/// Bind 'Locals.l<index>' ... 'Locals.l9' to their index around 'body'
func bindManyLocals(from index: Int, _ body: () async -> Void) async {
  let keys = [Locals.$l0, Locals.$l1, Locals.$l2, Locals.$l3, Locals.$l4,
              Locals.$l5, Locals.$l6, Locals.$l7, Locals.$l8, Locals.$l9]
  guard index < keys.count else {
    await body()
    return
  }
  await keys[index].withValue(index) {
    await bindManyLocals(from: index + 1, body)
  }
}

@main struct Main {
  static func main() async {
    let recorder = Recorder()

    // FIFO across many sends
    for i in 0 ..< 1000 {
      send recorder.tell(i)
    }
    let (received, _) = await recorder.drain()
    print("fifo: \(received.count) in order: \(received == Array(0 ..< 1000))")
    // CHECK: fifo: 1000 in order: true

    // Task locals, many and large values
    await Locals.$number.withValue(7) {
      await Locals.$name.withValue("seven") {
        await Locals.$large.withValue(Large(tag: "big")) {
          await bindManyLocals(from: 0) {
            // Shadow 'number' again, only the newest binding is copied
            await Locals.$number.withValue(8) {
              send recorder.observeLocals()
            }
          }
        }
      }
    }
    // No task locals at all
    send recorder.observeLocals()
    for line in await recorder.drain().1 { print(line) }
    // CHECK: number=8 name=seven large=big many=45
    // CHECK: number=0 name=<none> large=default many=-10

    // 'withUnsafeCurrentTask' and priority
    await Task(priority: .utility) {
      send recorder.observeTask(expectedBase: .utility)
    }.value
    await Task(priority: .userInitiated) {
      send recorder.observeTask(expectedBase: .userInitiated)
    }.value
    for line in await recorder.drain().1 { print(line) }
    // CHECK: task=yes base-matches=true priority-at-least-base=true
    // CHECK: task=yes base-matches=true priority-at-least-base=true

    // Cancellation through the unsafe handle is observed
    send recorder.cancelSelf()
    // Allocations from the task inside the callee, beyond the inline slab
    await Locals.$number.withValue(1) {
      send recorder.bindLocally()
    }
    for line in await recorder.drain().1 { print(line) }
    // CHECK: cancelled before=false after=true
    // CHECK: bound inner 4242 200 restored=default

    // A custom serial executor
    let executor = CountingSerialExecutor()
    let custom = CustomExecutorActor(executor: executor)
    for i in 0 ..< 100 {
      send custom.tell(i)
    }
    let seen = await custom.drain()
    print("custom: \(seen == Array(0 ..< 100)) enqueued: \(executor.enqueued >= 100)")
    // CHECK: custom: true enqueued: true

    // The main actor, sent from outside of it
    await Task.detached {
      for i in 0 ..< 10 {
        send tellMain(i)
      }
    }.value
    let main = await MainActor.run { mainActorReceived }
    print("main: \(main)")
    // CHECK: main: [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]

    print("done")
    // CHECK: done
  }
}
