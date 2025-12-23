// RUN: %target-run-simple-swift( -target %target-swift-5.1-abi-triple -parse-as-library -Xfrontend   -disable-availability-checking) 2>&1 | %FileCheck %s --dump-input=always
// REQUIRES: executable_test
// REQUIRES: concurrency

// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime

#if os(Linux)
import Glibc
#elseif os(Windows)
import MSVCRT
#elseif canImport(Android)
import Android
#else
import Darwin
#endif

final class NaiveQueueExecutor: TaskExecutor, SerialExecutor {
  init() {}

  public func enqueue(_ job: consuming ExecutorJob) {
    print("job task = \(job.unsafeCurrentTask)")
    print("job task name = \(String(describing: job.unsafeCurrentTask?.name))")

    job.runSynchronously(on: self.asUnownedSerialExecutor())
  }

  public func asUnownedSerialExecutor() -> UnownedSerialExecutor {
    UnownedSerialExecutor(ordinary: self)
  }
}

actor ActorOnNaiveQueueExecutor {
  let executor: NaiveQueueExecutor

  init() {
    self.executor = NaiveQueueExecutor()
  }

  nonisolated var unownedExecutor: UnownedSerialExecutor {
    self.executor.asUnownedSerialExecutor()
  }

  func test() async {
    try! await Task.sleep(for: .milliseconds(1))
    return
  }
}

@main struct Main {
  static func main() async {
    let ex = NaiveQueueExecutor()
    let task = Task(name: "Caplin the Task", executorPreference: ex) {
      print("Task.name = \(String(describing: Task.name))")
      // CHECK: Task.name = Optional("Caplin the Task")
      withUnsafeCurrentTask { task in
        print("unsafeTask.name = \(String(describing: task?.name))")
        // CHECK: unsafeTask.name = Optional("Caplin the Task")
      }

      await ActorOnNaiveQueueExecutor().test()
      // CHECK: job task name = Optional("Caplin the Task")

      print("inside")
    }

    _ = await task.value
    print("task.name = \(String(describing: task.name))")
    // CHECK: task.name = Optional("Caplin the Task")

    print("OK") // CHECK: OK
  }
}
