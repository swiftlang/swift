// RUN: %target-run-simple-swift( -Xfrontend -disable-availability-checking) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency

// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime

func task() async {
  _ = await Task(debugName: "Caplin the Task") {
    // CHECK: Task(...).debugName = Caplin the Task
    print("Task(...).debugName = \(Task.name ?? "NONE")")
    return 12
  }.value
}

await task()
