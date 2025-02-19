// RUN: %target-run-simple-swift( -Xfrontend -disable-availability-checking) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency

// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime

func pretendToThrow() throws {}

func test() async {
  // CHECK: Task.name = NONE OK
  print("Task.name = \(Task.name ?? "NONE OK")")

  _ = await Task(name: "Caplin the Task") {
    // CHECK: Task.name = Caplin the Task
    print("Task.name = \(Task.name ?? "NONE")")
    return 12
  }.value

  _ = try? await Task(name: "Caplin the Throwing Task") {
    // CHECK: Task.name = Caplin the Throwing Task
    print("Task.name = \(Task.name ?? "NONE")")
    try pretendToThrow()
    await Task {
      // CHECK: Does not inherit Task.name = NONE OK
      print("Does not inherit Task.name = \(Task.name ?? "NONE OK")")
    }.value
    return 12
  }.value

  _ = await Task.detached(name: "Caplin the Detached Task") {
    // CHECK: Task.name = Caplin the Detached Task
    print("Task.name = \(Task.name ?? "NONE")")
    return 12
  }.value

  _ = try? await Task.detached(name: "Caplin the Detached Throwing Task") {
    // CHECK: Task.name = Caplin the Detached Throwing Task
    print("Task.name = \(Task.name ?? "NONE")")
    try pretendToThrow()
    return 12
  }.value

  _ = await withTaskGroup(of: Int.self) { g in
    g.addTask(
      name: "Caplin the TaskGroup Task",
      executorPreference: nil) {
      // CHECK: Task.name = Caplin the TaskGroup Task
      print("Task.name = \(Task.name ?? "NONE")")
      return 12
    }
    _ = await g.next()
    _ = g.addTaskUnlessCancelled(
      name: "Caplin the TaskGroup Task (unless cancelled)",
      executorPreference: nil) {
      // CHECK: Task.name = Caplin the TaskGroup Task (unless cancelled)
      print("Task.name = \(Task.name ?? "NONE")")
      return 12
    }
  }

  _ = await withThrowingTaskGroup(of: Int.self) { g in
    g.addTask(
      name: "Caplin the ThrowingTaskGroup Task",
      executorPreference: nil) {
      // CHECK: Task.name = Caplin the ThrowingTaskGroup Task
      print("Task.name = \(Task.name ?? "NONE")")
      return 12
    }
    _ = try? await g.next()
    _ = g.addTaskUnlessCancelled(
      name: "Caplin the ThrowingTaskGroup Task (unless cancelled)",
      executorPreference: nil) {
      // CHECK: Task.name = Caplin the ThrowingTaskGroup Task (unless cancelled)
      print("Task.name = \(Task.name ?? "NONE")")
      return 12
    }
  }

  _ = await withDiscardingTaskGroup { g in
    g.addTask(
      name: "Caplin the DiscardingTaskGroup Task",
      executorPreference: nil) {
      // CHECK: Task.name = Caplin the DiscardingTaskGroup Task
      print("Task.name = \(Task.name ?? "NONE")")
    }
  }
  _ = await withDiscardingTaskGroup { g in
    _ = g.addTaskUnlessCancelled(
      name: "Caplin the DiscardingTaskGroup Task (unless cancelled)",
      executorPreference: nil) {
      // CHECK: Task.name = Caplin the DiscardingTaskGroup Task (unless cancelled)
      print("Task.name = \(Task.name ?? "NONE")")
    }
  }
  _ = try? await withThrowingDiscardingTaskGroup { g in
    g.addTask(
      name: "Caplin the ThrowingDiscardingTaskGroup Task",
      executorPreference: nil) {
      // CHECK: Task.name = Caplin the ThrowingDiscardingTaskGroup Task
      print("Task.name = \(Task.name ?? "NONE")")
    }
  }
  _ = try? await withThrowingDiscardingTaskGroup { g in
    _ = g.addTaskUnlessCancelled(
      name: "Caplin the ThrowingDiscardingTaskGroup Task (unless cancelled)",
      executorPreference: nil) {
      // CHECK: Task.name = Caplin the ThrowingDiscardingTaskGroup Task (unless cancelled)
      print("Task.name = \(Task.name ?? "NONE")")
    }
  }
}

await test()
