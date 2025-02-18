// RUN: %target-run-simple-swift( -Xfrontend -disable-availability-checking) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency

// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime

func test() async {
  // CHECK: Task.name = NONE
  print("Task.name = \(Task.name ?? "NONE")")

  _ = await Task(name: "Caplin the Task") {
    // CHECK: Task.name = Caplin the Task
    print("Task.name = \(Task.name ?? "NONE")")
    return 12
  }.value

//  _ = await Task.detached(name: "Caplin the Detached Task") {
//    // CHECK: Task.name = Caplin the Detached Task
//    print("Task.name = \(Task.name ?? "NONE")")
//    return 12
//  }.value

  _ = await withTaskGroup(of: Int.self) { g in
    g.addTask(name: "Caplin the TaskGroup Task") {
      // CHECK: Task.name = Caplin the TaskGroup Task
      print("Task.name = \(Task.name ?? "NONE")")
      return 12
    }
  }

//  _ = await withThrowingTaskGroup(of: Int.self) { g in
//    g.addTask(name: "Caplin the ThrowingTaskGroup Task") {
//      // CHECK: Task.name = Caplin the ThrowingTaskGroup Task
//      print("Task.name = \(Task.name ?? "NONE")")
//      return 12
//    }
//  }
}

await test()
