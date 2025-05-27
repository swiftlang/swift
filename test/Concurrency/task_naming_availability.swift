// RUN: %target-swift-frontend -emit-sil -verify -o /dev/null -verify %s

struct Boom: Error {}

@available(SwiftStdlib 6.2, *)
func testName() {
  _ = Task.name
}

@available(SwiftStdlib 6.0, *)
func taskExecutor() async {
  Task(name: "name", executorPreference: nil) { }
  Task(name: "name", executorPreference: nil) { throw Boom() }

  Task.detached(name: "name", executorPreference: nil) { throw Boom() }

  await withTaskGroup(of: Void.self) { group in
    group.addTask(name: "name", executorPreference: nil) {
      ()
    }
  }
  await withThrowingTaskGroup(of: Void.self) { group in
    group.addTask(name: "name", executorPreference: nil) {
      ()
    }
  }

  await withDiscardingTaskGroup { group in
    group.addTask(name: "name", executorPreference: nil) {
      ()
    }
  }
  try! await withThrowingDiscardingTaskGroup { group in
    group.addTask(name: "name", executorPreference: nil) {
      ()
    }
  }
}

@available(SwiftStdlib 5.1, *)
func backDeployedNames() async {
  Task(name: "name") { }
  Task(name: "name") { throw Boom() }

  Task.detached(name: "name") { }
  Task.detached(name: "name") { throw Boom() }

  await withTaskGroup(of: Void.self) { group in
    group.addTask(name: "name") {
      ()
    }
  }
  await withThrowingTaskGroup(of: Void.self) { group in
    group.addTask(name: "name") {
      ()
    }
  }
}

@available(SwiftStdlib 5.9, *)
func backDeployedDiscarding() async {
  await withDiscardingTaskGroup { group in
    group.addTask(name: "name") {
      ()
    }
  }
  try! await withThrowingDiscardingTaskGroup { group in
    group.addTask(name: "name") {
      ()
    }
  }
}

