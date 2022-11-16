// RUN: %target-run-simple-swift( -Xfrontend -disable-availability-checking -parse-as-library %import-libdispatch) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: libdispatch

@available(SwiftStdlib 5.1, *)
@discardableResult
func printTaskName(
    _ expected: String? = nil,
    file: String = #fileID, line: UInt = #line
) -> String? {
  let value = Task.name ?? "<undefined>"
  print("Task.name = \(value) at \(file):\(line)")
  if let expected = expected {
    assert("\(expected)" == "\(value)",
        "Expected [\(expected)] but found: \(value), at \(file):\(line)")
  }
  return expected
}

func canThrowButDoesnt() throws {}

// ==== ------------------------------------------------------------------------

@available(SwiftStdlib 5.1, *)
func unstructuredTaskNames() async throws {
  _ = await Task(name: "alpha") {
    printTaskName()
  }.value
  // CHECK: Task.name = alpha

  _ = await Task.detached(name: "alpha-detached") {
    printTaskName()
  }.value
  // CHECK: Task.name = alpha-detached

  try await Task(name: "alpha-throws") {
    printTaskName()
    try canThrowButDoesnt()
  }.value
  // CHECK: Task.name = alpha-throws

  try await Task.detached(name: "alpha-throws-detached") {
    printTaskName()
    try canThrowButDoesnt()
  }.value
  // CHECK: Task.name = alpha-throws-detached
}

@available(SwiftStdlib 5.1, *)
func groupTaskNames() async throws {
  printTaskName() // CHECK: Task.name = <undefined>

  // no value in parent, value in child
  let x1: Int = await withTaskGroup(of: Int.self) { group in
    group.addTask {
      printTaskName() // CHECK: <undefined>
      // inside the child task, set a value
      return 0
    }
    _ = await group.next()!

    group.addTask(name: "beta") {
      printTaskName() // CHECK: Task.name = beta
      return 0
    }
    _ = await group.next()!

    _ = group.addTaskUnlessCancelled(name: "beta-unless") {
      printTaskName() // CHECK: Task.name = beta-unless
      return 0
    }
    return await group.next()!
  }
  assert(x1 == 0)

  // value in parent and in groups
  let x2: Int = await Task(name: "gamma") {
    printTaskName() // CHECK: Task.name = gamma

    return await withTaskGroup(of: Int.self) { group in
      group.addTask {
        printTaskName() // CHECK: Task.name = gamma
        return 0
      }
      _ = await group.next()!

      _ = group.addTaskUnlessCancelled {
        printTaskName() // CHECK: Task.name = gamma
        return 0
      }
      return await group.next()!
    }
  }.value
  assert(x2 == 0)

  // throwing
  let x3: Int = try await withThrowingTaskGroup(of: Int.self) { group in
    group.addTask {
      printTaskName() // CHECK: Task.name = <undefined>
      return 0
    }
    _ = try await group.next()!

    group.addTask(name: "epsilon") {
      printTaskName() // CHECK: Task.name = epsilon
      return 0
    }
    _ = try await group.next()!

    _ = group.addTaskUnlessCancelled(name: "epsilon-unless") {
      printTaskName() // CHECK: Task.name = epsilon-unless
      return 0
    }
    return try await group.next()!
  }
  assert(x3 == 0)
}

@available(SwiftStdlib 5.1, *)
@main struct Main {
  static func main() async throws {
    try await unstructuredTaskNames()
    try await groupTaskNames()
  }
}
