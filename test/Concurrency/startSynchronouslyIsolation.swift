// RUN: %target-build-swift -swift-version 6 %s -strict-concurrency=complete -Xfrontend -verify

// REQUIRES: concurrency

@available(SwiftStdlib 6.2, *)
func sync() -> Task<String, Never> {
  Task.startSynchronously {
    return ""
  }
}

@available(SwiftStdlib 6.2, *)
func async() async throws {
  let t1 = Task.startSynchronously {
    return ""
  }
  let _: String = await t1.value
  
  let t2: Task<String, Error> = Task.startSynchronously {
    throw CancellationError()
  }
  let _: String = try await t2.value

  await withTaskGroup(of: Int.self) { group in
    group.startTaskSynchronously { 1 }
    group.startTaskSynchronouslyUnlessCancelled { 2 }
  }
  await withThrowingTaskGroup(of: Int.self) { group in
    group.startTaskSynchronously { 1 }
    group.startTaskSynchronouslyUnlessCancelled { 2 }
  }
  await withDiscardingTaskGroup { group in
    group.startTaskSynchronously { }
    group.startTaskSynchronouslyUnlessCancelled { }
  }
  try await withThrowingDiscardingTaskGroup { group in
    group.startTaskSynchronously { }
    group.startTaskSynchronouslyUnlessCancelled { }
  }
}