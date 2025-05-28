// RUN: %target-build-swift -swift-version 6 %s -strict-concurrency=complete -Xfrontend -verify

// REQUIRES: concurrency

@available(SwiftStdlib 6.2, *)
func sync() -> Task<String, Never> {
  Task.immediate {
    return ""
  }
}

@available(SwiftStdlib 6.2, *)
func async() async throws {
  let t1 = Task.immediate {
    return ""
  }
  let _: String = await t1.value
  
  let t2: Task<String, Error> = Task.immediate {
    throw CancellationError()
  }
  let _: String = try await t2.value

  await withTaskGroup(of: Int.self) { group in
    group.addImmediateTask { 1 }
    group.addImmediateTaskUnlessCancelled { 2 }
  }
  await withThrowingTaskGroup(of: Int.self) { group in
    group.addImmediateTask { () async throws -> Int in 1 }
    group.addImmediateTaskUnlessCancelled { () async throws -> Int in 2 }
  }
  await withDiscardingTaskGroup { group in
    group.addImmediateTask { }
    group.addImmediateTaskUnlessCancelled { }
  }
  try await withThrowingDiscardingTaskGroup { group in
    group.addImmediateTask { () async throws -> Void in }
    group.addImmediateTaskUnlessCancelled { () async throws -> Void in }
  }
}

@available(SwiftStdlib 6.2, *)
actor TestSelfCapture {
    func method() {}

    func test() {
        Task.immediate {
            method() // Ok due to `@_implicitSelfCapture`
        }
    }
}
