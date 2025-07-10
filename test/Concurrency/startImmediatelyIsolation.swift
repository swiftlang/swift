// RUN: %target-swift-frontend -parse-as-library -swift-version 6 -emit-sil -verify %s
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
    _ = group.addImmediateTaskUnlessCancelled { 2 }
  }
  await withThrowingTaskGroup(of: Int.self) { group in
    group.addImmediateTask { () async throws -> Int in 1 }
    _ = group.addImmediateTaskUnlessCancelled { () async throws -> Int in 2 }
  }
  await withDiscardingTaskGroup { group in
    group.addImmediateTask { }
    _ = group.addImmediateTaskUnlessCancelled { }
  }
  try await withThrowingDiscardingTaskGroup { group in
    group.addImmediateTask { () async throws -> Void in }
    _ = group.addImmediateTaskUnlessCancelled { () async throws -> Void in }
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

@available(SwiftStdlib 6.2, *)
struct TestThrowing {
  func test() {
    // expected-error@+1{{invalid conversion from throwing function of type '() throws -> Void' to non-throwing function type '@isolated(any) () async -> Void'}}
    let t: Task<Void, Never> = Task.immediate {
      throw Boom()
    }
    _ = t

    // ok
    let t2: Task<Void, Error> = Task.immediate {
      throw Boom()
    }
    _ = t2
  }
}

struct Boom: Error {}
