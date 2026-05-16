// RUN: %target-run-simple-swift( -target %target-swift-5.1-abi-triple -parse-as-library) 2>&1 | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime

import _Concurrency
import StdlibUnittest

struct UniqueResource: ~Copyable, Sendable {
  let value: Int
  init(_ value: Int) { self.value = value }
  deinit { print("UniqueResource(\(value)).deinit") }
}

struct TestError: Error {}

@main struct Main {
  static func main() async {
    let tests = TestSuite("Task: ~Copyable Success")

    if #available(SwiftStdlib 6.5, *) {
      tests.test("take nonthrowing Task<~Copyable, Never>") {
        let task = Task { UniqueResource(42) }
        let resource = await task.take()
        print("got: \(resource.value)")
        // CHECK: got: 42
        expectEqual(42, resource.value)
        _ = consume resource
        // CHECK: UniqueResource(42).deinit
      }

      tests.test("take throwing Task<~Copyable, any Error> success") {
        let task = Task { () async throws -> UniqueResource in UniqueResource(7) }
        do {
          let resource = try await task.take()
          print("got: \(resource.value)")
          // CHECK: got: 7
          expectEqual(7, resource.value)
          _ = consume resource
          // CHECK: UniqueResource(7).deinit
        } catch {
          expectUnreachable()
        }
      }

      tests.test("take throwing Task<~Copyable, any Error> failure") {
        let task = Task { () async throws -> UniqueResource in throw TestError() }
        do {
          let _ = try await task.take()
          expectUnreachable()
        } catch {
          // Expected
        }
      }

      tests.test("cancel Task<~Copyable, Never>") {
        let task = Task { UniqueResource(99) }
        task.cancel()
        let resource = await task.take()
        // The closure runs to completion regardless of cancellation since it
        // doesn't check; we just want to confirm cancel() is callable.
        expectEqual(99, resource.value)
        _ = consume resource
      }

      tests.test("Task<~Copyable, Never> identity / hashing") {
        let task = Task { UniqueResource(123) }
        expectTrue(task == task)
        var hasher = Hasher()
        task.hash(into: &hasher)
        let resource = await task.take()
        _ = consume resource
      }

      tests.test("Task.detached for ~Copyable Success") {
        let task: Task<UniqueResource, Never> =
          Task.detached { UniqueResource(99) }
        let resource = await task.take()
        expectEqual(99, resource.value)
        _ = consume resource
      }

      tests.test("Task.init with name for ~Copyable Success") {
        let task: Task<UniqueResource, Never> =
          Task(name: "nc-test") { UniqueResource(55) }
        let resource = await task.take()
        expectEqual(55, resource.value)
        _ = consume resource
      }

      tests.test("take while task body still executing routes through async completion fill") {
        // Suspending in the body forces `take()` to hit Status::Executing,
        // exercising completeFuture -> fillWithSuccess(wantsTake: true).
        let task = Task { () async -> UniqueResource in
          try? await Task.sleep(nanoseconds: 1_000_000)
          return UniqueResource(77)
        }
        let resource = await task.take()
        expectEqual(77, resource.value)
        _ = consume resource
      }

      tests.test("throwing take while task body still executing routes through async completion fill") {
        let task = Task { () async throws -> UniqueResource in
          try? await Task.sleep(nanoseconds: 1_000_000)
          return UniqueResource(88)
        }
        do {
          let resource = try await task.take()
          expectEqual(88, resource.value)
          _ = consume resource
        } catch {
          expectUnreachable("unexpected error: \(error)")
        }
      }

      tests.test("Task<Int, Never>.take then .value on alias traps") {
        expectCrashLater()
        let task = Task { 42 }
        let alias = task
        _ = await task.take()
        // alias still holds a reference to the same future, but the result
        // was moved out by take(); reading .value must trap rather than
        // observe moved-from storage.
        _ = await alias.value
      }

      tests.test("Task<Int, Never>.take twice on alias traps") {
        expectCrashLater()
        let task = Task { 7 }
        let alias = task
        _ = await task.take()
        // Second take() must trap on the claim flag.
        _ = await alias.take()
      }
    }

    await runAllTestsAsync()
  }
}
