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
    let tests = TestSuite("TaskGroup: ~Copyable ChildTaskResult")

    if #available(SwiftStdlib 6.5, *) {
      tests.test("TaskGroup<~Copyable> next() drains both tasks") {
        let total = await withTaskGroup(of: UniqueResource.self) { group in
          group.addTask { UniqueResource(10) }
          group.addTask { UniqueResource(32) }
          var sum = 0
          while let r = await group.next() {
            sum += r.value
            _ = consume r
          }
          return sum
        }
        expectEqual(42, total)
      }

      tests.test("ThrowingTaskGroup<~Copyable> success then failure") {
        do {
          try await withThrowingTaskGroup(of: UniqueResource.self) { group in
            group.addTask { UniqueResource(7) }
            group.addTask { () async throws -> UniqueResource in
              throw TestError()
            }
            var seen = 0
            while let r = try await group.next() {
              seen += r.value
              _ = consume r
            }
            expectUnreachable("expected to throw before the loop terminates")
            _ = seen
          }
          expectUnreachable("expected throw out of group body")
        } catch is TestError {
          // expected
        } catch {
          expectUnreachable("unexpected error: \(error)")
        }
      }

      tests.test("TaskGroup<~Copyable> cancelAll then drain") {
        let drained = await withTaskGroup(of: UniqueResource.self) { group in
          group.addTask { UniqueResource(1) }
          group.addTask { UniqueResource(2) }
          group.cancelAll()
          var count = 0
          while let r = await group.next() {
            count += 1
            _ = consume r
          }
          return count
        }
        // The closures don't check cancellation, so both still produce values.
        expectEqual(2, drained)
      }
      tests.test("ThrowingTaskGroup<~Copyable> nextResult() reports success and failure") {
        var seenSuccess = 0
        var seenFailure = 0
        do {
          try await withThrowingTaskGroup(of: UniqueResource.self) { group in
            group.addTask { UniqueResource(11) }
            group.addTask { () async throws -> UniqueResource in throw TestError() }
            while let result = await group.nextResult() {
              switch consume result {
              case .success(let r):
                seenSuccess += r.value
                _ = consume r
              case .failure:
                seenFailure += 1
              }
            }
          }
        } catch {
          expectUnreachable("nextResult should not rethrow")
        }
        expectEqual(11, seenSuccess)
        expectEqual(1, seenFailure)
      }

      tests.test("TaskGroup<~Copyable> addTaskUnlessCancelled accepts when not cancelled") {
        let total = await withTaskGroup(of: UniqueResource.self) { group in
          let added1 = group.addTaskUnlessCancelled { UniqueResource(3) }
          let added2 = group.addTaskUnlessCancelled { UniqueResource(4) }
          expectTrue(added1)
          expectTrue(added2)
          var sum = 0
          while let r = await group.next() {
            sum += r.value
            _ = consume r
          }
          return sum
        }
        expectEqual(7, total)
      }

      tests.test("TaskGroup<~Copyable> addTaskUnlessCancelled rejects after cancelAll") {
        let drained = await withTaskGroup(of: UniqueResource.self) { group in
          group.cancelAll()
          let added = group.addTaskUnlessCancelled { UniqueResource(99) }
          expectFalse(added)
          var count = 0
          while let r = await group.next() {
            count += 1
            _ = consume r
          }
          return count
        }
        expectEqual(0, drained)
      }

      tests.test("TaskGroup<~Copyable> waitForAll drains pending children") {
        await withTaskGroup(of: UniqueResource.self) { group in
          for i in 0..<5 {
            group.addTask { UniqueResource(i) }
          }
          await group.waitForAll()
          expectTrue(group.isEmpty)
        }
      }

      tests.test("ThrowingTaskGroup<~Copyable> waitForAll surfaces a thrown child") {
        do {
          try await withThrowingTaskGroup(of: UniqueResource.self) { group in
            group.addTask { UniqueResource(1) }
            group.addTask { () async throws -> UniqueResource in throw TestError() }
            try await group.waitForAll()
            expectUnreachable("waitForAll should rethrow the child failure")
          }
          expectUnreachable("expected throw out of group body")
        } catch is TestError {
          // expected
        } catch {
          expectUnreachable("unexpected error: \(error)")
        }
      }

      tests.test("ThrowingTaskGroup<~Copyable> addTaskUnlessCancelled rejects after cancelAll") {
        do {
          try await withThrowingTaskGroup(of: UniqueResource.self) { group in
            group.cancelAll()
            let added = group.addTaskUnlessCancelled {
              () async throws -> UniqueResource in UniqueResource(13)
            }
            expectFalse(added)
            try await group.waitForAll()
          }
        } catch {
          expectUnreachable("group should drain cleanly after rejected add")
        }
      }

      tests.test("TaskGroup<~Copyable> isEmpty / isCancelled state transitions") {
        await withTaskGroup(of: UniqueResource.self) { group in
          expectTrue(group.isEmpty)
          expectFalse(group.isCancelled)
          group.addTask { UniqueResource(1) }
          expectFalse(group.isEmpty)
          group.cancelAll()
          expectTrue(group.isCancelled)
          while let r = await group.next() {
            _ = consume r
          }
          expectTrue(group.isEmpty)
        }
      }

      tests.test("TaskGroup<~Copyable> fan-out: 32 children all delivered exactly once") {
        let sum = await withTaskGroup(of: UniqueResource.self) { group in
          for i in 1...32 {
            group.addTask { UniqueResource(i) }
          }
          var total = 0
          var count = 0
          while let r = await group.next() {
            total += r.value
            count += 1
            _ = consume r
          }
          expectEqual(32, count)
          return total
        }
        expectEqual((1 + 32) * 32 / 2, sum)
      }

      tests.test("TaskGroup<~Copyable> multi-phase add/drain reuses the group") {
        let total = await withTaskGroup(of: UniqueResource.self) { group in
          group.addTask { UniqueResource(1) }
          group.addTask { UniqueResource(2) }
          var phase1 = 0
          while let r = await group.next() {
            phase1 += r.value
            _ = consume r
          }
          expectTrue(group.isEmpty)

          group.addTask { UniqueResource(10) }
          group.addTask { UniqueResource(20) }
          var phase2 = 0
          while let r = await group.next() {
            phase2 += r.value
            _ = consume r
          }
          return phase1 + phase2
        }
        expectEqual(33, total)
      }
    }

    await runAllTestsAsync()
    print("done")
    // CHECK: done
  }
}
