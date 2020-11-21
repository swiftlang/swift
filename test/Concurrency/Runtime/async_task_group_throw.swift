// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-concurrency) | %FileCheck %s --dump-input always
// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: OS=macosx

import Dispatch

// ==== ------------------------------------------------------------------------
// MARK: "Infrastructure" for the tests

extension DispatchQueue {
  func async<R>(operation: @escaping () async throws -> R) -> Task.Handle<R> {
    let handle = Task.runDetached(operation: operation)

    // Run the task
    _ = { self.async { handle.run() } }() // force invoking the non-async version

    return handle
  }
}

struct Boom: Error {}
struct IgnoredBoom: Error {}

func one() async -> Int { 1 }

// ==== ------------------------------------------------------------------------
// MARK: Tests

func test_taskGroup_01_throw() {
  let taskHandle = DispatchQueue.main.async { () async throws -> Int in
    return await try Task.withGroup(resultType: Int.self) { (group) async throws -> Int in
      await group.add { await one() }
      await group.add { () async throws -> Int in throw Boom() }

      do {
        while let r = await try group.next() {
          print("next: \(r)")
        }
      } catch {
        print("error: \(error)")

        await group.add { () async throws -> Int in
            print("task 3 checking isCancelled")
          if await Task.isCancelled() {
            print("task 3 cancelled")
            throw IgnoredBoom() // shall be ignored, we're already throwing the initial error
          } else {
            print("task 3 completed normally")
            return 3
          }
        }

        throw error
      }

      print("task group returning normally")
      return 1
    }
  }

  // CHECK: main task
  // CHECK: next: 1
  // CHECK: error: Boom()
  // CHECK: task 3 cancelled

  DispatchQueue.main.async { () async in
    do {
      _ = await try taskHandle.get()
    } catch {
      // CHECK: rethrown: Boom()
      print("rethrown: \(error)")
      exit(0)
    }
    assert(false, "should have thrown")
    exit(1)
  }

  print("main task")
}

test_taskGroup_01_throw()

dispatchMain()
