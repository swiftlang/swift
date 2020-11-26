// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-concurrency) | %FileCheck %s --dump-input always
// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: OS=macosx
// REQUIRES: CPU=x86_64

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

@available(*, deprecated, message: "This is a temporary hack")
@discardableResult
func launch<R>(operation: @escaping () async throws -> R) -> Task.Handle<R> {
  let handle = Task.runDetached(operation: operation)

  // Run the task
  DispatchQueue.global(priority: .default).async { handle.run() }

  return handle
}

struct Boom: Error {}
struct IgnoredBoom: Error {}

func one() async -> Int { 1 }
func boom() async throws -> Int { throw Boom() }

// ==== ------------------------------------------------------------------------
// MARK: Tests

func test_taskGroup_throws() {
  let taskHandle = launch { () async throws -> Int in
    return await try Task.withGroup(resultType: Int.self) { group async throws -> Int in
      await group.add { await one() }
      await group.add { await try boom() }

      do {
        while let r = await try group.next() {
          print("next: \(r)")
        }
      } catch {
        print("error caught in group: \(error)")

        await group.add { () async -> Int in
          print("task 3 (cancelled: \(await Task.isCancelled()))")
          return 3
        }

        guard let got = await try! group.next() else {
          print("task group failed to get 3 (:\(#line))")
          return 0
        }

        print("task group next: \(got)")

        if got == 1 {
          // the previous 1 completed before the 3 we just submitted,
          // we still want to see that three so let's await for it
          guard let third = await try! group.next() else {
            print("task group failed to get 3 (:\(#line))")
            return got
          }

          print("task group returning normally: \(third)")
          return third
        } else {
          print("task group returning normally: \(got)")
          return got
        }
      }

      fatalError("Should have thrown and handled inside the catch block")
    }
  }

  // CHECK: main task
  // Optionally, we may get the first result back before the failure:
  // COM: next: 1
  // CHECK: error caught in group: Boom()
  // CHECK: task 3 (cancelled: false)
  // CHECK: task group returning normally: 3
  // CHECK: got: 3

  launch {
    do {
      let got = await try taskHandle.get()
      print("got: \(got)")
      exit(0)
    } catch {
      print("rethrown: \(error)")
      exit(1)
    }
  }

  print("main task")
}

test_taskGroup_throws()

dispatchMain()
