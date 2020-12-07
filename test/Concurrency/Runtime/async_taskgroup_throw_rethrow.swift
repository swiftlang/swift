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

func test_taskGroup_throws_rethrows() {
  let taskHandle = launch {
    return await try Task.withGroup(resultType: Int.self) { (group) async throws -> Int in
      await group.add { await one() }
      await group.add { await one() }
      await group.add { await try boom() }

      do {
        while let r = await try group.next() {
          print("next: \(r)")
        }
      } catch {
        print("error caught and rethrown in group: \(error)")
        throw error
      }

      fatalError("should have thrown")
    }
  }

  launch { () async in
    do {
      let got = await try taskHandle.get()
      print("got: \(got)")
      exit(1)
    } catch {
      print("rethrown: \(error)")
      exit(0)
    }
  }

  print("main task")
}


// CHECK: main task
// CHECK: error caught and rethrown in group: Boom()
// CHECK: rethrown: Boom()
test_taskGroup_throws_rethrows()

dispatchMain()
