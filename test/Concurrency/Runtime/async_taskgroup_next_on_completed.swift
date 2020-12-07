// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-concurrency) | %FileCheck %s --dump-input=always
// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: OS=macosx
// REQUIRES: CPU=x86_64

import Dispatch

// ==== ------------------------------------------------------------------------
// MARK: "Infrastructure" for the tests

extension DispatchQueue {
  func async<R>(operation: @escaping () async -> R) -> Task.Handle<R> {
    let handle = Task.runDetached(operation: operation)

    // Run the task
    _ = { self.async { handle.run() } }() // force invoking the non-async version

    return handle
  }
}

@available(*, deprecated, message: "This is a temporary hack")
@discardableResult
func launch<R>(operation: @escaping () async -> R) -> Task.Handle<R> {
  let handle = Task.runDetached(operation: operation)

  // Run the task
  DispatchQueue.global(priority: .default).async { handle.run() }

  return handle
}

// ==== ------------------------------------------------------------------------
// MARK: Tests

/// Tasks complete before they are next() polled.
func test_sum_nextOnCompleted() {
  let numbers = [1, 2, 3, 4, 5]
  let expected = numbers.reduce(0, +)

  let taskHandle = launch { () async -> Int in
    return await try! Task.withGroup(resultType: Int.self) { (group) async -> Int in
      for n in numbers {
        await group.add { () async -> Int in
          print("  complete group.add { \(n) }")
          return n
        }
      }

      // We specifically want to await on completed child tasks in this test,
      // so give them some time to complete before we hit group.next()
      sleep(3)

      var sum = 0
      do {
        while let r = await try group.next() {
          print("next: \(r)")
          DispatchQueue.main.sync { // TODO: remove once executors/actors are a thing
            sum += r
          }
          print("sum: \(sum)")
        }
      } catch {
        print("ERROR: \(error)")
      }

      assert(group.isEmpty, "Group must be empty after we consumed all tasks")

      print("task group returning: \(sum)")
      return sum
    }
  }

  // CHECK: main task

  // The completions may arrive in any order, we make no strong guarantees about it:
  // CHECK-DAG: complete group.add { [[N1:[0-9]+]] }
  // CHECK-DAG: complete group.add { [[N2:[0-9]+]] }
  // CHECK-DAG: complete group.add { [[N3:[0-9]+]] }
  // CHECK-DAG: complete group.add { [[N4:[0-9]+]] }
  // CHECK-DAG: complete group.add { [[N5:[0-9]+]] }
  // CHECK-DAG: next: [[N1]]
  // CHECK-DAG: next: [[N2]]
  // CHECK-DAG: next: [[N3]]
  // CHECK-DAG: next: [[N4]]
  // CHECK-DAG: next: [[N5]]

  // CHECK: sum: 15
  //
  // CHECK: task group returning: 15

  launch { () async in
    let sum = await try! taskHandle.get()
    // CHECK: result: 15
    print("result: \(sum)")
    exit(0)
  }

  print("main task")
}

test_sum_nextOnCompleted()

dispatchMain()
