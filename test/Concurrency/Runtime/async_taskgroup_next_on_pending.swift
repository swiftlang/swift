// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-concurrency) | %FileCheck %s --dump-input=always
// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: OS=macosx
// REQUIRES: CPU=x86_64

import Dispatch
import Darwin

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

func completeSlowly(n: Int) async -> Int {
  sleep(UInt32(n + 1))
  print("  complete group.add { \(n) }")
  return n
}

/// Tasks complete AFTER they are next() polled.
func test_sum_nextOnPending() {
  let numbers = [1, 2, 3, 4, 5]
  let expected = numbers.reduce(0, +)

  let taskHandle = launch { () async -> Int in
    return await try! Task.withGroup(resultType: Int.self) { (group) async -> Int in
      // await _taskPrintIDCurrent("TASK_ID: ADDING")
      for n in numbers {
        await group.add {
          // await _taskPrintIDCurrent("TASK_ID: ADDING-\(n) INSIDE")
          let res = await completeSlowly(n: n)
          // await _taskPrintIDCurrent("TASK_ID: ADDING-\(n) AFTER")
          return res
        }
      }

      var sum = 0
      print("before group.next(), sum: \(sum)")
      // await _taskPrintIDCurrent("TASK_ID: BEFORE NEXT")
      while let n = await try! group.next() {
        // await _taskPrintIDCurrent("TASK_ID: AFTER NEXT-\(n)")
        assert(numbers.contains(n), "Unexpected value: \(n)! Expected any of \(numbers)")
        print("next: \(n)")
        DispatchQueue.main.sync {
          sum += n
        }
        print("before group.next(), sum: \(sum)")
      }

      // await _taskPrintIDCurrent("TASK_ID: AFTER LOOP")
      print("task group returning: \(sum)")
      return sum
    }
  }

  // The completions are set apart by n seconds, so we expect them to arrive
  // in the order as the numbers (and delays) would suggest:
  //
  // CHECK: complete group.add { 1 }
  // CHECK: next: 1
  // CHECK: complete group.add { 2 }
  // CHECK: next: 2
  // CHECK: complete group.add { 3 }
  // CHECK: next: 3
  // CHECK: complete group.add { 4 }
  // CHECK: next: 4
  // CHECK: complete group.add { 5 }
  // CHECK: next: 5

  // CHECK: task group returning: 15

  launch { () async in
    let sum = await try! taskHandle.get()
    // CHECK: result: 15
    print("result: \(sum)")
    assert(sum == expected, "Expected: \(expected), got: \(sum)")
    exit(0)
  }

  print("main task")
}

test_sum_nextOnPending()

dispatchMain()
