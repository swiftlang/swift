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
  print("start group.add { \(n) }")
  sleep(1)
  print("complete group.add { \(n) }")
  fputs("error: complete group.add { \(n) }\n", stderr)
  return n
}

/// Tasks complete AFTER they are next() polled.
func test_sum_nextOnPending() {
  let numbers = [1, 2, 3, 4, 5]
  let expected = numbers.reduce(0, +)

  let taskHandle = launch { () async -> Int in
    return await try! Task.withGroup(resultType: Int.self) { (group) async -> Int in
      for n in numbers {
        await group.add { await completeSlowly(n: n) }
      }

      var sum = 0
      print("before group.next(), sum: \(sum)")
      while let r = await try! group.next() {
        assert(numbers.contains(r), "Unexpected value: \(r)! Expected any of \(numbers)")
        print("next: \(r)")
        DispatchQueue.main.sync { // FIXME: remove once executors/actors are a thing; they should provide us a way to guarantee the safety here
          sum += r
        }
        print("before group.next(), sum: \(sum)")
      }

      print("task group returning: \(sum)")
      return sum
    }
  }

  // CHECK: complete group.add { [[REG1:[0-9]+]] }
  // CHECK: complete group.add { [[REG2:[0-9]+]] }
  // CHECK: complete group.add { [[REG3:[0-9]+]] }
  // CHECK: complete group.add { [[REG4:[0-9]+]] }
  // CHECK: complete group.add { [[REG5:[0-9]+]] }

  // CON: next: [[REG1]]
  // CON: next: [[REG2]]
  // CON: next: [[REG3]]
  // CON: next: [[REG4]]
  // CON: next: [[REG5]]

  // CHECK: task group returning: 15

  launch { () async in
    let sum = await try! taskHandle.get()
    // CHECK: result: 15
    print("result: \(sum)")
    assert(sum == expected)
    exit(0)
  }

  print("main task")
}

test_sum_nextOnPending()

dispatchMain()
