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
func test_taskGroup_01_sum() {
  let numbers = [1]
//  let numbers = [1, 2, 3, 4, 5]
  let expected = numbers.reduce(0, +)

  let taskHandle = launch { () async -> Int in
    return await try! Task.withGroup(resultType: Int.self) { (group) async -> Int in
      for n in numbers {
        print("before group.add: \(n)")
        await group.add { () async -> Int in
          print("  inside group.add { \(n) }")
          return n
        }
      }

      sleep(1)

      var sum = 0
      print("before group.next(), sum: \(sum)")
      while let r = await try! group.next() {
        print("next: \(r)")
        sum += r
        print("before group.next(), sum: \(sum)")
      }

      print("task group returning: \(sum)")
      return sum
    }
  }

  // CHECK: main task
  // CHECK: before group.add: 1
  // CHECK: before group.add: 2
  // CHECK: before group.add: 3
  // CHECK: before group.add: 4
  // CHECK: before group.add: 5
  // CHECK: inside group.add { 1 }
  // CHECK: inside group.add { 2 }
  // CHECK: inside group.add { 3 }
  // COM: next: 1
  // COM: next: 2
  // COM: next: 3
  // COM: task group returning: 3

  launch { () async in
    let sum = await try! taskHandle.get()
    // CHECK: result: 3
    print("result: \(sum)")
//    assert(expected == sum, "Got: \(sum), expected: \(expected)")
    exit(0)
  }

  print("main task")
  sleep(5)
}

test_taskGroup_01_sum()

dispatchMain()
