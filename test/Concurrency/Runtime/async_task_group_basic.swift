// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-concurrency) | %FileCheck %s --dump-input always
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

func test_taskGroup_01_sum() {
  let numbers = [1, 2, 3, 4, 5]
  let expected = numbers.reduce(0, +)

  let taskHandle = launch { () async -> Int in
    return await try! Task.withGroup(resultType: Int.self) { (group) async -> Int in
      for n in numbers {
        print("before group.add: \(n)")
        await group.add { () async -> Int in
          print("-- inside group.add { \(n) }")
          return n
        }
      }

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
  // CHECK: next: 1
  // CHECK: next: 2
  // CHECK: next: 3
  // CHECK: task group returning: 3

  launch { () async in
    let sum = await try! taskHandle.get()
    // CHECK: result: 3
    print("result: \(sum)")
//    assert(expected == sum, "Got: \(sum), expected: \(expected)")
    exit(0)
  }

  print("main task")
}

test_taskGroup_01_sum()

dispatchMain()
