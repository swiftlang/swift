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
  let numbers = [1, 2]
//  let numbers = [1, 2, 3, 4, 5]
  let expected = numbers.reduce(0, +)

  let taskHandle = launch { () async -> Int in
    return await try! Task.withGroup(resultType: Int.self) { (group) async -> Int in
      for n in numbers {
        await group.add { () async -> Int in
          print("  inside group.add { \(n) }")
          return n
        }
      }

      sleep(3)

      var sum = 0
      do {
//        while let r = await try group.next() { // FIXME: unlock it working on while
//          print("next: \(r)")
//          sum += r
//          print("sum: \(sum)")
//        }
        if let r = await try group.next() {
          print("next: \(r)")
          sum += r
          print("sum: \(sum)")
        }
        if let r = await try group.next() {
          print("next: \(r)")
          sum += r
          print("sum: \(sum)")
        }
      } catch {
        print("ERROR: \(error)")
      }

      print("task group returning: \(sum)")
      return sum
    }
  }

  // CHECK: main task
  // CHECK-DAG: inside group.add { [[N:[0-9]+]] }
  // CHECK-DAG: next: {{[0-9]+}}
  //
  // CHECK-DAG: inside group.add { [[N:[0-9]+]] }
  // CHECK-DAG: next: {{[0-9]+}}
  //
  // CHECK: sum: 3
  //
  // CHECK: task group returning: 3

  launch { () async in
    let sum = await try! taskHandle.get()
    // CHECK: result: 3
    print("result: \(sum)")
    exit(0)
  }

  print("main task")
  sleep(3)
}

test_sum_nextOnCompleted()

dispatchMain()
