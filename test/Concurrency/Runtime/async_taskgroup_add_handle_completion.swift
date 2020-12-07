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

func completeImmediately(n: Int) async -> Int {
  print("complete group.add { \(n) }")
  return n
}

func completeSlowly(n: Int) async -> Int {
  sleep(3)
  print("complete group.add { \(n) }")
  return n
}

/// Tasks complete AFTER they are next() polled.
func test_sum_nextOnPending() {
  let taskHandle = launch { () async -> Int in
    return await try! Task.withGroup(resultType: Int.self) { (group) async -> Int in
      let firstHandle = await group.add {
        let res = await completeSlowly(n: 1)
        return res
      }
      let secondHandle = await group.add {
        let res = await completeImmediately(n: 2)
        return res
      }
      let thirdHandle = await group.add {
        let res = await completeSlowly(n: 3)
        return res
      }

      let first = await try! firstHandle.get()
      print("firstHandle.get(): \(first)")
      let second = await try! secondHandle.get()
      print("secondHandle.get(): \(second)")
      let third = await try! thirdHandle.get()
      print("thirdHandle.get(): \(third)")

      var sum = 0
      print("before group.next(), sum: \(sum)")
      while let n = await try! group.next() {
        assert(n <= 3, "Unexpected value: \(n)! Expected <= 3")
        print("next: \(n)")
        DispatchQueue.main.sync { // FIXME: remove once executors/actors are a thing; they should provide us a way to guarantee the safety here
          sum += n
        }
        print("before group.next(), sum: \(sum)")
      }

      print("task group returning: \(sum)")
      return sum
    }
  }

  // CHECK: firstHandle.get(): 1
  // CHECK: secondHandle.get(): 2
  // CHECK: thirdHandle.get(): 3

  // CHECK: task group returning: 6

  launch { () async in
    let sum = await try! taskHandle.get()
    // CHECK: result: 6
    print("result: \(sum)")
    assert(sum == 6)
    exit(0)
  }

  print("main task")
}

test_sum_nextOnPending()

dispatchMain()
