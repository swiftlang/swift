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

func completeSlowly(n: Int) async -> Int {
  print("start group.add { \(n) }")
  sleep(1)
  print("complete group.add { \(n) }")
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
      do {
        print("before group.next(), sum: \(sum)")
        if let r = await try group.next() {
          print("next: \(r)")
          sum += r
        }

        print("before group.next(), sum: \(sum)")
        if let r = await try group.next() {
          print("next: \(r)")
          sum += r
        }

        print("before group.next(), sum: \(sum)")
        if let r = await try group.next() {
          print("next: \(r)")
          sum += r
        }

        print("before group.next(), sum: \(sum)")
        if let r = await try group.next() {
          print("next: \(r)")
          sum += r
        }

        print("before group.next(), sum: \(sum)")
        if let r = await try group.next() {
          print("next: \(r)")
          sum += r
        }

//        print("before group.next(), sum: \(sum)")
//        if let r = await try group.next() {
//          print("next: \(r)")
//          sum += r
//        }
      } catch {
        print("ERROR: \(error)")
      }

      print("task group returning: \(sum)")
      return sum
    }
  }

  // CHECK: before group.next(), sum: 0
  //
  // COM: start group.add { 1 }
  // COM: complete group.add { 1 }
  // COM: next: 1
  //
  // COM: start group.add { 2 }
  // COM: complete group.add { 2 }
  // COM: next: 2
  //
  // COM: start group.add { 3 }
  // COM: complete group.add { 3 }
  // COM: next: 3
  //
  // CHECK: task group returning: 15

  launch { () async in
    let sum = await try! taskHandle.get()
    // CHECK: result: 3
    print("result: \(sum)")
    exit(0)
  }

  print("main task")
  sleep(20)
  exit(-1)
}

test_sum_nextOnPending()

dispatchMain()
