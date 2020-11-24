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
  let numbers = [2, 1, 3]
  let expected = numbers.reduce(0, +)

  let taskHandle = launch { () async -> Int in
    return await try! Task.withGroup(resultType: Int.self) { (group) async -> Int in
      let one = await group.add {
        await try launch { () async -> Int in
          sleep(3) // TODO: reimplement with completing them in order
          return 1
        }.get()
      }
      let two = await group.add {
        await try launch { () async -> Int in
          sleep(2) // TODO: reimplement with completing them in order
          return 2
        }.get()
      }
      let three = await group.add {
        await try launch { () async -> Int in
          sleep(2) // TODO: reimplement with completing them in order
          return 3
        }.get()
      }

      var sum = 0
      while let r = await try! group.next() {
        print("next: \(r)")
        sum += r
      }

      print("task group returning: \(sum)")
      return sum
    }
  }

  // CHECK: main task
  // CHECK: next: 1
  // CHECK: next: 2
  // CHECK: task group returning: 3

  launch { () async in
    let sum = await try! taskHandle.get()
    // CHECK: result: 3
    print("result: \(sum)")
    assert(expected == sum)
    exit(0)
  }

  print("main task")
}

test_taskGroup_01_sum()

dispatchMain()
