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

func asyncInt() async -> Int {
  42
}

// ==== ------------------------------------------------------------------------
// MARK: Tests

func test_taskGroup_isEmpty() {
  // CHECK: main task

  let taskHandle = launch { () async -> Int in
    return await try! Task.withGroup(resultType: Int.self) { (group) async -> Int in
      // CHECK: before add: isEmpty=true
      print("before add: isEmpty=\(group.isEmpty)")

//      await group.add {
//        print("inside run")
//        defer { print("completed run") }
//        sleep(2)
//        return await asyncInt()
//      }
//      // TODO: CH ECK: after add: isEmpty=false
//      print("after add: isEmpty=\(group.isEmpty)")

      return 0
    }
  }

  launch { () async in
    _ = await try! taskHandle.get()
    exit(0)
  }

  print("main task")
}

test_taskGroup_isEmpty()

dispatchMain()
