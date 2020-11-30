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

func asyncInt() async -> Int {
  42
}

// ==== ------------------------------------------------------------------------
// MARK: Tests

func test_taskGroup_isEmpty() {
  // CHECK: main task

  let taskHandle = DispatchQueue.main.async { () async -> Int in
    return await try! Task.withGroup(resultType: Int.self) { (group) async -> Int in
      // CHECK: before add: isEmpty=true
      print("before add: isEmpty=\(group.isEmpty)")

      await group.add {
        sleep(2)
        return await asyncInt()
      }

      // CHECK: while add running, outside: isEmpty=false
      // CHECK: inside add: isEmpty=false
      print("while add running, outside: isEmpty=\(group.isEmpty)")

      while let value = await try! group.next() {
        print("next: \(value)")
        sleep(1)
      }

      // CHECK: after draining tasks: isEmpty=true
      print("after draining tasks: isEmpty=\(group.isEmpty)")

      return 0
    }
  }

  print("main task")
  sleep(10)
}

test_taskGroup_isEmpty()

dispatchMain()
