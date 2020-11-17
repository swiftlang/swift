// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-concurrency) | %FileCheck %s
// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: OS=macosx

import Dispatch

extension DispatchQueue {
  func async<R>(execute: @escaping () async -> R) -> Task.Handle<R> {
    let handle = Task.runDetached(operation: execute)

    // Run the task
    _ = { self.async { handle.run() } }()

    return handle
  }
}

func test(name: String) {
  let taskHandle = DispatchQueue.main.async { () async -> String in
    return "Hello \(name) from async world"
  }

  _ = DispatchQueue.main.async { () async in
    // CHECK: Sleeping
    print("Sleeping...")
    sleep(2)
    let result = await try! taskHandle.get()
    // CHECK: Hello Ted from async world
    print(result)
    assert(result == "Hello Ted from async world")
    exit(0)
  }

  print("Main task")
}

test(name: "Ted")

dispatchMain()
