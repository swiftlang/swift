// RUN: %target-run-simple-swift(-target %target-swift-5.1-abi-triple -enable-actor-data-race-checks -swift-version 5 -strict-concurrency=minimal) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime
// UNSUPPORTED: freestanding

@preconcurrency @MainActor
protocol P {
  func requirement()
}

class C: P {
  func requirement() {
    call {
      print("don't crash!")
    }
  }

  var task: Task<Void, Never>?

  @preconcurrency func call(closure: @escaping @Sendable () -> Void) {
    task = Task.detached {
      closure()
    }
  }

  func wait() async {
    await task?.value
  }
}

// CHECK: don't crash!
let c = C()
c.requirement()
await c.wait()
