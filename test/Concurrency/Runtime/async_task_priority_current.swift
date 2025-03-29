// RUN: %target-run-simple-swift( -target %target-swift-5.1-abi-triple %import-libdispatch -parse-as-library) | %FileCheck --dump-input=always %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: libdispatch

// rdar://76038845
// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime

import Dispatch

@available(SwiftStdlib 5.1, *)
@main struct Main {
  static func main() async {
    print("main priority: \(Task.currentPriority)") // CHECK: main priority: TaskPriority.medium
    await test_detach()
    await test_multiple_lo_indirectly_escalated()
  }
}

@available(SwiftStdlib 5.1, *)
func test_detach() async {
  let a1 = Task.currentPriority
  print("a1: \(a1)") // CHECK: a1: TaskPriority.medium

  // Note: remember to detach using a higher priority, otherwise a lower one
  // might be escalated by the get() and we could see `default` in the detached
  // task.
  await detach(priority: .userInitiated) {
    let a2 = Task.currentPriority
    print("a2: \(a2)") // CHECK: a2: TaskPriority.high
  }.get()

  let a3 = Task.currentPriority
  print("a3: \(a3)") // CHECK: a3: TaskPriority.medium
}

@available(SwiftStdlib 5.1, *)
func test_multiple_lo_indirectly_escalated() async {
  @Sendable
  func loopUntil(priority: TaskPriority) async {
    while (Task.currentPriority != priority) {
      await Task.sleep(1_000_000_000)
    }
  }

  let z = detach(priority: .background) {
    await loopUntil(priority: .userInitiated)
  }
  let x = detach(priority: .background) {
    _ = await z // waiting on `z`, but it won't complete since we're also background
    await loopUntil(priority: .userInitiated)
  }

  // detach, don't wait
  detach(priority: .userInitiated) {
    await x // escalates x, which waits on z, so z also escalates
  }

  // since `_` awaited from userInitiated on `x` we:
  // - boost `x` to `userInitiated`
  // and then since `x` waits on `z`
  // - `z` also gets boosted to `userInitiated`
  // which "unlocks" it, allowing the 'default' `await z` to complete:
  await x
  await z
  print("default done") // CHECK: default done
}


