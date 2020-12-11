// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-concurrency) | %FileCheck %s --dump-input always
// REQUIRES: executable_test
// REQUIRES: concurrency

func test_getPriority() {
  runAsyncAndBlock {
    let p = await Task.currentPriority()
    // CHECK: priority: default
    print("priority: \(p)")
    assert(p == Task.Priority.default)
  }
}

test_getPriority()
