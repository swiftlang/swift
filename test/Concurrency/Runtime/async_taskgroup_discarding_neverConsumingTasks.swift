// RUN: %target-run-simple-swift( -Xfrontend -disable-availability-checking -parse-as-library) | %FileCheck %s --dump-input=always
// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime

// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: freestanding

// FIXME: enable discarding taskgroup on windows; rdar://104762037
// UNSUPPORTED: OS=windows-msvc

actor Waiter {
  let until: Int
  var count: Int

  var cc: CheckedContinuation<Int, Never>?

  init(until: Int) {
    self.until = until
    self.count = 0
  }

  func increment() {
    self.count += 1
    if self.until <= self.count {
      if let cc = self.cc {
        cc.resume(returning: self.count)
      }
    }
  }

  func wait() async -> Int {
    if self.until <= self.count {
      return self.count
    }

    return await withCheckedContinuation { cc in
      self.cc = cc
    }
  }
}

func test_discardingTaskGroup_neverConsume() async {
  print(">>> \(#function)")
  let until = 100
  let waiter = Waiter(until: until)

  print("Start tasks: \(until)")
  let allTasks = await withDiscardingTaskGroup() { group in
    for n in 1...until {
      group.addTask {
        try? await Task.sleep(until: .now + .milliseconds(100), clock: .continuous)
        await waiter.increment()
      }
    }

    return until
  }

  // CHECK: all tasks: 100
  print("all tasks: \(allTasks)")
}

func test_discardingTaskGroup_neverConsume(sleepBeforeGroupWaitAll: Duration) async {
  print(">>> \(#function)")
  let until = 100
  let waiter = Waiter(until: until)

  print("Start tasks: \(until)")
  let allTasks = await withDiscardingTaskGroup() { group in
    for n in 1...until {
      group.addTask {
        try? await Task.sleep(until: .now + .milliseconds(100), clock: .continuous)
        await waiter.increment()
      }
    }

    // wait a little bit, so some tasks complete before we hit the implicit "wait at end of task group scope"
    try? await Task.sleep(until: .now + sleepBeforeGroupWaitAll, clock: .continuous)

    return until
  }

  // CHECK: all tasks: 100
  print("all tasks: \(allTasks)")
}

func test_discardingTaskGroup_bigReturn() async {
  print(">>> \(#function)")

  // Test returning a very large value to ensure we don't overflow memory.
  let array = await withDiscardingTaskGroup { group in
    group.addTask {}
    try? await Task.sleep(until: .now + .milliseconds(100), clock: .continuous)
    return InlineArray<32768, Int>(repeating: 12345)
  }

  // CHECK: Huge return value produced: 12345 12345
  print("Huge return value produced:", array[0], array[32767])
}

@main struct Main {
  static func main() async {
    await test_discardingTaskGroup_neverConsume()
    await test_discardingTaskGroup_neverConsume(sleepBeforeGroupWaitAll: .milliseconds(500))
    await test_discardingTaskGroup_bigReturn()
  }
}
