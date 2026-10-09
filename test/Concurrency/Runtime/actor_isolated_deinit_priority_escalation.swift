// RUN: %target-run-simple-swift(-O -target %target-future-triple %import-libdispatch) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime
// REQUIRES: libdispatch
// UNSUPPORTED: back_deployment_runtime

// Ensure that releasing the last reference to an actor with an isolated deinit
// while another job is running on the actor does not crash.
// rdar://188980139

import Dispatch
import Synchronization

actor Victim {
  let finished = Atomic<Bool>(false)

  isolated deinit {}

  func markFinished() {
    finished.store(true, ordering: .releasing)
  }

  nonisolated func spawnLowPriorityWork() {
    Task(priority: .utility) { await self.markFinished() }
  }
}

let workers = 4
let duration: UInt64 = 3_000_000_000
let deadline = DispatchTime.now().uptimeNanoseconds + duration

DispatchQueue.concurrentPerform(iterations: workers) { _ in
  DispatchQueue.global(qos: .userInitiated).sync {
    while DispatchTime.now().uptimeNanoseconds < deadline {
      for _ in 0..<1000 {
        let v = Victim()
        v.spawnLowPriorityWork()
        while !v.finished.load(ordering: .acquiring) {}
      }
    }
  }
}

// CHECK: done
print("done")
