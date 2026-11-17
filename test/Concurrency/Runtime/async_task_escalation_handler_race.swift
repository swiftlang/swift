// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -parse-as-library -O %import-libdispatch -o %t/escalation_handler_race
// RUN: %target-codesign %t/escalation_handler_race
// RUN: %target-run %t/escalation_handler_race

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: libdispatch
// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: back_deploy_concurrency
// UNSUPPORTED: freestanding
// UNSUPPORTED: single_threaded_concurrency

// Escalation notifies exactly the priority escalation handlers that were
// installed when the task's priority changed: not one installed after the task
// observed the new priority, and not one removed before the escalation finished.

import Dispatch

final class Flag: @unchecked Sendable {
  var value = false
}

@available(SwiftStdlib 6.2, *)
func run() {
  for i in 0..<1000 {
    let started = DispatchSemaphore(value: 0)
    let done = DispatchSemaphore(value: 0)
    let firstHandlerRan = Flag()
    let task = Task.detached(priority: .background) {
      await withTaskPriorityEscalationHandler {
        started.signal()
        while Task.currentPriority != .medium {}
      } onPriorityEscalated: { _, _ in
        firstHandlerRan.value = true
      }
      await withTaskPriorityEscalationHandler {
        for _ in 0..<20_000 { _ = Task.currentPriority }
      } onPriorityEscalated: { old, new in
        fatalError("iteration \(i): escalated from \(old) to \(new) after the task saw \(new)")
      }
      done.signal()
    }
    started.wait()
    task.escalatePriority(to: .medium)
    done.wait()
    if !firstHandlerRan.value {
      fatalError("iteration \(i): escalation within a handler's scope did not run it")
    }
  }
}

@main struct Main {
  static func main() {
    if #available(SwiftStdlib 6.2, *) {
      run()
    }
  }
}
