// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -parse-as-library -O -Xfrontend -disable-availability-checking %import-libdispatch -o %t/split_continuation_handler_race
// RUN: %target-codesign %t/split_continuation_handler_race
// RUN: %target-run %t/split_continuation_handler_race

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: libdispatch
// REQUIRES: synchronization
// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: back_deploy_concurrency
// UNSUPPORTED: freestanding
// UNSUPPORTED: single_threaded_concurrency

// Cancels and escalates a task while it installs the handlers of a wait, and
// then resumes it. If the wait doesn't suspend, no handler runs. Otherwise the
// cancellation handler must run exactly once, and the last call of the
// escalation handler must report the priority the task had when it was resumed.
// The escalation handler must never run concurrently with itself, and no
// handler runs after the wait returned.

import Dispatch
import Synchronization

// Written by the task before it signals `started`, and read after that.
final class ContinuationBox: @unchecked Sendable {
  var continuation: Continuation<Int, Never>?
}

final class Observations: Sendable {
  let cancellations = Atomic<Int>(0)
  let inEscalationHandler = Atomic<Bool>(false)
  let lastReported = Atomic<UInt8>(0)
}

func run() {
  for i in 0..<2000 {
    let box = ContinuationBox()
    let observations = Observations()
    let started = DispatchSemaphore(value: 0)
    let waited = DispatchSemaphore(value: 0)
    let proceed = DispatchSemaphore(value: 0)

    let task = Task.detached(priority: .background) {
      let value = await withContinuation(of: Int.self, throwing: Never.self) {
        continuation, waiter in
        box.continuation = consume continuation
        started.signal()
        return await waiter.wait(
          onCancel: { _ in
            observations.cancellations.add(1, ordering: .relaxed)
          },
          onPriorityEscalated: { _, new in
            let (exchanged, _) = observations.inEscalationHandler
              .compareExchange(expected: false, desired: true,
                               ordering: .acquiring)
            precondition(exchanged, "iteration \(i): concurrent escalation calls")
            observations.lastReported.store(new.rawValue, ordering: .relaxed)
            observations.inEscalationHandler.store(false, ordering: .releasing)
          })
      }
      // Let the main thread escalate the task after the wait returned.
      waited.signal()
      proceed.wait()
      return value
    }

    started.wait()
    task.escalatePriority(to: .medium)
    task.cancel()
    box.continuation.take()!.resume(returning: i)

    // The handlers are gone once the wait returned, so this escalation must not
    // reach them.
    waited.wait()
    task.escalatePriority(to: .high)
    proceed.signal()

    let done = DispatchSemaphore(value: 0)
    Task.detached {
      let value = await task.value
      precondition(value == i)
      done.signal()
    }
    done.wait()

    let cancellations = observations.cancellations.load(ordering: .relaxed)
    let last = observations.lastReported.load(ordering: .relaxed)
    if cancellations == 0 && last == 0 {
      // The resume came before the wait, so the wait didn't suspend.
      continue
    }
    precondition(cancellations == 1,
                 "iteration \(i): cancellation handler ran \(cancellations) times")
    precondition(last == TaskPriority.medium.rawValue,
                 "iteration \(i): last escalation call reported \(last)")
  }
}

@main struct Main {
  static func main() {
    run()
    print("OK")
  }
}
