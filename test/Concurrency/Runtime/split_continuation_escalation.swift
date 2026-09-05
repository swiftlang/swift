// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -Xfrontend -disable-availability-checking -parse-as-library -o %t/split_continuation_escalation -swift-version 6
// RUN: %target-codesign %t/split_continuation_escalation
// RUN: %target-run %t/split_continuation_escalation | %FileCheck %s

// REQUIRES: VENDOR=apple
// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime
// REQUIRES: libdispatch
// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: back_deploy_concurrency

// Priority escalation is only delivered where the platform supports it; the
// simulators are excluded for the same reason as async_task_escalate_priority.
// UNSUPPORTED: DARWIN_SIMULATOR=watchos
// UNSUPPORTED: DARWIN_SIMULATOR=ios
// UNSUPPORTED: DARWIN_SIMULATOR=tvos

// wait(onCancel:onEscalate:) installs both handlers in one step, so one
// suspension can be both escalated and cancelled: the escalation handler
// sees the new priority, and the cancellation handler then resumes the
// continuation.

@_spi(Concurrency) import _Concurrency
@preconcurrency import Dispatch
import Darwin

// Holds the resume half of a split continuation so another thread can
// resume it.
final class ContinuationHolder<Success: ~Copyable, Failure: Error>: @unchecked Sendable {
  private var continuation: Continuation<Success, Failure>?

  init(_ continuation: consuming Continuation<Success, Failure>) {
    self.continuation = consume continuation
  }

  func take() -> Continuation<Success, Failure> {
    continuation.take()!
  }
}

// Handlers run holding the status-record lock, so they can't resume inline.
extension ContinuationHolder where Success: Sendable {
  func resumeFromDetachedTask(returning value: Success) {
    Task.detached { [self] in
      self.take().resume(returning: value)
    }
  }
}

final class Box<Value>: @unchecked Sendable {
  var value: Value
  init(_ value: Value) { self.value = value }
}

@main struct Main {
  static func main() async {
    // Off the main actor: this blocks on semaphores, which would otherwise
    // deadlock the actor the child task needs.
    await Task.detached {
      let suspended = DispatchSemaphore(value: 0)
      let escalated = DispatchSemaphore(value: 0)
      let observedPriority = Box<TaskPriority?>(nil)

      let task = Task(priority: .background) { () -> Int in
        await withContinuation(of: Int.self, throwing: Never.self) {
          (continuation: consuming Continuation<Int, Never>,
           awaiter: consuming ContinuationAwaiter<Int, Never>) in
          let holder = ContinuationHolder(continuation)
          suspended.signal()
          return await awaiter.wait(
            onCancel: { holder.resumeFromDetachedTask(returning: 7) },
            onEscalate: { newPriority in
              observedPriority.value = newPriority
              escalated.signal()
            })
        }
      }

      suspended.wait()
      usleep(100_000)

      task.escalatePriority(to: .medium)
      escalated.wait()

      // The cancellation handler is still installed, so cancelling now
      // resumes the continuation.
      task.cancel()

      let result = await task.value
      precondition(result == 7, "continuation did not resume: \(result)")
      precondition(observedPriority.value == .medium,
                   "escalation handler saw \(String(describing: observedPriority.value))")
      // CHECK: OK
      print("OK")
    }.value
  }
}
