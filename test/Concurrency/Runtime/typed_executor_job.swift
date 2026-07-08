// RUN: %target-run-simple-swift( -Xfrontend -disable-availability-checking %import-libdispatch -parse-as-library ) | %FileCheck %s
// REQUIRES: concurrency
// REQUIRES: executable_test
// REQUIRES: concurrency_runtime
// UNSUPPORTED: freestanding
// UNSUPPORTED: back_deployment_runtime

// Exercises the sleep-continuation path end to end on the default (Dispatch)
// executor: registerOperation -> withExecutorContinuation ->
// ContinuousClockExecutor.enqueue, with the executor resuming the
// ExecutorContinuation (success on fire, throwing CancellationError on cancel).

import Dispatch
import _Concurrency

@main struct Main {
  static func main() async {
    // 1. Normal completion.
    let start = DispatchTime.now()
    do {
      try await Task.sleep(nanoseconds: 50_000_000)   // 50ms
    } catch {
      print("unexpected error: \(error)")
    }
    let ms = Double(DispatchTime.now().uptimeNanoseconds
                    - start.uptimeNanoseconds) / 1_000_000
    // CHECK: slept
    print(ms >= 40 ? "slept (>=40ms)" : "slept too short: \(ms)")

    // 2. Cancellation is delivered as CancellationError through the buffer.
    let t = Task {
      do {
        try await Task.sleep(nanoseconds: 5_000_000_000)
        print("NOT cancelled")
      } catch is CancellationError {
        print("cancelled OK")
      } catch {
        print("other error: \(error)")
      }
    }
    t.cancel()
    await t.value
    // CHECK-NEXT: cancelled OK

    // 2b. Reaching a sleep while the task is *already* cancelled.  The first
    // sleep observes cancellation; its error is swallowed with `try?`, but the
    // task stays cancelled, so the *second* sleep is armed on an
    // already-cancelled task.  This exercises the immediate-fire branch of
    // swift_task_addExecutorCancellationHandler (fire cancel(jobID) at arm,
    // install no record) and the matching no-op removal after resumption.
    let preCancelled = Task {
      try? await Task.sleep(nanoseconds: 5_000_000_000)
      do {
        try await Task.sleep(nanoseconds: 5_000_000_000)
        print("second sleep NOT cancelled")
      } catch is CancellationError {
        print("pre-cancelled OK")
      } catch {
        print("second sleep other error: \(error)")
      }
    }
    preCancelled.cancel()
    await preCancelled.value
    // CHECK-NEXT: pre-cancelled OK

    // 3. Several sequential typed sleeps complete.
    var n = 0
    for _ in 0..<3 {
      try? await Task.sleep(nanoseconds: 10_000_000)
      n += 1
    }
    // CHECK-NEXT: sequential 3
    print("sequential \(n)")

    // 4. Stress the fire-vs-cancel race.  Start many short sleeps of staggered
    // durations, wait ~1ms so some have already fired, then cancel them all --
    // so cancellations land both before and *after* each sleep's fire.  This
    // exercises registerOperation -> arm -> fire racing cancel,
    // and in particular the deregister sweep of the spurious
    // cancelled-early marker left when a cancel loses that race (rdar timer
    // tombstone leak).  Every task must resolve; the process must not hang.
    var tasks: [Task<Void, Never>] = []
    for i in 0..<200 {
      tasks.append(Task {
        try? await Task.sleep(nanoseconds: UInt64(i % 8) * 250_000 + 250_000)
      })
    }
    try? await Task.sleep(nanoseconds: 1_000_000)
    for t in tasks { t.cancel() }
    for t in tasks { await t.value }
    // CHECK-NEXT: stress 200
    print("stress \(tasks.count)")

    // CHECK-NEXT: done
    print("done")
  }
}
