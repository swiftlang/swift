// RUN: %target-run-simple-swift( -Xfrontend -disable-availability-checking %import-libdispatch -parse-as-library ) | %FileCheck %s
// REQUIRES: concurrency
// REQUIRES: executable_test
// REQUIRES: concurrency_runtime
// UNSUPPORTED: freestanding
// UNSUPPORTED: back_deployment_runtime

// Exercises the *job*-based `ContinuousClockExecutor.enqueue(_:for:at:tolerance:)`
// path end to end on the default (Dispatch) executor:
//   registerOperation() -> enqueue(job, for:at:) -> the Swift-owned
//   DispatchTimerScheduler arms a dispatch timer, and on fire runs the job.
//
// `Task.sleep` / `Clock.sleep` drive the *continuation* enqueue; this test
// drives the *job* overload, which otherwise has no in-tree caller now that the
// global delayed-job runtime hooks (`swift_task_enqueueGlobalWithDeadline`)
// are retired.  The only way to get hold of an `ExecutorJob` is to let the
// runtime hand one to a custom executor, so we funnel a task's jobs through a
// `TaskExecutor` that re-schedules each of them via the timed job `enqueue`.

@_spi(ExperimentalScheduling) @_spi(ExperimentalCustomExecutors) import _Concurrency
import Dispatch

// A `TaskExecutor` that runs every job it is handed `delay` in the future, by
// registering an operation on the default clock executor and enqueuing the job
// through the job-based timed `enqueue`.
final class DelayingTaskExecutor: TaskExecutor, @unchecked Sendable {
  let delay: Duration
  init(delay: Duration) { self.delay = delay }

  func enqueue(_ job: consuming ExecutorJob) {
    guard let clock = Task.defaultExecutor as? any ContinuousClockExecutor else {
      fatalError("default executor is not a ContinuousClockExecutor")
    }
    let registration = clock.registerOperation()
    let deadline = ContinuousClock().now.advanced(by: delay)
    clock.enqueue(job, for: registration, at: deadline, tolerance: nil)
  }
}

@main struct Main {
  static func main() async {
    let executor = DelayingTaskExecutor(delay: .milliseconds(50))
    let start = DispatchTime.now()

    // The task's jobs are enqueued on `executor`, which re-schedules each one
    // through the timed job `enqueue`.  The task therefore only makes progress
    // once a dispatch timer fires and the scheduler runs the job -- so reaching
    // the far side of the `yield` proves the job path scheduled, fired, and ran.
    await withTaskExecutorPreference(executor) {
      await Task.yield()
    }

    let ms = Double(DispatchTime.now().uptimeNanoseconds
                    - start.uptimeNanoseconds) / 1_000_000

    // The job must actually have gone through the timer (not run inline), so at
    // least one `delay` must have elapsed.
    // CHECK: job ran after delay: true
    print("job ran after delay: \(ms >= 30)")

    // CHECK-NEXT: done
    print("done")
  }
}
