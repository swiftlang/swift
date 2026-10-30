// RUN: %target-run-simple-swift(%import-libdispatch -parse-as-library)

// REQUIRES: concurrency
// REQUIRES: executable_test
// REQUIRES: libdispatch

// rdar://106849189 move-only types should be supported in freestanding mode
// UNSUPPORTED: freestanding

// UNSUPPORTED: back_deployment_runtime
// REQUIRES: concurrency_runtime

import Dispatch
import StdlibUnittest

// The runtime function that other code (and the old Task.sleep) uses to ask
// the global executor to run a job at a point in time.
@_silgen_name("swift_task_enqueueGlobalWithDeadline")
func _enqueueGlobalWithDeadline(_ seconds: Int64, _ nanoseconds: Int64,
                                _ toleranceSeconds: Int64,
                                _ toleranceNanoseconds: Int64,
                                _ clock: Int32, _ job: UnownedJob)

@_silgen_name("swift_get_time")
func _getTime(_ seconds: UnsafeMutablePointer<Int64>,
              _ nanoseconds: UnsafeMutablePointer<Int64>,
              _ clock: Int32)

/// A task executor that delays the first job it is given by calling
/// `swift_task_enqueueGlobalWithDeadline()`, and runs anything after that
/// straight away.
@available(SwiftStdlib 6.0, *)
final class DeadlineExecutor: TaskExecutor, @unchecked Sendable {
  let clock: Int32
  let delayNanoseconds: Int64
  let toleranceNanoseconds: Int64

  private let lock = DispatchSemaphore(value: 1)
  private var scheduled = false

  init(clock: Int32, delayNanoseconds: Int64, toleranceNanoseconds: Int64) {
    self.clock = clock
    self.delayNanoseconds = delayNanoseconds
    self.toleranceNanoseconds = toleranceNanoseconds
  }

  func enqueue(_ job: consuming ExecutorJob) {
    lock.wait()
    let alreadyScheduled = scheduled
    scheduled = true
    lock.signal()

    if alreadyScheduled {
      UnownedJob(job).runSynchronously(on: asUnownedTaskExecutor())
      return
    }

    var seconds: Int64 = 0
    var nanoseconds: Int64 = 0
    _getTime(&seconds, &nanoseconds, clock)

    // The deadline is absolute, on the clock we were given
    nanoseconds += delayNanoseconds
    seconds += nanoseconds / 1_000_000_000
    nanoseconds %= 1_000_000_000

    _enqueueGlobalWithDeadline(seconds, nanoseconds,
                               0, toleranceNanoseconds,
                               clock, UnownedJob(job))
  }
}

@available(SwiftStdlib 6.0, *)
func measure(clock: Int32, toleranceNanoseconds: Int64) async -> Duration {
  let executor = DeadlineExecutor(clock: clock,
                                  delayNanoseconds: 100_000_000,
                                  toleranceNanoseconds: toleranceNanoseconds)
  let start = ContinuousClock.now
  await Task(executorPreference: executor) {}.value
  return ContinuousClock.now - start
}

@available(SwiftStdlib 6.0, *)
@main struct Main {
  static func main() async {
    let tests = TestSuite("enqueue_global_with_deadline")

    // The deadline is a time on the clock, not a delay: before this was fixed,
    // a deadline of (say) a million seconds since boot would be treated as a
    // delay of a million seconds.
    for (name, clock) in [("continuous", Int32(1)), ("suspending", Int32(2))] {
      tests.test("A deadline on the \(name) clock is honoured, with no tolerance") {
        let elapsed = await measure(clock: clock, toleranceNanoseconds: -1)
        expectTrue(elapsed >= .milliseconds(90), "ran early: \(elapsed)")
        expectTrue(elapsed < .seconds(120), "ran late: \(elapsed)")
      }

      tests.test("A deadline on the \(name) clock is honoured, with a tolerance") {
        let elapsed = await measure(clock: clock,
                                    toleranceNanoseconds: 10_000_000)
        expectTrue(elapsed >= .milliseconds(90), "ran early: \(elapsed)")
        expectTrue(elapsed < .seconds(120), "ran late: \(elapsed)")
      }
    }

    await runAllTestsAsync()
  }
}
