// RUN: %empty-directory(%t)
// RUN: %target-build-swift -g %s %S/Inputs/SignpostMonitor.swift -o %t/a.out -target %target-swift-5.1-abi-triple -parse-as-library -Xfrontend -disable-availability-checking -Onone
// RUN: %target-codesign %t/a.out
// RUN: env %env-SWIFT_CONCURRENCY_TRACING_SUBSYSTEM=org.swift.concurrency.test.tracing %target-run %t/a.out

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime
// REQUIRES: OS=macosx
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

// Verify the signpost of a task that a task stealer starts. Task stealers only
// exist where the runtime supports priority escalation. Elsewhere the test
// checks the signpost of the task itself.

@_spi(ExperimentalCustomExecutors) @_spi(ExperimentalScheduling) import _Concurrency
import Darwin
import StdlibUnittest
import Synchronization

// The platform's default executor, which holds the jobs enqueued on it while
// `hold()` is in effect.
final class HoldingDefaultExecutor: TaskExecutor, SchedulingExecutor, Sendable {
  private let platform: any TaskExecutor = PlatformExecutorFactory.defaultExecutor
  private let held = Mutex<[UnownedJob]?>(nil)

  func enqueue(_ job: consuming ExecutorJob) {
    let job = UnownedJob(job)
    let isHeld = held.withLock { jobs in
      guard jobs != nil else { return false }
      jobs!.append(job)
      return true
    }
    if !isHeld {
      platform.enqueue(job)
    }
  }

  func enqueue<C: Clock>(_ job: consuming ExecutorJob,
                         at instant: C.Instant,
                         tolerance: C.Duration?,
                         clock: C) {
    (platform as! any SchedulingExecutor).enqueue(
      job, at: instant, tolerance: tolerance, clock: clock)
  }

  func hold() {
    held.withLock { $0 = [] }
  }

  // Stops holding jobs and returns the jobs held so far, in the order they
  // were enqueued.
  func release() -> [UnownedJob] {
    held.withLock { jobs in
      defer { jobs = nil }
      return jobs ?? []
    }
  }
}

let holdingDefaultExecutor = HoldingDefaultExecutor()

struct TestFactory: ExecutorFactory {
  static var mainExecutor: any MainExecutor {
    PlatformExecutorFactory.mainExecutor
  }
  static var defaultExecutor: any TaskExecutor { holdingDefaultExecutor }
}

// Runs a job the way the Dispatch-based default executor does: `swift_job_run`
// with the generic executor.
@_silgen_name("swift_job_run")
func _runJob(_ job: UnownedJob, on executor: UnownedSerialExecutor)

let genericSerialExecutor = unsafeBitCast((0, 0), to: UnownedSerialExecutor.self)

// Whether the runtime supports priority escalation, and therefore task
// stealers.
let runtimeSupportsPriorityEscalation: Bool = {
  guard let symbol = dlsym(
    UnsafeMutableRawPointer(bitPattern: -2),  // RTLD_DEFAULT
    "_swift_concurrency_debug_supportsPriorityEscalation")
  else {
    return false
  }
  return symbol.load(as: Bool.self)
}()

@main
struct Main {
  typealias DefaultExecutorFactory = TestFactory

  static func main() async {
    let tests = TestSuite("SignpostTracingTaskStealer")

    tests.test("A task stealer on the default executor reports the global concurrent executor kind") {
      guard let monitor = await SignpostMonitor.start() else {
        print("skipping: signpost log streaming unavailable in this environment")
        return
      }

      // Enqueue a task on the default executor and escalate it while it is
      // still enqueued. With escalation, the runtime enqueues a task stealer
      // after the task.
      holdingDefaultExecutor.hold()
      let task = Task.detached(name: "EscalatedTask", priority: .low) { 1 }
      task.escalatePriority(to: .high)
      let jobs = holdingDefaultExecutor.release()
      expectEqual(runtimeSupportsPriorityEscalation ? 2 : 1, jobs.count)

      // Run the jobs as the Dispatch-based default executor does, the stealer
      // first. The job that runs first starts the task, and the other does
      // nothing.
      for job in jobs.reversed() {
        _runJob(job, on: genericSerialExecutor)
      }
      expectEqual(1, await task.value)

      // executorKind=0 is the global concurrent executor.
      await monitor.expectAllSignpostsReceived(
        [ExpectedSignpost(name: "job_run", type: .begin,
                          messageContains: "taskName=EscalatedTask executorKind=0")]
      )
    }

    await runAllTestsAsync()
  }
}
