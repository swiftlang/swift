// RUN: %target-run-simple-swift( -target %target-swift-5.1-abi-triple -parse-as-library -disable-availability-checking -swift-version 6)
// RUN: %target-run-simple-swift( -target %target-swift-5.1-abi-triple -parse-as-library -disable-availability-checking -swift-version 6 -O)

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime

// A default executor that is both a serial and a task executor. However it
// passes its roles when it runs a job, it must be as efficient as one passing
// itself only as a task executor. That holds for a task without a preference
// and for a task that prefers the default executor.

@_spi(ExperimentalCustomExecutors) import _Concurrency
import StdlibUnittest
import Synchronization

// The roles an executor passes when it runs a job.
enum PassedRoles: CaseIterable, CustomStringConvertible {
  case taskExecutorOnly
  case serialExecutorOnly
  case both

  var description: String {
    switch self {
    case .taskExecutorOnly: "only the task executor"
    case .serialExecutorOnly: "only the serial executor"
    case .both: "both roles"
    }
  }
}

// An executor that runs jobs as soon as they are enqueued, unless held,
// in which case it queues them until drained.
final class DualRoleExecutor: SerialExecutor, TaskExecutor, Sendable {
  private let jobs = Mutex<[UnownedJob]>([])
  private let held = Atomic<Bool>(false)

  func enqueue(_ job: UnownedJob) {
    if held.load(ordering: .relaxed) {
      jobs.withLock { $0.append(job) }
    } else {
      unsafe job.runSynchronously(on: asUnownedTaskExecutor())
    }
  }

  func hold() {
    held.store(true, ordering: .relaxed)
  }

  func asUnownedSerialExecutor() -> UnownedSerialExecutor {
    UnownedSerialExecutor(ordinary: self)
  }

  func asUnownedTaskExecutor() -> UnownedTaskExecutor {
    UnownedTaskExecutor(ordinary: self)
  }

  // Runs queued jobs until there are none, then stops holding. Returns how many
  // jobs ran.
  func drainAndRelease(passing roles: PassedRoles) -> Int {
    var jobsRun = 0
    while let job = jobs.withLock({ $0.isEmpty ? nil : $0.removeFirst() }) {
      jobsRun += 1
      switch roles {
      case .taskExecutorOnly:
        unsafe job.runSynchronously(on: asUnownedTaskExecutor())
      case .serialExecutorOnly:
        unsafe job.runSynchronously(on: asUnownedSerialExecutor())
      case .both:
        unsafe job.runSynchronously(
          isolatedTo: asUnownedSerialExecutor(), taskExecutor: asUnownedTaskExecutor())
      }
    }
    held.store(false, ordering: .relaxed)
    return jobsRun
  }
}

final class InlineMainExecutor: MainExecutor, Sendable {
  func enqueue(_ job: consuming ExecutorJob) {
    unsafe UnownedJob(job).runSynchronously(on: self.asUnownedSerialExecutor())
  }
  func checkIsolated() {}
  func run() throws {}
  func stop() {}
}

let dualRoleExecutor = DualRoleExecutor()

struct TestFactory: ExecutorFactory {
  static var mainExecutor: any MainExecutor { InlineMainExecutor() }
  static var defaultExecutor: any TaskExecutor { dualRoleExecutor }
}

// Runs a task that yields `count` times, returning how many jobs the default
// executor ran for it. The task prefers no task executor, or the default
// executor if `preferringDefaultExecutor` is true.
func jobsForYielding(
  _ count: Int,
  passing roles: PassedRoles,
  preferringDefaultExecutor: Bool
) async -> Int {
  dualRoleExecutor.hold()
  let operation: @Sendable () async -> Void = {
    for _ in 0..<count {
      await Task.yield()
    }
  }
  let task = preferringDefaultExecutor
    ? Task.detached(executorPreference: dualRoleExecutor, operation: operation)
    : Task.detached(operation: operation)
  let jobs = dualRoleExecutor.drainAndRelease(passing: roles)
  await task.value
  return jobs
}

@main struct Main {
  typealias DefaultExecutorFactory = TestFactory

  static func main() async {
    let tests = TestSuite("A default executor that is both a serial and a task executor")

    // Each yield takes two jobs: the yield's own job, which resumes the task,
    // and the task's job, which then runs the task's concurrent code inline.
    // With the task's first job, that is two jobs per yield plus one.
    for roles in PassedRoles.allCases {
      for preferringDefaultExecutor in [false, true] {
        let preference = preferringDefaultExecutor
          ? "preferring the default executor" : "without a preference"
        tests.test("passing \(roles), a task \(preference) runs two jobs per yield plus one") {
          expectEqual(
            201,
            await jobsForYielding(
              100, passing: roles,
              preferringDefaultExecutor: preferringDefaultExecutor))
        }
      }
    }

    await runAllTestsAsync()
  }
}
