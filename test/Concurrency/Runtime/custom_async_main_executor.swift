// RUN: %target-run-simple-swift( -Xfrontend -disable-availability-checking %import-libdispatch -parse-as-library) | %FileCheck %s --dump-input=always

// REQUIRES: concurrency
// REQUIRES: executable_test
// REQUIRES: concurrency_runtime

@MainActor
var foo: Int = 42

func asyncFunc() async {
  print("Hello World!")
  await asyncFuncMain()
}

@MainActor
func asyncFuncMain() async {
  print("Hello Main Actor!")
}

@available(SwiftStdlib 5.1, *)
@_silgen_name("swift_job_run")
@usableFromInline
internal func _swiftJobRun(_ job: UnownedJob,
                           _ executor: UnownedSerialExecutor) -> ()

final class InlineRightAwayExecutor: SerialExecutor {
  func enqueue(_ job: UnownedJob) {
    print("\(Self.self): enqueue job: \(job)")
    runJob(job)
  }

  func asUnownedSerialExecutor() -> UnownedSerialExecutor {
    UnownedSerialExecutor(ordinary: self)
  }
}

extension SerialExecutor {
  func runJob(_ job: UnownedJob) {
    _swiftJobRun(job, asUnownedSerialExecutor())
  }
}

@main struct MyProgram {

  static func main() async throws {
    let inlineRightAwayExecutor = InlineRightAwayExecutor()
    _Concurrency.setMainActorExecutor(inlineRightAwayExecutor)

    print("\(foo)")
    foo += 1
    await asyncFunc()
    print("\(foo)")

    // CHECK: InlineRightAwayExecutor: enqueue job:
  }
}