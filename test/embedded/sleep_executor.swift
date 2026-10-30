// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-feature Embedded -parse-as-library %s -c -o %t/a.o
// RUN: %target-clang %target-clang-resource-dir-opt %t/a.o -o %t/a.out -L%swift_obj_root/lib/swift/embedded/%module-target-triple %target-clang-resource-dir-opt -lswift_Concurrency %target-swift-default-executor-opt -dead_strip %swift_obj_root/lib/swift/embedded/%module-target-triple/libswiftUnicodeDataTables.a
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: swift_in_compiler
// REQUIRES: optimized_stdlib
// REQUIRES: OS=macosx || OS=wasip1
// REQUIRES: swift_feature_Embedded

import _Concurrency

@available(SwiftStdlib 6.3, *)
final class TestExecutor: TaskExecutor, SchedulingExecutor, SerialExecutor, @unchecked Sendable {
  var asScheduling: SchedulingExecutor? {
    return self
  }

  public func enqueue(_ job: consuming ExecutorJob) {
    job.runSynchronously(on: self.asUnownedSerialExecutor())
  }

  public func enqueue<C: Clock>(_ job: consuming ExecutorJob,
                                after delay: C.Duration,
                                tolerance: C.Duration? = nil,
                                clock: C) {
    job.runSynchronously(on: self.asUnownedSerialExecutor())
  }
}

@available(SwiftStdlib 6.3, *)
@main struct Main {
  static func main() async {
    let taskExecutor = TestExecutor()

    let task = Task(executorPreference: taskExecutor) {
      print("foo") // CHECK: foo
    }

    await task.value
  }
}
