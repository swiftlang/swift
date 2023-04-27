// RUN: %target-run-simple-swift( -Xfrontend -disable-availability-checking %import-libdispatch -parse-as-library) | %FileCheck %s

// REQUIRES: concurrency
// REQUIRES: executable_test

// rdar://106849189 move-only types should be supported in freestanding mode
// UNSUPPORTED: freestanding

// UNSUPPORTED: back_deployment_runtime
// REQUIRES: concurrency_runtime

final class InlineExecutor: SerialExecutor, CustomStringConvertible {
  public func enqueue(_ job: UnownedJob) {
    job.runSynchronously(on: self.asUnownedSerialExecutor())
  }
  public func enqueue(_ job: __owned Job) {
    job.runSynchronously(on: self.asUnownedSerialExecutor())
  }
  public func enqueue(_ job: __owned ExecutorJob) {
    job.runSynchronously(on: self.asUnownedSerialExecutor())
  }

  var description: Swift.String {
    "InlineExecutor()"
  }
}

let inlineExecutor = InlineExecutor()

actor Custom {
  var count = 0

  nonisolated var unownedExecutor: UnownedSerialExecutor {
    print("custom unownedExecutor")
    return inlineExecutor.asUnownedSerialExecutor()
  }

  func report() async {
    print("custom.count == \(count)")
    count += 1
  }
}

@available(SwiftStdlib 5.1, *)
@main struct Main {
  static func main() async {
    print("begin")
    let actor = Custom()
    await actor.report()
    await actor.report()
    await actor.report()
    print("end")
  }
}

// CHECK:      begin
// CHECK-NEXT: custom unownedExecutor
// CHECK-NEXT: custom.count == 0
// CHECK-NEXT: custom unownedExecutor
// CHECK-NEXT: custom.count == 1
// CHECK-NEXT: custom unownedExecutor
// CHECK-NEXT: custom.count == 2
// CHECK-NEXT: end
