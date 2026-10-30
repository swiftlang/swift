// RUN: %target-run-simple-swift(-parse-as-library -target %target-swift-6.2-abi-triple %import-libdispatch -enable-builtin-module) | %FileCheck %s

// REQUIRES: concurrency
// REQUIRES: executable_test
// REQUIRES: concurrency_runtime
// REQUIRES: libdispatch

// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: freestanding

import Builtin
import Dispatch
@_spi(Concurrency) import _Concurrency

enum Locals {
  @TaskLocal static var name: String = "<none>"
}

actor Counter {
  var value = 0

  nonisolated func incrementAssumingIsolated() {
    assumeIsolated { $0.value += 1 }
  }

  func get() -> Int { value }
}

let syncFlags = _taskCreateFlags(
  priority: nil,
  isChildTask: false,
  copyTaskLocals: true,
  inheritContext: false,
  enqueueJob: true,
  addPendingGroupTaskUnconditionally: false,
  isDiscardingTask: true,
  isSynchronousStart: false,
  hasSynchronousOperation: true)

let asyncFlags = _taskCreateFlags(
  priority: nil,
  isChildTask: false,
  copyTaskLocals: true,
  inheritContext: false,
  enqueueJob: true,
  addPendingGroupTaskUnconditionally: false,
  isDiscardingTask: true,
  isSynchronousStart: false,
  hasSynchronousOperation: false)

@available(SwiftStdlib 6.5, *)
func launch(_ operation: sending @escaping () -> Void) {
  _ = Builtin.createSynchronousDiscardingTask(
    flags: syncFlags,
    operation: operation)
}

// A 'send' of an async target is a regular discarding task
@available(SwiftStdlib 6.5, *)
func launchAsyncTarget(_ operation: sending @escaping () async -> Void) {
  _ = Builtin.createDiscardingTask(
    flags: asyncFlags,
    operation: operation)
}

@available(SwiftStdlib 6.5, *)
func launch(on actor: Counter, _ operation: sending @escaping () -> Void) {
  let executor = unsafeBitCast(actor.unownedExecutor,
                               to: Builtin.Executor.self)
  _ = Builtin.createSynchronousDiscardingTask(
    flags: syncFlags,
    initialSerialExecutor: executor,
    operation: operation)
}

@main struct Main {
  static func main() async {
    guard #available(SwiftStdlib 6.5, *) else {
      fatalError("This test shouldn't be running against old stdlibs.")
    }

    let done = DispatchSemaphore(value: 0)

    // No task locals at all
    launch {
      let isTask = withUnsafeCurrentTask { $0 != nil }
      print("no locals: isTask=\(isTask) name=\(Locals.name)")
      done.signal()
    }
    done.wait()
    // CHECK: no locals: isTask=true name=<none>

    // Task locals are copied into the new task
    Locals.$name.withValue("hello") {
      launch {
        print("locals: name=\(Locals.name)")
        done.signal()
      }
    }
    done.wait()
    // CHECK: locals: name=hello

    // With an initial serial executor, the operation runs on the actor
    let counter = Counter()
    for _ in 0..<100 {
      launch(on: counter) {
        counter.incrementAssumingIsolated()
        done.signal()
      }
    }
    for _ in 0..<100 {
      done.wait()
    }
    print("counter: \(await counter.get())")
    // CHECK: counter: 100

    // An async target can suspend
    launchAsyncTarget {
      let value = await counter.get()
      print("async target: counter=\(value)")
      done.signal()
    }
    done.wait()
    // CHECK: async target: counter=100

    print("done")
    // CHECK: done
  }
}
