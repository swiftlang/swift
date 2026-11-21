// RUN: %target-run-simple-swift( -target %target-swift-5.1-abi-triple %import-libdispatch -parse-as-library) | %FileCheck %s
// RUN: %target-run-simple-swift( -target %target-swift-5.1-abi-triple %import-libdispatch -parse-as-library -swift-version 5 -strict-concurrency=complete -enable-upcoming-feature NonisolatedNonsendingByDefault)  | %FileCheck %s
// REQUIRES: swift_feature_NonisolatedNonsendingByDefault

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: libdispatch

// rdar://76038845
// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: use_os_stdlib

import Dispatch

let seconds: UInt64 = 1_000_000_000

@available(SwiftStdlib 5.1, *)
func test_detach_cancel_while_child_running() async {
  let task: Task<Bool, Error> = Task.detached {
    async let childCancelled: Bool = { () -> Bool in
      await Task.sleep(3 * seconds)
      return Task.isCancelled
    }()

    let childWasCancelled = await childCancelled
    print("child, cancelled: \(childWasCancelled)") // CHECK: child, cancelled: true
    let selfWasCancelled =  Task.isCancelled
    print("self, cancelled: \(selfWasCancelled )") // CHECK: self, cancelled: true
    return selfWasCancelled
  }

  // sleep here, i.e. give the task a moment to start running
  await Task.sleep(2 * seconds)

  task.cancel()
  print("task.cancel()")
  let got = try! await task.get()
  print("was cancelled: \(got)") // CHECK: was cancelled: true
}

@available(SwiftStdlib 5.1, *)
func test_cancel_while_withTaskCancellationHandler_inflight() async {
  let task: Task<Bool, Error> = Task.detached {
    await withTaskCancellationHandler {
      await Task.sleep(2 * seconds)
      print("operation-1")
      await Task.sleep(1 * seconds)
      print("operation-2")
      return Task.isCancelled
    } onCancel: {
      print("onCancel")
    }

  }

  await Task.sleep(1 * seconds)

  // CHECK: task.cancel()
  // CHECK: onCancel
  // CHECK: operation-1
  // CHECK: operation-2
  print("task.cancel()")
  task.cancel()
  let got = try! await task.get()
  print("was cancelled: \(got)") // CHECK: was cancelled: true
}

@available(SwiftStdlib 5.1, *)
func test_cancel_while_withTaskCancellationHandler_onlyOnce() async {
  let task: Task<Bool, Error> = Task.detached {
    await withTaskCancellationHandler {
      await Task.sleep(2 * seconds)
      await Task.sleep(2 * seconds)
      await Task.sleep(2 * seconds)
      print("operation-done")
      return Task.isCancelled
    } onCancel: {
      print("onCancel")
    }
  }

  await Task.sleep(1 * seconds)

  // CHECK: task.cancel()
  // CHECK: onCancel
  // onCancel runs only once, even though we attempt to cancel the task many times
  // CHECK-NEXT: operation-done
  print("task.cancel()")
  task.cancel()
  task.cancel()
  task.cancel()
  let got = try! await task.get()
  print("was cancelled: \(got)") // CHECK: was cancelled: true
}

// A synchronous function, so this uses the synchronous overload.
// The handler unblocks the operation, which blocks until the task is cancelled.
@available(SwiftStdlib 6.5, *)
func sync_withTaskCancellationHandler_blockUntilCancelled() -> Bool {
  let semaphore = DispatchSemaphore(value: 0)
  return withTaskCancellationHandler {
    print("sync operation: waiting")
    semaphore.wait()
    print("sync operation: woken up")
    return Task.isCancelled
  } onCancel: {
    print("sync onCancel")
    semaphore.signal()
  }
}

@available(SwiftStdlib 6.5, *)
func test_cancel_while_sync_withTaskCancellationHandler_inflight() async {
  let task: Task<Bool, Never> = Task.detached {
    sync_withTaskCancellationHandler_blockUntilCancelled()
  }

  await Task.sleep(1 * seconds)

  // CHECK: sync operation: waiting
  // CHECK: task.cancel()
  // CHECK-NEXT: sync onCancel
  // CHECK-NEXT: sync operation: woken up
  print("task.cancel()")
  task.cancel()
  let got = await task.value
  print("sync was cancelled: \(got)") // CHECK: sync was cancelled: true
}

// The handler runs immediately, before the operation.
@available(SwiftStdlib 6.5, *)
func test_sync_withTaskCancellationHandler_alreadyCancelled() async {
  // CHECK: sync onCancel
  // CHECK-NEXT: sync operation: waiting
  // CHECK-NEXT: sync operation: woken up
  // CHECK-NEXT: sync already cancelled: true
  let got = await Task.detached {
    withUnsafeCurrentTask { $0!.cancel() }
    return sync_withTaskCancellationHandler_blockUntilCancelled()
  }.value
  print("sync already cancelled: \(got)")
}

@available(SwiftStdlib 6.5, *)
func sync_withTaskCancellationHandler_withReason() {
  withTaskCancellationHandler {
    print("sync operation (reason): start")
    withUnsafeCurrentTask { $0!.cancel(reason: .deadlineExpired) }
    print("sync operation (reason): end")
  } onCancel: { reason in
    print("sync onCancel: \(reason)")
  }
}

@available(SwiftStdlib 6.5, *)
func test_cancel_while_sync_withTaskCancellationHandler_withReason() async {
  // CHECK: sync operation (reason): start
  // CHECK-NEXT: sync onCancel: deadlineExpired
  // CHECK-NEXT: sync operation (reason): end
  await Task.detached {
    sync_withTaskCancellationHandler_withReason()
  }.value
}

enum Boom: Error {
  case boom
}

struct Unique: ~Copyable {
  let value: Int
}

/// Runs `body` on a thread that isn't running a task.
@available(SwiftStdlib 6.5, *)
func onThreadWithoutTask(_ body: @escaping @Sendable () -> Void) async {
  await withCheckedContinuation { (cc: CheckedContinuation<Void, Never>) in
    DispatchQueue.global().async {
      withUnsafeCurrentTask { precondition($0 == nil, "expected no current task") }
      body()
      cc.resume()
    }
  }
}

// Without a task there's nothing that could be cancelled, so the handlers
// can never run, therefore installing in such context is just a no-op.
@available(SwiftStdlib 6.5, *)
func test_sync_withTaskCancellationHandler_withoutTask() async {

  // CHECK: without task: start
  // CHECK-NEXT: without task: innermost 7
  // CHECK-NEXT: without task: caught boom
  // CHECK-NEXT: without task: done 7
  await onThreadWithoutTask {
    // If there was a task here, pretend we're cancelling it.
    withUnsafeCurrentTask {
      $0?.cancel() // Should trivially no-op
      $0?.escalatePriority(to: .high) // Should trivially no-op
    }

    print("without task: start")
    let value = withTaskCancellationShield {
      withTaskCancellationHandler {
        withTaskPriorityEscalationHandler {
          let unique = withTaskCancellationHandler {
            Unique(value: 7)
          } onCancel: { reason in
            print("without task: onCancel: \(reason)")
          }
          print("without task: innermost \(unique.value)")
          return unique.value
        } onPriorityEscalated: { _, _ in
          print("without task: onPriorityEscalated")
        }
      } onCancel: {
        print("without task: onCancel")
      }
    }

    do {
      try withTaskCancellationHandler { () throws(Boom) in
        throw .boom
      } onCancel: {
        print("without task: onCancel")
      }
    } catch {
      print("without task: caught \(error)")
    }

    print("without task: done \(value)")
  }
}

@available(SwiftStdlib 6.5, *)
@main struct Main {
  static func main() async {
    await test_detach_cancel_while_child_running()
    await test_cancel_while_withTaskCancellationHandler_inflight()
    await test_cancel_while_withTaskCancellationHandler_onlyOnce()
    await test_cancel_while_sync_withTaskCancellationHandler_inflight()
    await test_sync_withTaskCancellationHandler_alreadyCancelled()
    await test_cancel_while_sync_withTaskCancellationHandler_withReason()
    await test_sync_withTaskCancellationHandler_withoutTask()
  }
}
