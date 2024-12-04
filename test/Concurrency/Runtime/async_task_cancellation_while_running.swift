// RUN: %target-run-simple-swift( -target %target-swift-5.1-abi-triple %import-libdispatch -parse-as-library) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: libdispatch

// rdar://76038845
// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime

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

@available(SwiftStdlib 5.1, *)
@main struct Main {
  static func main() async {
    await test_detach_cancel_while_child_running()
    await test_cancel_while_withTaskCancellationHandler_inflight()
    await test_cancel_while_withTaskCancellationHandler_onlyOnce()
  }
}
