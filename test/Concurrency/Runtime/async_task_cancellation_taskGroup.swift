// RUN: %target-run-simple-swift( -target %target-swift-5.1-abi-triple %import-libdispatch -parse-as-library) | %FileCheck %s --dump-input=always

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: libdispatch

// rdar://76038845
// REQUIRES: concurrency_runtime

// rdar://103606995
// UNSUPPORTED: back_deployment_runtime

import Dispatch

@available(SwiftStdlib 5.1, *)
func test_detach_cancel_taskGroup() async {
  print(#function) // CHECK: test_detach_cancel_taskGroup

  await withTaskGroup(of: Void.self) { group in
    group.cancelAll() // immediately cancel the group
    print("group.cancel()") // CHECK: group.cancel()

    group.addTask {
      // immediately cancelled child task...
      await withTaskCancellationHandler {
        print("child: operation, was cancelled: \(Task.isCancelled)")
      } onCancel: {
        print("child: onCancel, was cancelled: \(Task.isCancelled)")
      }
    }
    // CHECK: child: onCancel, was cancelled: true
    // CHECK: child: operation, was cancelled: true
  }

  print("done") // CHECK: done
}

@available(SwiftStdlib 5.1, *)
@main struct Main {
  static func main() async {
    await test_detach_cancel_taskGroup()
  }
}
