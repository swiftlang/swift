// RUN: %target-run-simple-swift( -target %target-swift-5.1-abi-triple %import-libdispatch -parse-as-library) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: libdispatch

// rdar://76038845
// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime

func test_taskGroup_cancelAll() async {

    await withTaskCancellationHandler {
        await withTaskGroup(of: Int.self, returning: Void.self) { group in
            group.spawn {
                await Task.sleep(3_000_000_000)
                let c = Task.isCancelled
                print("group task isCancelled: \(c)")
                return 0
           }

           group.cancelAll() // Cancels the group but not the task
           _ = await group.next()
       }
    } onCancel : {
       print("parent task cancel handler called")
    }

    // CHECK-NOT: parent task cancel handler called
    // CHECK: group task isCancelled: true
    // CHECK: done
    print("done")
}

@available(SwiftStdlib 5.1, *)
@main struct Main {
  static func main() async {
    await test_taskGroup_cancelAll()
  }
}
