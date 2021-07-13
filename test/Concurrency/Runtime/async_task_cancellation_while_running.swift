// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-concurrency  %import-libdispatch -parse-as-library)

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: libdispatch

// rdar://76038845
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

import Dispatch

@available(SwiftStdlib 5.5, *)
func test_detach_cancel_while_child_running() async {
  let h: Task<Bool, Error> = detach {
    async let childCancelled: Bool = { () -> Bool in
      await Task.sleep(3_000_000_000)
      return Task.isCancelled
    }()

    let childWasCancelled = await childCancelled
    print("child, cancelled: \(childWasCancelled)") // CHECK: child, cancelled: true
    let selfWasCancelled =  Task.isCancelled
    print("self, cancelled: \(selfWasCancelled )") // CHECK: self, cancelled: true
    return selfWasCancelled
  }

  // sleep here, i.e. give the task a moment to start running
  await Task.sleep(2_000_000_000)

  h.cancel()
  print("handle cancel")
  let got = try! await h.get()
  print("was cancelled: \(got)") // CHECK: was cancelled: true
}

@available(SwiftStdlib 5.5, *)
@main struct Main {
  static func main() async {
    await test_detach_cancel_while_child_running()
  }
}
