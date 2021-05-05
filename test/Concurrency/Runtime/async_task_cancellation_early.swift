// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-concurrency  %import-libdispatch -parse-as-library)

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: libdispatch

// rdar://76038845
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

import Dispatch

@available(SwiftStdlib 5.5, *)
func test_detach_cancel_child_early() async {
  print(#function) // CHECK: test_detach_cancel_child_early
  let h: Task.Handle<Bool, Error> = detach {
    async let childCancelled: Bool = { () -> Bool in
      await Task.sleep(2_000_000_000)
      return Task.isCancelled
    }()

    let xx = await childCancelled
    print("child, cancelled: \(xx)") // CHECK: child, cancelled: true
    let cancelled =  Task.isCancelled
    print("self, cancelled: \(cancelled )") // CHECK: self, cancelled: true
    return cancelled
  }

  // no sleep here -- this confirms that the child task `x`
  // carries the cancelled flag, as it is started from a cancelled task.

  h.cancel()
  print("handle cancel")
  let got = try! await h.get()
  print("was cancelled: \(got)") // CHECK: was cancelled: true
}

@available(SwiftStdlib 5.5, *)
@main struct Main {
  static func main() async {
    await test_detach_cancel_child_early()
  }
}
