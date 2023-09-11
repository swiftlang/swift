// RUN: %target-run-simple-swift( -Xfrontend -disable-availability-checking %import-libdispatch -parse-as-library) | %FileCheck %s --dump-input=always

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: libdispatch

// rdar://76038845
// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime

import Dispatch

@available(SwiftStdlib 5.1, *)
func test_detach_cancel_child_early() async {
  print(#function) // CHECK: test_detach_cancel_child_early
  let h: Task<Bool, Error> = Task.detached {
    async let childCancelled: Bool = { () -> Bool in
      try? await Task.sleep(for: .seconds(10)) // we'll be woken up by cancellation
      return Task.isCancelled
    }()

    let xx = await childCancelled
    print("child, cancelled: \(xx)") // CHECK: child, cancelled: true
    let cancelled = Task.isCancelled
    print("self, cancelled: \(cancelled)") // CHECK: self, cancelled: true
    return cancelled
  }

  h.cancel()
  print("handle cancel")
  let got = try! await h.value
  print("was cancelled: \(got)") // CHECK: was cancelled: true
}

@available(SwiftStdlib 5.1, *)
@main struct Main {
  static func main() async {
    await test_detach_cancel_child_early()
  }
}
