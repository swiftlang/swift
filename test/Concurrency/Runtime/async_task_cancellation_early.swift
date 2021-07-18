// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-concurrency  %import-libdispatch -parse-as-library) | %FileCheck %s --dump-input=always

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: libdispatch

// Temporarily disabled to unblock PR testing:
// REQUIRES: rdar80745964

// rdar://76038845
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

import Dispatch

@available(SwiftStdlib 5.5, *)
func test_detach_cancel_child_early() async {
  print(#function) // CHECK: test_detach_cancel_child_early
  let h: Task<Bool, Error> = Task.detached {
    async let childCancelled: Bool = { () -> Bool in
      await Task.sleep(2_000_000_000)
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

@available(SwiftStdlib 5.5, *)
@main struct Main {
  static func main() async {
    await test_detach_cancel_child_early()
  }
}
