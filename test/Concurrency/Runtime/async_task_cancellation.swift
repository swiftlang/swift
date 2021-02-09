// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-concurrency  %import-libdispatch -parse-as-library)

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: libdispatch

import Dispatch

#if canImport(Darwin)
import Darwin
#elseif canImport(Glibc)
import Glibc
#endif

func test_runDetached_cancel_and_child_too() async {
  let h: Task.Handle<Bool> = Task.runDetached {
    async let x: Bool = { () -> Bool in
      print("x start")
      sleep(2)
      let cancelled =  await Task.isCancelled()
      print("x done: \(cancelled)")
      return cancelled
    }()

    sleep(3)

    let xx = await x
    print("child, cancelled: \(xx)") // CHECK: child, cancelled: true
    let cancelled =  await Task.isCancelled()
    print("self, cancelled: \(cancelled )") // CHECK: self, cancelled: true
    return cancelled
  }

  h.cancel()
  print("handle cancel")
  let got = try! await h.get()
  print("was cancelled: \(got)") // CHECK: was cancelled: true
}



@main struct Main {
  static func main() async {
    await test_runDetached_cancel_and_child_too()
  }
}
