// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-concurrency  %import-libdispatch -parse-as-library)

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: libdispatch

#if canImport(Darwin)
import Darwin
#elseif canImport(Glibc)
import Glibc
#endif

func test_runDetached_cancel_child_early() async {
  print(#function) // CHECK: test_runDetached_cancel_child_early
  let h: Task.Handle<Bool, Error> = Task.runDetached {
    async let childCancelled: Bool = { () -> Bool in
      usleep(1000 * 2)
      return await Task.__unsafeCurrentAsync().isCancelled
    }()

    let xx = await childCancelled
    print("child, cancelled: \(xx)") // CHECK: child, cancelled: true
    let cancelled =  await Task.__unsafeCurrentAsync().isCancelled
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

@main struct Main {
  static func main() async {
    await test_runDetached_cancel_child_early()
  }
}
