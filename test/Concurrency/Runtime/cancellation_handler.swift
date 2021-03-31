// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-concurrency %import-libdispatch)
// REQUIRES: concurrency
// REQUIRES: executable_test

// rdar://76038845
// UNSUPPORTED: use_os_stdlib

class Canary {
  deinit {
    print("canary died")
  }
}

do {
  let task = Task.runDetached {
    let canary = Canary()
    _ = await Task.withCancellationHandler {
      print(canary)
    } operation: {
      await Task.sleep(1_000_000)
    }
  }
  task.cancel()
}
// CHECK: Canary
// CHECK-NEXT: canary died
