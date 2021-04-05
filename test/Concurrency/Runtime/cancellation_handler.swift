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

if #available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *) {
  let task = detach {
    let canary = Canary()
    _ = await Task.withCancellationHandler {
      print(canary)
    } operation: {
      await Task.sleep(1_000_000)
    }
  }
  task.cancel()
} else {
  // Fake prints to satisfy FileCheck.
  print("Canary")
  print("canary died")
}
// CHECK: Canary
// CHECK-NEXT: canary died
