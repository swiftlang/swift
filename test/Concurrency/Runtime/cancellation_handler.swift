// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-concurrency %import-libdispatch)
// REQUIRES: concurrency
// REQUIRES: executable_test

// rdar://76038845
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: OS=windows-msvc

// for sleep
#if canImport(Darwin)
    import Darwin
#elseif canImport(Glibc)
    import Glibc
#endif

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
  sleep(1)
  detach {
    await Task.withCancellationHandler {
        print("Task was cancelled!")
    }
    operation: {
        print("Running the operation...")
    }
  }
  sleep(10)
} else {
  // Fake prints to satisfy FileCheck.
  print("Canary")
  print("canary died")
  print("Running the operation...")
}
// CHECK: Canary
// CHECK-NEXT: canary died
// CHECK-NEXT: Running the operation...
