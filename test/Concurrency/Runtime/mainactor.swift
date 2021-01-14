// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-concurrency %import-libdispatch) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: libdispatch
// REQUIRES: rdar72105129

// XFAIL: *

#if canImport(Darwin)
import Darwin
#elseif canImport(Glibc)
import Glibc
#endif

import Foundation

// CHECK: starting
// CHECK-NOT: ERROR
// CHECK: hello from main actor!
// CHECK-NOT: ERROR
// CHECK: ending

@MainActor func helloMainActor() {
  if Thread.isMainThread {
    print("hello from main actor!")
  } else {
    print("ERROR: not on correct thread!")
  }
}

func someFunc() async {
  await helloMainActor()
}

runAsyncAndBlock {
  print("starting")
  let handle = Task.runDetached(operation: someFunc)
  do {
    try await handle.get()
  } catch {
    print("ERROR: exception was thrown while waiting for task to complete")
  }
  print("ending")
}
