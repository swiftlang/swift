// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-concurrency %import-libdispatch) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: libdispatch

#if canImport(Darwin)
import Darwin
#elseif canImport(Glibc)
import Glibc
#endif

import Foundation

// CHECK: starting
// CHECK-NOT: ERROR
// CHECK: launched
// CHECK-NOT: ERROR
// CHECK: hello from main actor!

@MainActor func helloMainActor() -> Never {
  if Thread.isMainThread {
    print("hello from main actor!")
    exit(EXIT_SUCCESS)
  } else {
    print("ERROR: not on correct thread!")
    exit(EXIT_FAILURE)
  }
}

func someFunc() async {
  await helloMainActor()
}

print("starting")
let _ = Task.runDetached(operation: someFunc)
print("launched")
dispatchMain() // give up the main queue.
