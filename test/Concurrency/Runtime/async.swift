// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-concurrency %import-libdispatch)

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: libdispatch

// rdar://76038845
// UNSUPPORTED: use_os_stdlib

import Dispatch
import StdlibUnittest

// for sleep
#if canImport(Darwin)
    import Darwin
#elseif canImport(Glibc)
    import Glibc
#endif

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
actor MyActor {
  func synchronous() { }

  func doSomething(expectedPriority: Task.Priority) {
    async {
      synchronous() // okay to be synchronous
      assert(Task.currentPriority == expectedPriority)
    }
  }
}
