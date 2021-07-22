// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-concurrency -Xfrontend -disable-availability-checking %import-libdispatch)

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

var asyncTests = TestSuite("Async")

@available(SwiftStdlib 5.5, *)
actor MyActor {
  func synchronous() { }

  func doSomething(expectedPriority: TaskPriority) {
    async {
      synchronous() // okay to be synchronous
    }
  }
}

if #available(SwiftStdlib 5.5, *) {
  let actor = MyActor()

  asyncTests.test("Detach") {
    detach(priority: .background) {
      async {
        await actor.doSomething(expectedPriority: .background)
      }
    }
    sleep(1)
  }

  asyncTests.test("MainQueue") {
    DispatchQueue.main.async {
      async {
      }
    }
    sleep(1)
  }

  asyncTests.test("GlobalDispatchQueue") {
    DispatchQueue.global(qos: .utility).async {
      async {
#if (os(macOS) || os(iOS) || os(tvOS) || os(watchOS))
        // Non-Darwin platforms currently lack qos_class_self().
#endif
      }
    }
    sleep(1)
  }
}

runAllTests()
