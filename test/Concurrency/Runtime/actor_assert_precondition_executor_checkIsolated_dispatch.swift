// RUN: %empty-directory(%t)
// RUN: %target-build-swift -Xfrontend -disable-availability-checking %import-libdispatch -parse-as-library %s -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN:  %target-run %t/a.out

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime

// REQUIRES: libdispatch

// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: back_deploy_concurrency
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: freestanding

// FIXME(concurrency): rdar://119743909 fails in optimize tests.
// UNSUPPORTED: swift_test_mode_optimize
// UNSUPPORTED: swift_test_mode_optimize_size

// FIXME(concurrency): corelibs-dispatch is missing the `DispatchSerialQueue` type
// XFAIL: OS=freebsd, OS=openbsd, OS=linux-gnu, OS=linux-android, OS=linux-androideabi

import StdlibUnittest
import Dispatch

// FIXME(concurrency): Dispatch should provide such implementation
extension DispatchSerialQueue {
  public func checkIsolated() {
    dispatchPrecondition(condition: .onQueue(self))
  }
}

/// We only do the executor dance because missing 'asUnownedSerialExecutor'
/// on DispatchSerialQueue on some platforms.
final class NaiveQueueExecutor: SerialExecutor {
  let queue: DispatchSerialQueue

  init(queue: DispatchSerialQueue) {
    self.queue = queue
  }

  public func enqueue(_ unowned: UnownedJob) {
    queue.sync {
      unowned.runSynchronously(on: self.asUnownedSerialExecutor())
    }
  }

  public func asUnownedSerialExecutor() -> UnownedSerialExecutor {
    UnownedSerialExecutor(ordinary: self)
  }

  func checkIsolated() {
    self.queue.checkIsolated()
  }
}

actor ActorOnNaiveQueueExecutor {
  let queue: DispatchSerialQueue
  let executor: NaiveQueueExecutor

  init() {
    self.queue = DispatchSerialQueue(label: "MyQueue")
    self.executor = NaiveQueueExecutor(queue: queue)
  }

  nonisolated var unownedExecutor: UnownedSerialExecutor {
    self.executor.asUnownedSerialExecutor()
  }

  nonisolated func checkPreconditionIsolated() async {
    print("Before queue.sync {}")
    self.queue.sync {
      print("Before preconditionIsolated")
      self.preconditionIsolated()
      print("After preconditionIsolated")

      print("Before dispatchPrecondition")
      dispatchPrecondition(condition: .onQueue(self.queue))
      print("After preconditionIsolated")
    }
  }
}

actor Other {
  func checkUnexpected(actor: some Actor) {
    actor.assumeIsolated { isolatedActor in
      print("OK")
    }
  }
}

// TODO: DispatchSerialQueue conformance to SerialExecutor not available on all platforms yet
//actor ActorOnQueue {
//  let queue: DispatchSerialQueue
//
//  init() {
//    self.queue = DispatchSerialQueue(label: "MyQueue")
//  }
//
//  nonisolated var unownedExecutor: UnownedSerialExecutor {
//    self.queue.asUnownedSerialExecutor()
//  }
//
//  nonisolated func checkPreconditionIsolated() async {
//    print("Before queue.sync {}")
//    self.queue.sync {
//      print("Before preconditionIsolated")
//      self.queue.preconditionIsolated()
//      print("After preconditionIsolated")
//
//      print("Before dispatchPrecondition")
//      dispatchPrecondition(condition: .onQueue(self.queue))
//      print("After preconditionIsolated")
//    }
//  }
//}

@main struct Main {
  static func main() async {
    let tests = TestSuite("AssertPreconditionActorExecutorCheckIsolated")

    if #available(SwiftStdlib 6.0, *) {
      let actorOnQueue = ActorOnNaiveQueueExecutor()

      tests.test("\(ActorOnNaiveQueueExecutor.self): queue.sync { preconditionIsolated() } ") {
        await actorOnQueue.checkPreconditionIsolated()
      }

      tests.test("\(Other.self): wrongly assume default actor on another actor's custom executor") {
        expectCrashLater() // this will call into dispatch precondition, no message
        let other = Other()
        await other.checkUnexpected(actor: actorOnQueue)
      }

      tests.test("\(Other.self): correctly assume default actor on same default actor") {
        let other = Other()
        await other.checkUnexpected(actor: other)
      }

      // TODO: DispatchSerialQueue conformance to SerialExecutor not available on all platforms yet
      // tests.test("\(ActorOnQueue.self): queue.sync { preconditionIsolated() } ") {
      //   await ActorOnQueue().checkPreconditionIsolated()
      // }

    }

    await runAllTestsAsync()
  }
}
