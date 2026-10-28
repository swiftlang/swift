// RUN: %target-run-simple-swift( -swift-version 6 -Xfrontend -disable-availability-checking %import-libdispatch -parse-as-library) | %FileCheck %s
// RUN: %target-run-simple-swift( -O -swift-version 6 -Xfrontend -disable-availability-checking %import-libdispatch -parse-as-library) | %FileCheck %s
// RUN: %target-run-simple-swift( -swift-version 6 -enable-upcoming-feature NonisolatedNonsendingByDefault -Xfrontend -disable-availability-checking %import-libdispatch -parse-as-library) | %FileCheck %s
// RUN: %target-run-simple-swift( -O -swift-version 6 -enable-upcoming-feature NonisolatedNonsendingByDefault -Xfrontend -disable-availability-checking %import-libdispatch -parse-as-library) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: libdispatch
// REQUIRES: concurrency_runtime
// REQUIRES: swift_feature_NonisolatedNonsendingByDefault
// UNSUPPORTED: back_deployment_runtime

// rdar://190427405
// https://github.com/swiftlang/swift/issues/93432
// A defer must inherit the isolation of its enclosing closure.

actor Worker {
  func work() {}
}

@MainActor
final class Model {
  let worker = Worker()

  func taskDefer() -> Task<Void, Never> {
    Task {
      defer {
        MainActor.preconditionIsolated("defer in a MainActor Task closure")
        await worker.work()
        MainActor.preconditionIsolated("defer in a MainActor Task closure, after await")
      }
      await worker.work()
    }
  }
}

actor Counter {
  let worker = Worker()

  func taskDefer() -> Task<Void, Never> {
    Task {
      defer {
        self.preconditionIsolated("defer in an actor Task closure")
        await worker.work()
        self.preconditionIsolated("defer in an actor Task closure, after await")
      }
      await worker.work()
    }
  }

  // The defer does not name the actor, only the closure does
  func syncDeferIsolationInTask() async {
    await Task {
      let expected: any Actor = self
      defer {
        let isolation = #isolation // Not in the autoclosure of 'precondition', where '#isolation' would be nil
        precondition(isolation === expected, "#isolation in a sync defer in an actor Task closure")
      }
      await worker.work()
    }.value
  }
}

@MainActor
func nonsendingClosureDefer() async {
  let body: nonisolated(nonsending) () async -> Void = {
    defer {
      MainActor.preconditionIsolated("defer in a nonisolated(nonsending) closure")
      await Worker().work()
      MainActor.preconditionIsolated("defer in a nonisolated(nonsending) closure, after await")
    }
    await Worker().work()
  }
  await body()
}

@main struct Main {
  static func main() async {
    await Model().taskDefer().value
    print("defer in a MainActor Task closure: ran on the main actor")
    // CHECK: defer in a MainActor Task closure: ran on the main actor

    await Counter().taskDefer().value
    print("defer in an actor Task closure: ran on the actor")
    // CHECK: defer in an actor Task closure: ran on the actor

    await Counter().syncDeferIsolationInTask()
    print("#isolation in a sync defer in an actor Task closure: the actor")
    // CHECK: #isolation in a sync defer in an actor Task closure: the actor

    await nonsendingClosureDefer()
    print("defer in a nonisolated(nonsending) closure: ran on the caller's actor")
    // CHECK: defer in a nonisolated(nonsending) closure: ran on the caller's actor
  }
}
