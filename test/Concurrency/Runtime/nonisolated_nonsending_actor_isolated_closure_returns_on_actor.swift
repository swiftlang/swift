// RUN: %empty-directory(%t/src)
// RUN: split-file %s %t/src

// RUN: %target-build-swift -O -swift-version 6 -target %target-swift-5.1-abi-triple \
// RUN:   -parse-as-library -emit-library -emit-module-path %t/Lib.swiftmodule \
// RUN:   -module-name Lib %t/src/Lib.swift -o %t/%target-library-name(Lib)
// RUN: %target-codesign %t/%target-library-name(Lib)

// RUN: %target-build-swift -O -swift-version 6 -target %target-swift-6.0-abi-triple \
// RUN:   %import-libdispatch -parse-as-library -I %t -L %t -lLib %target-rpath(%t) \
// RUN:   %t/src/main.swift -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out %t/%target-library-name(Lib) | %FileCheck %s

// Forced no-inlining so actor isolated closures are always called directly
// instead of being inlined into the actor methods.
// RUN: %target-build-swift -O -swift-version 6 -target %target-swift-6.0-abi-triple \
// RUN:   %import-libdispatch -parse-as-library -I %t -L %t -lLib %target-rpath(%t) \
// RUN:   -D NO_INLINING \
// RUN:   %t/src/main.swift -o %t/a-no-inlining.out
// RUN: %target-codesign %t/a-no-inlining.out
// RUN: %target-run %t/a-no-inlining.out %t/%target-library-name(Lib) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime
// REQUIRES: libdispatch
// REQUIRES: synchronization

// UNSUPPORTED: back_deployment_runtime

// https://github.com/swiftlang/swift/issues/92915
// rdar://189171715
//
// An actor isolated closure that is passed to a nonisolated(nonsending)
// function keeps its actor isolation, but is called without a hop and its
// caller doesn't hop back after the call. So the closure must get back to the
// actor before it returns, even if it suspended and resumed somewhere else.

//--- Lib.swift

public nonisolated(nonsending) func run(_ body: nonisolated(nonsending) () async -> Void) async {
  await body()
}

public nonisolated(nonsending) func nonsendingNoop() async {}

//--- main.swift

import Lib
import Synchronization

let counter = Atomic<Int>(0)

#if NO_INLINING
@inline(never)
#endif
@concurrent func hopOff() async {
  counter.add(1, ordering: .relaxed)
}

actor Other {
  func ping() {
    counter.add(1, ordering: .relaxed)
  }
}

actor A {
  var count = 0
  let other = Other()

  func endsWithConcurrentCall() async {
    await run {
      // This A-isolated closure is passed to a nonisolated(nonsending)
      // parameter. It keeps its A isolation, but is called like a
      // nonisolated(nonsending) function: 'run' calls it on A without a hop,
      // which is fine since we call 'run' from A anyway, however ...
      self.count += 1
      await hopOff()
      // ... since this closure "behaves like nonisolated(nonsending)", its
      // caller assumes it returns while isolated to A.
      //
      // We MUST HOP back to A after hopOff() here, before returning,
      // as otherwise we'd return while on the hopped-off-to actor isolation
    }
    // Nothing hops back to A after 'run' returns either: 'run' is
    // nonisolated(nonsending), so returning on A is the closure's job

    // If the closure missed the hop back, we'd still be running on the generic
    // executor here, and violate this precondition:
    self.preconditionIsolated("endsWithConcurrentCall() resumed off the actor")
  }

  func endsWithCallToOtherActor() async {
    await run {
      count += 1
      await other.ping()
    }
    preconditionIsolated("endsWithCallToOtherActor() resumed off the actor")
  }

  func checkOnActor(_ step: String) {
    preconditionIsolated("\(step): off the actor")
  }

  // Every call that leaves the actor needs its own hop back to A
  func multipleHops() async {
    await run {
      // Called on A, no hop needed
      self.checkOnActor("start")
      await other.ping()
      self.checkOnActor("after other.ping()")
      await hopOff()
      self.checkOnActor("after hopOff()")
      // Stays on A
      await nonsendingNoop()
      self.checkOnActor("after nonsendingNoop()")
      await other.ping()
    }
    preconditionIsolated("multipleHops() resumed off the actor")
  }
}

actor Generic<T> {
  var count = 0
  let other = Other()
  var waiters: [CheckedContinuation<Void, Never>] = []

#if NO_INLINING
  @inline(never)
  @_semantics("optimize.sil.specialize.generic.never")
#endif
  func endsWithConcurrentCall() async {
    await withTaskCancellationHandler {
      count += 1
      await hopOff()
    } onCancel: {
      fatalError("Should not be cancelled")
    }
    preconditionIsolated("Generic.endsWithConcurrentCall() resumed off the actor")
  }

#if NO_INLINING
  @inline(never)
  @_semantics("optimize.sil.specialize.generic.never")
#endif
  func endsWithCallToOtherActor() async {
    await withTaskCancellationHandler {
      count += 1
      await other.ping()
    } onCancel: {
      fatalError("Should not be cancelled")
    }
    preconditionIsolated("Generic.endsWithCallToOtherActor() resumed off the actor")
  }

  // The continuation resumes on the generic executor
#if NO_INLINING
  @inline(never)
  @_semantics("optimize.sil.specialize.generic.never")
#endif
  func endsWithContinuation() async {
    await withTaskCancellationHandler {
      await withCheckedContinuation {
        self.waiters.append($0)
      }
    } onCancel: {
      fatalError("Should not be cancelled")
    }
    preconditionIsolated("Generic.endsWithContinuation() resumed off the actor")
  }

  func resumeWaiter() -> Bool {
    guard !waiters.isEmpty else { return false }
    waiters.removeFirst().resume()
    return true
  }
}

@main struct Main {
  static func main() async {
    let a = A()
    await a.endsWithConcurrentCall()
    // CHECK: endsWithConcurrentCall
    print("endsWithConcurrentCall")

    await a.endsWithCallToOtherActor()
    // CHECK: endsWithCallToOtherActor
    print("endsWithCallToOtherActor")

    await a.multipleHops()
    // CHECK: multipleHops
    print("multipleHops")

    let g = Generic<Int>()
    await g.endsWithConcurrentCall()
    // CHECK: Generic.endsWithConcurrentCall
    print("Generic.endsWithConcurrentCall")

    await g.endsWithCallToOtherActor()
    // CHECK: Generic.endsWithCallToOtherActor
    print("Generic.endsWithCallToOtherActor")

    let waiter = Task { await g.endsWithContinuation() }
    while await !g.resumeWaiter() {
      await Task.yield()
    }
    await waiter.value
    // CHECK: Generic.endsWithContinuation
    print("Generic.endsWithContinuation")
  }
}
