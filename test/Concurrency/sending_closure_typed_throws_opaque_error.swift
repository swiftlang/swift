// RUN: %target-swift-frontend -emit-sil -o /dev/null -verify -swift-version 6 -parse-as-library %s
// RUN: %target-swift-frontend -emit-sil -o /dev/null -verify -swift-version 5 -strict-concurrency=complete -parse-as-library %s
// RUN: %target-swift-frontend -emit-sil -o /dev/null -verify -swift-version 6 -parse-as-library -default-isolation MainActor %s

// REQUIRES: concurrency

// A non-throwing closure literal passed to a 'sending @isolated(any)'
// parameter whose thrown error is the callee's generic 'Failure' parameter,
// with 'Failure' bound to 'any Error' by context, must be accepted just like
// it is when the parameter is spelled with an untyped 'throws'.
//
// SILGen used to emit such a closure at its own (non-throwing) type and wrap
// it in a reabstraction thunk, which region isolation then reported as
// sending an actor-isolated value. 'TaskLike' mirrors the shape that
// 'Task.init' has when it adopts typed throws.

struct TaskLike<Success: Sendable, Failure: Error>: Sendable {
  init(
    @_inheritActorContext @_implicitSelfCapture operation: sending @escaping @isolated(any) () async throws(Failure) -> Success
  ) {}

  static func detached(
    operation: sending @escaping @isolated(any) () async throws(Failure) -> Success
  ) -> Self {
    fatalError()
  }
}

func doSomething() async {}

// rdar://173501057
@MainActor class MainActorStoresTask {
  var someTask: TaskLike<Void, Error>?

  func start() {
    someTask = TaskLike {}
  }

  func startWeakSelf() {
    someTask = TaskLike { @MainActor [weak self] in
      _ = self
    }
  }

  func returnsTask() -> TaskLike<Void, Error> {
    return TaskLike { [weak self] () -> Void in
      _ = self
      await doSomething()
    }
  }
}

// rdar://173256879
actor ActorReturnsTask {
  func test() async -> TaskLike<Void, any Error> {
    return TaskLike {
      _ = self
    }
  }
}

// rdar://173339104
public final class NonisolatedClassWithDidSet {
  private var cond: Bool? {
    didSet {}
  }

  private var task: TaskLike<Void, Error>?

  public init() {
    task = TaskLike { [weak self] in
      self?.cond = true
    }
  }
}

// Throwing and explicitly typed closures were already fine; keep them so.
@MainActor func throwingClosures() {
  struct E: Error {}
  func thrower() throws {}
  let _: TaskLike<Void, any Error> = TaskLike { try thrower() }
  let _: TaskLike<Void, any Error> = TaskLike { () throws in }
  let _: TaskLike<Void, E> = TaskLike { () throws(E) in }
  let _: TaskLike<Void, E> = TaskLike {}
  let _: TaskLike<Void, Never> = TaskLike {}
}

// Captures of a closure emitted directly at the destination type are still
// tracked by region isolation.
class NonSendable {
  func use() {}
}

func stillDiagnosesRealRaces() {
  let ns = NonSendable()
  let _: TaskLike<Void, any Error> = TaskLike.detached {
    ns.use()
  }
  ns.use()
}
