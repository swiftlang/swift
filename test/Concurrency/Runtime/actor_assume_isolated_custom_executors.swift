// RUN: %target-run-simple-swift( -Xfrontend -disable-availability-checking %import-libdispatch -parse-as-library -parse-stdlib)
// RUN: %target-run-simple-swift( -Xfrontend -disable-availability-checking %import-libdispatch -parse-as-library -parse-stdlib -swift-version 6)

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime
// REQUIRES: libdispatch

// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: back_deploy_concurrency
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: freestanding

// Exercises assumeIsolated, and the builtins it is implemented with, on actors
// and global actors that use custom executors

import Swift
import _Concurrency
import Dispatch
import StdlibUnittest

// ==== -----------------------------------------------------------------------
// MARK: Executors

final class QueueExecutor: SerialExecutor, @unchecked Sendable {
  let queue: DispatchQueue

  init(label: String) {
    self.queue = DispatchQueue(label: label)
  }

  func enqueue(_ job: consuming ExecutorJob) {
    let job = UnownedJob(job)
    queue.async {
      job.runSynchronously(on: self.asUnownedSerialExecutor())
    }
  }
}

struct Boom: Error, Equatable {
  let value: Int
}

// ==== -----------------------------------------------------------------------
// MARK: Actor with a custom executor

actor CustomExecutorActor {
  let executor = QueueExecutor(label: "CustomExecutorActor")
  var count = 0
  var name = "custom"

  nonisolated var unownedExecutor: UnownedSerialExecutor {
    executor.asUnownedSerialExecutor()
  }

  /// Runs on the custom executor; calls into synchronous nonisolated code that
  /// assumes the isolation
  func checkValues() -> (Int, Int, String) {
    (incrementAssumingIsolated(self),
     incrementAssumingIsolated(self),
     nameAssumingIsolated(self))
  }

  func checkRethrow() -> Boom? {
    do {
      _ = try throwAssumingIsolated(self)
      return nil
    } catch let error as Boom {
      return error
    } catch {
      fatalError("unexpected error \(error)")
    }
  }
}

/// Synchronous and nonisolated: must use assumeIsolated to touch the actor
func incrementAssumingIsolated(_ actor: CustomExecutorActor) -> Int {
  actor.assumeIsolated { actor in
    actor.count += 1
    return actor.count
  }
}

func nameAssumingIsolated(_ actor: CustomExecutorActor) -> String {
  actor.assumeIsolated { actor in
    actor.name += "!"
    return actor.name
  }
}

func throwAssumingIsolated(_ actor: CustomExecutorActor) throws -> Int {
  try actor.assumeIsolated { actor in
    actor.count += 1
    throw Boom(value: actor.count)
  }
}

/// An actor sharing the custom executor of another actor
actor SharesCustomExecutor {
  let other: CustomExecutorActor

  init(other: CustomExecutorActor) {
    self.other = other
  }

  nonisolated var unownedExecutor: UnownedSerialExecutor {
    other.unownedExecutor
  }

  func incrementOther() -> Int {
    incrementAssumingIsolated(other)
  }
}

// ==== -----------------------------------------------------------------------
// MARK: Global actor with a custom executor

@globalActor
actor CustomGlobalActor {
  static let shared = CustomGlobalActor()
  static let executor = QueueExecutor(label: "CustomGlobalActor")

  nonisolated var unownedExecutor: UnownedSerialExecutor {
    Self.executor.asUnownedSerialExecutor()
  }
}

@CustomGlobalActor var customGlobalCount = 0

/// What an `assumeIsolated` for an arbitrary global actor looks like: check the
/// executor, then apply the global actor isolated closure without escaping it
func assumeCustomGlobalActor<T>(
  _ operation: @CustomGlobalActor () throws -> T
) rethrows -> T {
  CustomGlobalActor.shared.preconditionIsolated()
  return try Builtin.applyGlobalActorIsolatedUnchecked(operation)
}

func incrementCustomGlobal() -> Int {
  assumeCustomGlobalActor {
    customGlobalCount += 1
    return customGlobalCount
  }
}

func throwOnCustomGlobal() throws -> Int {
  try assumeCustomGlobalActor {
    customGlobalCount += 1
    throw Boom(value: customGlobalCount)
  }
}

@CustomGlobalActor
func onCustomGlobalActor() -> (Int, Int, Boom?) {
  let first = incrementCustomGlobal()

  let second = incrementCustomGlobal()

  var caught: Boom? = nil
  do {
    _ = try throwOnCustomGlobal()
  } catch let error as Boom {
    caught = error
  } catch {
    fatalError("unexpected error \(error)")
  }
  return (first, second, caught)
}

// ==== -----------------------------------------------------------------------
// MARK: Main actor

@MainActor var mainActorCount = 0

func incrementMainActor() -> Int {
  MainActor.assumeIsolated {
    mainActorCount += 1
    return mainActorCount
  }
}

func throwOnMainActor() throws -> Int {
  try MainActor.assumeIsolated {
    mainActorCount += 1
    throw Boom(value: mainActorCount)
  }
}

@MainActor
func onMainActor() -> (Int, Boom?, String) {
  let value = incrementMainActor()
  var caught: Boom? = nil
  do {
    _ = try throwOnMainActor()
  } catch let error as Boom {
    caught = error
  } catch {
    fatalError("unexpected error \(error)")
  }
  return (value, caught, mainActorStringViaBuiltin())
}

/// The builtin binds its generic global actor from the argument's type, so (as
/// in the stdlib) it is applied to a typed `@MainActor` parameter; a closure
/// literal passed directly carries no isolation the solver could bind
func applyOnMainActor<T>(_ operation: @MainActor () throws -> T) rethrows -> T {
  try Builtin.applyGlobalActorIsolatedUnchecked(operation)
}

@MainActor
func mainActorStringViaBuiltin() -> String {
  applyOnMainActor {
    "main-" + String(mainActorCount)
  }
}

// ==== -----------------------------------------------------------------------
// MARK: Context integrity
//
// assumeIsolated must run the closure in the caller's context, on the expected
// actor, and leave that context intact: same executor (and queue), same task,
// task locals visible, no leaked or over-released captures, and executor
// tracking still correct after a later suspension

enum Marker {
  @TaskLocal static var value = "none"
}

// @unchecked Sendable: captured by actor isolated closures from nonisolated code
final class Canary: @unchecked Sendable {
  var value = 1
}

func currentTaskHash() -> Int? {
  withUnsafeCurrentTask { $0?.hashValue }
}

func isolationIs(_ expected: AnyObject, _ isolation: isolated (any Actor)? = #isolation) -> Bool {
  guard let isolation else { return false }
  return (isolation as AnyObject) === expected
}

/// Synchronous and nonisolated; verifies the context before, inside and after
func contextCheckAssumingIsolated(
  _ actor: CustomExecutorActor, task: Int?, canary: Canary
) -> Int {
  dispatchPrecondition(condition: .onQueue(actor.executor.queue))

  let result = actor.assumeIsolated { isolatedActor in
    isolatedActor.preconditionIsolated()
    precondition(isolatedActor === actor, "closure got a different actor")
    // Evaluate #isolation here, not inside precondition's (nonisolated)
    // autoclosure, where it would be nil
    let isIsolated = isolationIs(actor)
    precondition(isIsolated, "#isolation inside the closure is not the actor")
    dispatchPrecondition(condition: .onQueue(actor.executor.queue))
    precondition(currentTaskHash() == task, "closure ran on a different task")
    precondition(Marker.value == "outer", "task local not visible in the closure")

    // Nested assumeIsolated on the same actor
    let nested = isolatedActor.assumeIsolated { again in
      precondition(again === actor)
      dispatchPrecondition(condition: .onQueue(actor.executor.queue))
      return again.count
    }
    precondition(nested == isolatedActor.count)

    isolatedActor.count += canary.value
    return isolatedActor.count
  }

  actor.preconditionIsolated()
  dispatchPrecondition(condition: .onQueue(actor.executor.queue))
  precondition(currentTaskHash() == task, "task changed after assumeIsolated")
  precondition(Marker.value == "outer", "task local lost after assumeIsolated")
  return result
}

extension CustomExecutorActor {
  func checkContextIntegrity(friend: SharesCustomExecutor) async -> Int {
    let task = currentTaskHash()
    precondition(task != nil)
    var canary = Canary()

    var total = 0
    for _ in 0 ..< 1_000 {
      total = contextCheckAssumingIsolated(self, task: task, canary: canary)
    }
    precondition(isKnownUniquelyReferenced(&canary),
                 "assumeIsolated leaked a reference to a captured object")

    // The error path leaves the context intact too
    do {
      _ = try throwAssumingIsolated(self)
      fatalError("expected a throw")
    } catch {
      self.preconditionIsolated()
      dispatchPrecondition(condition: .onQueue(executor.queue))
      precondition(currentTaskHash() == task)
    }

    // A sibling actor sharing the executor can be assumed from here as well
    let fromFriend = friend.assumeIsolated { isolatedFriend in
      isolatedFriend.preconditionIsolated()
      // Evaluate #isolation here, not inside precondition's (nonisolated)
      // autoclosure, where it would be nil
      let isIsolated = isolationIs(isolatedFriend)
      precondition(isIsolated)
      dispatchPrecondition(condition: .onQueue(executor.queue))
      return 7
    }
    precondition(fromFriend == 7)

    // Suspend and resume: executor tracking must still be correct
    await Task.yield()
    self.preconditionIsolated()
    dispatchPrecondition(condition: .onQueue(executor.queue))
    precondition(currentTaskHash() == task)

    total = contextCheckAssumingIsolated(self, task: task, canary: canary)
    precondition(isKnownUniquelyReferenced(&canary))
    return total
  }
}

@CustomGlobalActor
func checkCustomGlobalActorContextIntegrity() async -> Int {
  let task = currentTaskHash()
  precondition(task != nil)
  var canary = Canary()
  let queue = CustomGlobalActor.executor.queue

  func once() -> Int {
    let result = assumeCustomGlobalActor {
      CustomGlobalActor.shared.preconditionIsolated()
      // Evaluate #isolation here, not inside precondition's (nonisolated)
      // autoclosure, where it would be nil
      let isIsolated = isolationIs(CustomGlobalActor.shared)
      precondition(isIsolated, "#isolation inside the closure is not the global actor")
      dispatchPrecondition(condition: .onQueue(queue))
      precondition(currentTaskHash() == task)
      precondition(Marker.value == "outer")
      customGlobalCount += canary.value
      return customGlobalCount
    }
    CustomGlobalActor.shared.preconditionIsolated()
    dispatchPrecondition(condition: .onQueue(queue))
    precondition(currentTaskHash() == task)
    return result
  }

  var total = 0
  for _ in 0 ..< 1_000 { total = once() }
  precondition(isKnownUniquelyReferenced(&canary),
               "applyGlobalActorIsolatedUnchecked leaked a captured object")

  await Task.yield()
  CustomGlobalActor.shared.preconditionIsolated()
  dispatchPrecondition(condition: .onQueue(queue))
  total = once()
  return total
}

@MainActor
func checkMainActorContextIntegrity() async -> Int {
  let task = currentTaskHash()
  precondition(task != nil)
  var canary = Canary()

  func once() -> Int {
    let result = MainActor.assumeIsolated {
      MainActor.preconditionIsolated()
      // Evaluate #isolation here, not inside precondition's (nonisolated)
      // autoclosure, where it would be nil
      let isIsolated = isolationIs(MainActor.shared)
      precondition(isIsolated, "#isolation inside the closure is not the main actor")
      dispatchPrecondition(condition: .onQueue(.main))
      precondition(currentTaskHash() == task)
      precondition(Marker.value == "outer")
      mainActorCount += canary.value
      return mainActorCount
    }
    MainActor.preconditionIsolated()
    dispatchPrecondition(condition: .onQueue(.main))
    precondition(currentTaskHash() == task)
    return result
  }

  let start = mainActorCount
  var total = 0
  for _ in 0 ..< 1_000 { total = once() }
  precondition(total == start + 1_000)
  precondition(isKnownUniquelyReferenced(&canary),
               "MainActor.assumeIsolated leaked a captured object")

  await Task.yield()
  MainActor.preconditionIsolated()
  dispatchPrecondition(condition: .onQueue(.main))
  return once() - start
}

// ==== -----------------------------------------------------------------------
// MARK: Tests

@main struct Main {
  static func main() async {
    let tests = TestSuite("AssumeIsolatedCustomExecutors")

    tests.test("Actor.assumeIsolated: actor with custom executor, value results") {
      let actor = CustomExecutorActor()
      let (one, two, name) = await actor.checkValues()
      expectEqual(1, one)
      expectEqual(2, two)
      expectEqual("custom!", name)
    }

    tests.test("Actor.assumeIsolated: actor with custom executor, rethrows") {
      let actor = CustomExecutorActor()
      let caught = await actor.checkRethrow()
      expectEqual(Boom(value: 1), caught)
      expectEqual(1, await actor.count)
    }

    tests.test("Actor.assumeIsolated: from an actor sharing the custom executor") {
      let actor = CustomExecutorActor()
      let friend = SharesCustomExecutor(other: actor)
      expectEqual(1, await friend.incrementOther())
      expectEqual(2, await friend.incrementOther())
    }

    tests.test("Builtin.applyGlobalActorIsolatedUnchecked: global actor with custom executor") {
      let (first, second, caught) = await onCustomGlobalActor()
      expectEqual(1, first)
      expectEqual(2, second)
      expectEqual(Boom(value: 3), caught)
    }

    tests.test("MainActor.assumeIsolated: value result and rethrow") {
      let (value, caught, string) = await onMainActor()
      expectEqual(1, value)
      expectEqual(Boom(value: 2), caught)
      expectEqual("main-2", string)
    }

    tests.test("Actor.assumeIsolated: context integrity on a custom executor") {
      let actor = CustomExecutorActor()
      let friend = SharesCustomExecutor(other: actor)
      let total = await Marker.$value.withValue("outer") {
        await actor.checkContextIntegrity(friend: friend)
      }
      // 1000 increments, one throwing increment, one more increment
      expectEqual(1_002, total)
      expectEqual(1_002, await actor.count)
    }

    tests.test("Builtin.applyGlobalActorIsolatedUnchecked: context integrity on a custom global actor") {
      let before = await customGlobalCount
      let total = await Marker.$value.withValue("outer") {
        await checkCustomGlobalActorContextIntegrity()
      }
      expectEqual(before + 1_001, total)
    }

    tests.test("MainActor.assumeIsolated: context integrity") {
      let delta = await Marker.$value.withValue("outer") {
        await checkMainActorContextIntegrity()
      }
      expectEqual(1_001, delta)
    }

    // @Sendable: in Swift 6 mode a synchronous closure inherits main()'s main
    // actor isolation and, being passed to a non-Swift 6 module, would get a
    // dynamic main actor check that traps before the test body runs
    tests.test("Actor.assumeIsolated: wrongly assume a custom executor actor")
    .require(.crashTesting)
    .code { @Sendable in
      // A custom executor that does not recognize the current context falls
      // back to its checkIsolated(), whose default implementation crashes
      // before assumeIsolated reports its own message
      expectCrashLater(withMessage: "Unexpected isolation context, expected to be executing on QueueExecutor")
      let actor = CustomExecutorActor()
      _ = incrementAssumingIsolated(actor)
    }

    await runAllTestsAsync()
  }
}
