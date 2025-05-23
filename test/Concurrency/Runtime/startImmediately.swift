// RUN: %empty-directory(%t)
// RUN: %target-build-swift -Xfrontend -disable-availability-checking %s %import-libdispatch -swift-version 6 -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s --dump-input=always

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime
// REQUIRES: libdispatch

// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: back_deploy_concurrency
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: freestanding

import _Concurrency
import Dispatch

enum CompareHow {
  case equal
  case notEqual

  func check(_ wasEqual: Bool) -> Bool {
    switch self {
    case .equal: wasEqual
    case .notEqual: !wasEqual
    }
  }
}

#if canImport(Darwin)
import Darwin
typealias ThreadID = pthread_t
func getCurrentThreadID() -> ThreadID { pthread_self() }
func compareThreadIDs(_ a: ThreadID, _ how: CompareHow, _ b: ThreadID) -> Bool {
  how.check(pthread_equal(a, b) != 0)
}
#elseif canImport(Glibc)
import Glibc
typealias ThreadID = pthread_t
func getCurrentThreadID() -> ThreadID { pthread_self() }
func compareThreadIDs(_ a: ThreadID, _ how: CompareHow, _ b: ThreadID) -> Bool {
  how.check(pthread_equal(a, b) != 0)
}
#elseif os(Windows)
import WinSDK
typealias ThreadID = UInt32
func getCurrentThreadID() -> ThreadID { GetCurrentThreadId() }
func compareThreadIDs(_ a: ThreadID, _ how: CompareHow, _ b: ThreadID) -> Bool {
  how.check(a == b)
}
#elseif os(WASI)
typealias ThreadID = UInt32
func getCurrentThreadID() -> ThreadID { 0 }
func compareThreadIDs(_ a: ThreadID, _ how: CompareHow, _ b: ThreadID) -> Bool {
  how.check(a == b)
}
#endif
extension ThreadID: @unchecked Sendable {}

@globalActor
actor MyGlobalActor {
  static let shared: MyGlobalActor = MyGlobalActor()

  @MyGlobalActor
  static func test() {}
}

final class NaiveQueueExecutor: SerialExecutor {
  let queue: DispatchQueue

  init(queue: DispatchQueue) {
    self.queue = queue
  }

  public func enqueue(_ job: consuming ExecutorJob) {
    let unowned = UnownedJob(job)
    print("NaiveQueueExecutor(\(self.queue.label)) enqueue [thread:\(getCurrentThreadID())]")
    queue.async {
      unowned.runSynchronously(on: self.asUnownedSerialExecutor())
    }
  }
}

@globalActor
actor DifferentGlobalActor {
  static let queue = DispatchQueue(label: "DifferentGlobalActor-queue")
  let executor: NaiveQueueExecutor
  nonisolated let unownedExecutor: UnownedSerialExecutor

  init() {
    self.executor = NaiveQueueExecutor(queue: DifferentGlobalActor.queue)
    self.unownedExecutor = executor.asUnownedSerialExecutor()
  }

  static let shared: DifferentGlobalActor = DifferentGlobalActor()

  @DifferentGlobalActor
  static func test() {}
}

// Test on all platforms
func syncOnMyGlobalActor() -> [Task<Void, Never>] {
  MyGlobalActor.shared.preconditionIsolated("Should be executing on the global actor here")
  print("Confirmed to be on @MyGlobalActor")

  // This task must be guaranteed to happen AFTER 'tt' because we are already on this actor
  // so this enqueue must happen after we give up the actor.
  print("schedule Task { @MyGlobalActor }, before immediate [thread:\(getCurrentThreadID())] @ :\(#line)")
  let t1 = Task { @MyGlobalActor in
    print("inside Task { @MyGlobalActor }, after sleep [thread:\(getCurrentThreadID())] @ :\(#line)")
  }

  print("before immediate [thread:\(getCurrentThreadID())] @ :\(#line)")
  let outerTID = getCurrentThreadID()
  let tt = Task.immediate { @MyGlobalActor in
    let innerTID = getCurrentThreadID()
    print("inside immediate, outer thread = \(outerTID)")
    print("inside immediate, inner thread = \(innerTID)")
    if (compareThreadIDs(outerTID, .notEqual, innerTID)) {
      print("ERROR! Outer Thread ID must be equal Thread ID inside runSynchronously synchronous part!")
    }

    print("inside immediate, sleep now [thread:\(getCurrentThreadID())] @ :\(#line)")
    _ = try? await Task.sleep(for: .seconds(1))
    print("after sleep, inside immediate [thread:\(getCurrentThreadID())] @ :\(#line)")
  }

  return [t1, tt]
}

func syncOnMyGlobalActorHopToDifferentActor() -> [Task<Void, Never>] {
  MyGlobalActor.shared.preconditionIsolated("Should be executing on the global actor here")
  print("Confirmed to be on @MyGlobalActor")

  // This task must be guaranteed to happen AFTER 'tt' because we are already on this actor
  // so this enqueue must happen after we give up the actor.
  print("schedule Task { @DifferentGlobalActor }, before immediate [thread:\(getCurrentThreadID())] @ :\(#line)")
  let t1 = Task { @DifferentGlobalActor in
    print("inside Task { @DifferentGlobalActor } [thread:\(getCurrentThreadID())] @ :\(#line)")
    DifferentGlobalActor.shared.preconditionIsolated("Expected Task{} to be on DifferentGlobalActor")
  }

  print("before immediate [thread:\(getCurrentThreadID())] @ :\(#line)")
  let outerTID = getCurrentThreadID()
  let tt = Task.immediate { @DifferentGlobalActor in
    let innerTID = getCurrentThreadID()
    print("inside immediate, outer thread = \(outerTID)")
    print("inside immediate, inner thread = \(innerTID)")
    if (compareThreadIDs(outerTID, .equal, innerTID)) {
      // This case specifically is NOT synchronously run because we specified a different isolation for the closure
      // and FORCED a hop to the DifferentGlobalActor executor.
      print("ERROR! Outer Thread ID must NOT equal Thread ID inside runSynchronously synchronous part!")
    }
    // We crucially need to see this task be enqueued on the different global actor,
    // so it did not execute "synchronously" after all - it had to hop to the other actor.
    dispatchPrecondition(condition: .onQueue(DifferentGlobalActor.queue))
    DifferentGlobalActor.shared.preconditionIsolated("Expected Task.immediate { @DifferentGlobalActor in } to be on DifferentGlobalActor")

    print("inside immediate, sleep now [thread:\(getCurrentThreadID())] @ :\(#line)")
    _ = try? await Task.sleep(for: .milliseconds(100))

    print("inside immediate, after sleep [thread:\(getCurrentThreadID())] @ :\(#line)")
    dispatchPrecondition(condition: .onQueue(DifferentGlobalActor.queue))
    DifferentGlobalActor.shared.preconditionIsolated("Expected Task.immediate { @DifferentGlobalActor in } to be on DifferentGlobalActor")

    // do something here
    await MyGlobalActor.test()
    DifferentGlobalActor.test()
  }

  return [t1, tt]
}

func syncOnNonTaskThread(synchronousTask behavior: SynchronousTaskBehavior) {
  let sem1 = DispatchSemaphore(value: 0)
  let sem2 = DispatchSemaphore(value: 0)
  let queue = DispatchQueue(label: "CustomQueue")

  queue.async {
    // This is in order so we don't have a "current task" nor any "current executor"
    print("before immediate [thread:\(getCurrentThreadID())] @ :\(#line)")

    let outerTID = getCurrentThreadID()
    let tt = Task.immediate {
      dispatchPrecondition(condition: .onQueue(queue))

      let innerTID = getCurrentThreadID()
      if compareThreadIDs(outerTID, .notEqual, innerTID) {
        print("inside immediate, outer thread = \(outerTID)")
        print("inside immediate, inner thread = \(innerTID)")
        print("ERROR! Outer Thread ID must be equal Thread ID inside runSynchronously synchronous part!")
      }
      print("inside immediate [thread:\(getCurrentThreadID())] @ :\(#line)")

      switch behavior {
      case .suspend:
        // sleep until woken up by outer task; i.e. actually suspend
        print("inside immediate, before sleep [thread:\(getCurrentThreadID())] @ :\(#line)")
        _ = try? await Task.sleep(for: .seconds(10))
        print("inside immediate, after sleep [thread:\(getCurrentThreadID())] @ :\(#line)")
      case .dontSuspend:
        print("inside immediate, done [thread:\(getCurrentThreadID())] @ :\(#line)")
        ()
      }
      sem1.signal()
    }
    print("after immediate, outside; cancel (wakeup) the synchronous task! [thread:\(getCurrentThreadID())] @ :\(#line)")
    tt.cancel() // wake up the sleep

    sem1.wait()
    sem2.signal()
  }

  sem2.wait()
}

enum SynchronousTaskBehavior {
  case suspend
  case dontSuspend
}

print("\n\n==== ------------------------------------------------------------------")
print("syncOnMyGlobalActor()")

await Task { @MyGlobalActor in
  MyGlobalActor.shared.preconditionIsolated("Should be executing on the global actor here")
  for t in syncOnMyGlobalActor() {
    await t.value
  }
}.value

// CHECK-LABEL: syncOnMyGlobalActor()
// CHECK: Confirmed to be on @MyGlobalActor
// CHECK: schedule Task { @MyGlobalActor }, before immediate [thread:[[CALLING_THREAD:.*]]]
// CHECK: before immediate [thread:[[CALLING_THREAD]]]
// CHECK-NOT: ERROR!
// CHECK: inside immediate, sleep now
// CHECK: inside Task { @MyGlobalActor }, after sleep
// resume on some other thread
// CHECK: after sleep, inside immediate

print("\n\n==== ------------------------------------------------------------------")
print("syncOnMyGlobalActorHopToDifferentActor()")

await Task { @MyGlobalActor in
  MyGlobalActor.shared.preconditionIsolated("Should be executing on the global actor here")
  for t in syncOnMyGlobalActorHopToDifferentActor() {
    await t.value
  }
}.value

// Assertion Notes: We expect the task to be on the specified queue as we force the Task.immediate
// task to enqueue on the DifferentGlobalActor, however we CANNOT use threads to verify this behavior,
// because dispatch may still pull tricks and reuse threads. We can only verify that we're on the right
// queue, and that the `enqueue` calls on the target executor happen when we expect them to.
//
// CHECK: syncOnMyGlobalActorHopToDifferentActor()
// CHECK: Confirmed to be on @MyGlobalActor
// CHECK: before immediate

// This IS actually enqueueing on the target actor (not synchronous), as expected:
// CHECK: NaiveQueueExecutor(DifferentGlobalActor-queue) enqueue
// CHECK: inside immediate, sleep now

// After the sleep we get back onto the specified executor as expected
// CHECK: NaiveQueueExecutor(DifferentGlobalActor-queue) enqueue
// CHECK: inside immediate, after sleep

print("\n\n==== ------------------------------------------------------------------")
var behavior: SynchronousTaskBehavior = .suspend
print("syncOnNonTaskThread(synchronousTask: \(behavior))")
syncOnNonTaskThread(synchronousTask: behavior)

// CHECK-LABEL: syncOnNonTaskThread(synchronousTask: suspend)
// No interleaving allowed between "before" and "inside":
// CHECK-NEXT: before immediate [thread:[[CALLING_THREAD2:.*]]]
// CHECK-NOT: ERROR!
// CHECK-NEXT: inside immediate [thread:[[CALLING_THREAD2]]]
// CHECK-NEXT: inside immediate, before sleep [thread:[[CALLING_THREAD2]]]
// CHECK-NEXT: after immediate, outside; cancel (wakeup) the synchronous task!  [thread:[[CALLING_THREAD2]]]
// CHECK-NEXT: inside immediate, after sleep

print("\n\n==== ------------------------------------------------------------------")
behavior = .dontSuspend
print("syncOnNonTaskThread(synchronousTask: \(behavior))")
syncOnNonTaskThread(synchronousTask: behavior)

// CHECK-LABEL: syncOnNonTaskThread(synchronousTask: dontSuspend)
// CHECK-NEXT: before immediate [thread:[[CALLING_THREAD3:.*]]]
// CHECK-NOT: ERROR!
// CHECK-NEXT: inside immediate [thread:[[CALLING_THREAD3]]]
// CHECK: inside immediate, done [thread:[[CALLING_THREAD3]]]
// CHECK: after immediate, outside; cancel (wakeup) the synchronous task!  [thread:[[CALLING_THREAD3]]]

print("\n\n==== ------------------------------------------------------------------")
print("callActorFromStartSynchronousTask() - not on specific queue")
callActorFromStartSynchronousTask(recipient: .recipient(Recipient()))

// CHECK: callActorFromStartSynchronousTask()
// No interleaving allowed between "before" and "inside":
// CHECK: before immediate [thread:[[CALLING_THREAD4:.*]]]
// CHECK-NEXT: inside immediate [thread:[[CALLING_THREAD4]]]

// It is important that as we suspend on the actor call, the 'after' immediate gets to run
// CHECK-NEXT: inside immediate, call rec.sync() [thread:[[CALLING_THREAD4]]]
// CHECK: after immediate
// CHECK-NOT: ERROR!
// CHECK: inside immediate, call rec.sync() done

// CHECK-NOT: ERROR!
// CHECK: inside immediate, done

/// Don't want to involve protocol calls to not confuse the test with additional details,
/// so we use concrete types here.
enum TargetActorToCall {
  case recipient(Recipient)
  case recipientOnQueue(RecipientOnQueue)
}

protocol RecipientProtocol where Self: Actor {
  func callAndSuspend(syncTaskThreadID: ThreadID) async
}

// default actor, must not declare an 'unownedExecutor'
actor Recipient: RecipientProtocol {
  func callAndSuspend(syncTaskThreadID: ThreadID) async {
    self.preconditionIsolated()

    print("\(Recipient.self)/\(#function) Current actor thread id = \(getCurrentThreadID()) @ :\(#line)")
    if compareThreadIDs(syncTaskThreadID, .equal, getCurrentThreadID()) {
      print("NOTICE: Actor must not run on the synchronous task's thread :\(#line)")
    }

    try? await Task.sleep(for: .milliseconds(100))
  }
}

func callActorFromStartSynchronousTask(recipient rec: TargetActorToCall) {
  let sem1 = DispatchSemaphore(value: 0)
  let sem2 = DispatchSemaphore(value: 0)
  let queue = DispatchQueue(label: "CustomQueue")

  queue.async {
    let outerTID = getCurrentThreadID()
    print("before immediate [thread:\(outerTID)] @ :\(#line)")
    let tt = Task.immediate {
      dispatchPrecondition(condition: .onQueue(queue))

      let innerTID = getCurrentThreadID()
      precondition(compareThreadIDs(outerTID, .equal, innerTID), "Outer Thread ID must be equal Thread ID inside runSynchronously synchronous part!")
      print("inside immediate [thread:\(getCurrentThreadID())] @ :\(#line)")

      for i in 1..<10 {
        queue.async {
          print("- async work on queue")
        }
      }

      print("inside immediate, call rec.sync() [thread:\(getCurrentThreadID())] @ :\(#line)")
      switch rec {
      case .recipient(let recipient): await recipient.callAndSuspend(syncTaskThreadID: innerTID)
      case .recipientOnQueue(let recipient): await recipient.callAndSuspend(syncTaskThreadID: innerTID)
      }
      print("inside immediate, call rec.sync() done [thread:\(getCurrentThreadID())] @ :\(#line)")

      // after suspension we are supposed to hop off to the global pool,
      // thus the thread IDs cannot be the same anymore
      print("Inner thread id = \(innerTID)")
      print("Current thread id = \(getCurrentThreadID())")
      // Dispatch may end up reusing the thread used to service the queue so we
      // cannot truly assert exact thread identity in such tests.
      // Usually this will be on a different thread by now though.
      if compareThreadIDs(innerTID, .equal, getCurrentThreadID()) {
        print("NOTICE: Task resumed on same thread as it entered the synchronous task!")
      }

      print("inside immediate, done [thread:\(getCurrentThreadID())] @ :\(#line)")
      sem1.signal()
    }

    print("after immediate [thread:\(getCurrentThreadID())] @ :\(#line)")
    sem2.signal()
  }

  sem1.wait()
  sem2.wait()
}

print("\n\n==== ------------------------------------------------------------------")
print("callActorFromStartSynchronousTask() - actor in custom executor with its own queue")
let actorQueue = DispatchQueue(label: "recipient-actor-queue")
callActorFromStartSynchronousTask(recipient: .recipientOnQueue(RecipientOnQueue(queue: actorQueue)))


//            50: callActorFromStartSynchronousTask()
//            51: before immediate [thread:0x00007000054f5000] @ :366
//            52: inside immediate [thread:0x00007000054f5000] @ :372
//            53: inside immediate, call rec.sync() [thread:0x00007000054f5000] @ :380
//            54: Recipient/sync(syncTaskThreadID:) Current actor thread id = 0x000070000567e000 @ :336
//            55: inside immediate, call rec.sync() done [thread:0x000070000567e000] @ :385
//            56: Inner thread id = 0x00007000054f5000
//            57: Current thread id = 0x000070000567e000
//            60: after immediate [thread:0x00007000054f5000] @ :418
//            61: - async work on queue
//            62: - async work on queue
//            63: - async work on queue
//            64: - async work on queue
//            65: - async work on queue
//            67: - async work on queue
//            68: - async work on queue
//            69: - async work on queue
//            71: Inner thread id = 0x00007000054f5000
//            72: Current thread id = 0x000070000567e000
//            73: inside immediate, done [thread:0x000070000567e000] @ :414

// CHECK-LABEL: callActorFromStartSynchronousTask() - actor in custom executor with its own queue
// No interleaving allowed between "before" and "inside":
// CHECK: before immediate [thread:[[CALLING_THREAD4:.*]]]
// CHECK-NEXT: inside immediate [thread:[[CALLING_THREAD4]]]

// As we call into an actor, we must enqueue to its custom executor;
// Make sure the enqueue happens as expected and only then do we give up the calling thread
// allowing the 'after immediate' to run.
//
// CHECK-NEXT: inside immediate, call rec.sync() [thread:[[CALLING_THREAD4]]]
// CHECK: after immediate
// CHECK-NOT: ERROR!
// CHECK: inside immediate, call rec.sync() done

// CHECK-NOT: ERROR!
// CHECK: inside immediate, done

actor RecipientOnQueue: RecipientProtocol {
  let executor: NaiveQueueExecutor
  nonisolated let unownedExecutor: UnownedSerialExecutor

  init(queue: DispatchQueue) {
    self.executor = NaiveQueueExecutor(queue: queue)
    self.unownedExecutor = executor.asUnownedSerialExecutor()
  }

  func callAndSuspend(syncTaskThreadID: ThreadID) async {
    self.preconditionIsolated()
    dispatchPrecondition(condition: .onQueue(self.executor.queue))

    print("\(Recipient.self)/\(#function) Current actor thread id = \(getCurrentThreadID()) @ :\(#line)")
    if compareThreadIDs(syncTaskThreadID, .equal, getCurrentThreadID()) {
      print("NOTICE: Actor must not run on the synchronous task's thread :\(#line)")
    }

    try? await Task.sleep(for: .milliseconds(100))
  }
}
