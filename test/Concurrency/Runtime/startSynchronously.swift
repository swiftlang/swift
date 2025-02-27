// REQUIRES: rdar145735542
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
}

// Test on all platforms
func syncOnMyGlobalActor() -> [Task<Void, Never>] {
  MyGlobalActor.shared.preconditionIsolated("Should be executing on the global actor here")
  print("Confirmed to be on @MyGlobalActor")

  // This task must be guaranteed to happen AFTER 'tt' because we are already on this actor
  // so this enqueue must happen after we give up the actor.
  print("schedule Task { @MyGlobalActor }, before startSynchronously [thread:\(getCurrentThreadID())] @ :\(#line)")
  let t1 = Task { @MyGlobalActor in
    print("inside Task { @MyGlobalActor }, after sleep [thread:\(getCurrentThreadID())] @ :\(#line)")
  }

  print("before startSynchronously [thread:\(getCurrentThreadID())] @ :\(#line)")
  let outerTID = getCurrentThreadID()
  let tt = Task.startSynchronously { @MyGlobalActor in
    let innerTID = getCurrentThreadID()
    print("inside startSynchronously, outer thread = \(outerTID)")
    print("inside startSynchronously, inner thread = \(innerTID)")
    if (compareThreadIDs(outerTID, .notEqual, innerTID)) {
      print("ERROR! Outer Thread ID must be equal Thread ID inside runSynchronously synchronous part!")
    }

    print("inside startSynchronously, sleep now [thread:\(getCurrentThreadID())] @ :\(#line)")
    _ = try? await Task.sleep(for: .seconds(1))
    print("after sleep, inside startSynchronously [thread:\(getCurrentThreadID())] @ :\(#line)")
  }

  return [t1, tt]
}

func syncOnNonTaskThread(synchronousTask behavior: SynchronousTaskBehavior) {
  let sem1 = DispatchSemaphore(value: 0)
  let sem2 = DispatchSemaphore(value: 0)
  let queue = DispatchQueue(label: "CustomQueue")

  queue.async {
    // This is in order so we don't have a "current task" nor any "current executor"
    print("before startSynchronously [thread:\(getCurrentThreadID())] @ :\(#line)")

    let outerTID = getCurrentThreadID()
    let tt = Task.startSynchronously {
      dispatchPrecondition(condition: .onQueue(queue))

      let innerTID = getCurrentThreadID()
      if compareThreadIDs(outerTID, .notEqual, innerTID) {
        print("inside startSynchronously, outer thread = \(outerTID)")
        print("inside startSynchronously, inner thread = \(innerTID)")
        print("ERROR! Outer Thread ID must be equal Thread ID inside runSynchronously synchronous part!")
      }
      print("inside startSynchronously [thread:\(getCurrentThreadID())] @ :\(#line)")

      switch behavior {
      case .suspend:
        // sleep until woken up by outer task; i.e. actually suspend
        print("inside startSynchronously, before sleep [thread:\(getCurrentThreadID())] @ :\(#line)")
        _ = try? await Task.sleep(for: .seconds(10))
        print("inside startSynchronously, after sleep [thread:\(getCurrentThreadID())] @ :\(#line)")
      case .dontSuspend:
        print("inside startSynchronously, done [thread:\(getCurrentThreadID())] @ :\(#line)")
        ()
      }
      sem1.signal()
    }
    print("after startSynchronously, outside; cancel (wakeup) the synchronous task! [thread:\(getCurrentThreadID())] @ :\(#line)")
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
// CHECK: schedule Task { @MyGlobalActor }, before startSynchronously [thread:[[CALLING_THREAD:.*]]]
// CHECK: before startSynchronously [thread:[[CALLING_THREAD]]]
// CHECK-NOT: ERROR!
// CHECK: inside startSynchronously, sleep now
// CHECK: inside Task { @MyGlobalActor }, after sleep
// resume on some other thread
// CHECK: after sleep, inside startSynchronously

print("\n\n==== ------------------------------------------------------------------")
var behavior: SynchronousTaskBehavior = .suspend
print("syncOnNonTaskThread(synchronousTask: \(behavior))")
syncOnNonTaskThread(synchronousTask: behavior)

// CHECK-LABEL: syncOnNonTaskThread(synchronousTask: suspend)
// No interleaving allowed between "before" and "inside":
// CHECK-NEXT: before startSynchronously [thread:[[CALLING_THREAD2:.*]]]
// CHECK-NOT: ERROR!
// CHECK-NEXT: inside startSynchronously [thread:[[CALLING_THREAD2]]]
// CHECK-NEXT: inside startSynchronously, before sleep [thread:[[CALLING_THREAD2]]]
// CHECK-NEXT: after startSynchronously, outside; cancel (wakeup) the synchronous task!  [thread:[[CALLING_THREAD2]]]
// CHECK-NEXT: inside startSynchronously, after sleep

print("\n\n==== ------------------------------------------------------------------")
behavior = .dontSuspend
print("syncOnNonTaskThread(synchronousTask: \(behavior))")
syncOnNonTaskThread(synchronousTask: behavior)

// CHECK-LABEL: syncOnNonTaskThread(synchronousTask: dontSuspend)
// CHECK-NEXT: before startSynchronously [thread:[[CALLING_THREAD3:.*]]]
// CHECK-NOT: ERROR!
// CHECK-NEXT: inside startSynchronously [thread:[[CALLING_THREAD3]]]
// CHECK: inside startSynchronously, done [thread:[[CALLING_THREAD3]]]
// CHECK: after startSynchronously, outside; cancel (wakeup) the synchronous task!  [thread:[[CALLING_THREAD3]]]

print("\n\n==== ------------------------------------------------------------------")
print("callActorFromStartSynchronousTask()")
callActorFromStartSynchronousTask(recipient: .recipient(Recipient()))

// CHECK: callActorFromStartSynchronousTask()
// No interleaving allowed between "before" and "inside":
// CHECK: before startSynchronously [thread:[[CALLING_THREAD4:.*]]]
// CHECK-NEXT: inside startSynchronously [thread:[[CALLING_THREAD4]]]

// It is important that as we suspend on the actor call, the 'after' startSynchronously gets to run
// CHECK-NEXT: inside startSynchronously, call rec.sync() [thread:[[CALLING_THREAD4]]]
// CHECK: after startSynchronously
// CHECK-NOT: ERROR!
// CHECK: inside startSynchronously, call rec.sync() done

// CHECK-NOT: ERROR!
// CHECK: inside startSynchronously, call rec.async()
// CHECK-NOT: ERROR!
// CHECK: inside startSynchronously, call rec.async() done

// CHECK-NOT: ERROR!
// CHECK: inside startSynchronously, done

/// Don't want to involve protocol calls to not confuse the test with additional details,
/// so we use concrete types here.
enum TargetActorToCall {
  case recipient(Recipient)
  case recipientOnQueue(RecipientOnQueue)
}

protocol RecipientProtocol where Self: Actor {
  func sync(syncTaskThreadID: ThreadID) async
  func async(syncTaskThreadID: ThreadID) async
}

// default actor, must not declare an 'unownedExecutor'
actor Recipient {
  func sync(syncTaskThreadID: ThreadID) {
    self.preconditionIsolated()

    print("\(Recipient.self)/\(#function) Current actor thread id = \(getCurrentThreadID()) @ :\(#line)")
    if compareThreadIDs(syncTaskThreadID, .equal, getCurrentThreadID()) {
      print("NOTICE: Actor must not run on the synchronous task's thread :\(#line)")
    }
  }

  func async(syncTaskThreadID: ThreadID) async {
    self.preconditionIsolated()

    // Dispatch may end up reusing the thread used to service the queue so we
    // cannot truly assert exact thread identity in such tests.
    // Usually this will be on a different thread by now though.
    print("\(Recipient.self)/\(#function) Current actor thread id = \(getCurrentThreadID()) @ :\(#line)")
    if compareThreadIDs(syncTaskThreadID, .equal, getCurrentThreadID()) {
      print("NOTICE: Actor must not run on the synchronous task's thread :\(#line)")
    }

    await Task {
      self.preconditionIsolated()
    }.value
  }
}

func callActorFromStartSynchronousTask(recipient rec: TargetActorToCall) {
  let sem1 = DispatchSemaphore(value: 0)
  let sem2 = DispatchSemaphore(value: 0)
  let queue = DispatchQueue(label: "CustomQueue")

  queue.async {
    let outerTID = getCurrentThreadID()
    print("before startSynchronously [thread:\(outerTID)] @ :\(#line)")
    let tt = Task.startSynchronously {
      dispatchPrecondition(condition: .onQueue(queue))

      let innerTID = getCurrentThreadID()
      precondition(compareThreadIDs(outerTID, .equal, innerTID), "Outer Thread ID must be equal Thread ID inside runSynchronously synchronous part!")
      print("inside startSynchronously [thread:\(getCurrentThreadID())] @ :\(#line)")

      for i in 1..<10 {
        queue.async {
          print("- async work on queue")
        }
      }

      print("inside startSynchronously, call rec.sync() [thread:\(getCurrentThreadID())] @ :\(#line)")
      switch rec {
      case .recipient(let recipient): await recipient.sync(syncTaskThreadID: innerTID)
      case .recipientOnQueue(let recipient): await recipient.sync(syncTaskThreadID: innerTID)
      }
      print("inside startSynchronously, call rec.sync() done [thread:\(getCurrentThreadID())] @ :\(#line)")

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

      print("inside startSynchronously, call rec.async() [thread:\(getCurrentThreadID())] @ :\(#line)")
      switch rec {
      case .recipient(let recipient): await recipient.async(syncTaskThreadID: innerTID)
      case .recipientOnQueue(let recipient): await recipient.async(syncTaskThreadID: innerTID)
      }
      print("inside startSynchronously, call rec.async() done [thread:\(getCurrentThreadID())] @ :\(#line)")

      print("Inner thread id = \(innerTID)")
      print("Current thread id = \(getCurrentThreadID())")
      // Dispatch may end up reusing the thread used to service the queue so we
      // cannot truly assert exact thread identity in such tests.
      // Usually this will be on a different thread by now though.
      if compareThreadIDs(innerTID, .equal, getCurrentThreadID()) {
        print("NOTICE: Task resumed on same thread as it entered the synchronous task!")
      }

      print("inside startSynchronously, done [thread:\(getCurrentThreadID())] @ :\(#line)")
      sem1.signal()
    }

    print("after startSynchronously [thread:\(getCurrentThreadID())] @ :\(#line)")
    sem2.signal()
  }

  sem1.wait()
  sem2.wait()
}

print("\n\n==== ------------------------------------------------------------------")
print("callActorFromStartSynchronousTask() - actor in custom executor with its own queue")
let actorQueue = DispatchQueue(label: "recipient-actor-queue")
callActorFromStartSynchronousTask(recipient: .recipientOnQueue(RecipientOnQueue(queue: actorQueue)))

// CHECK-LABEL: callActorFromStartSynchronousTask() - actor in custom executor with its own queue
// No interleaving allowed between "before" and "inside":
// CHECK: before startSynchronously [thread:[[CALLING_THREAD4:.*]]]
// CHECK-NEXT: inside startSynchronously [thread:[[CALLING_THREAD4]]]

// As we call into an actor, we must enqueue to its custom executor;
// Make sure the enqueue happens as expected and only then do we give up the calling thread
// allowing the 'after startSynchronously' to run.
//
// CHECK-NEXT: inside startSynchronously, call rec.sync() [thread:[[CALLING_THREAD4]]]
// CHECK: NaiveQueueExecutor(recipient-actor-queue) enqueue
// CHECK: after startSynchronously
// CHECK-NOT: ERROR!
// CHECK: inside startSynchronously, call rec.sync() done

// CHECK-NOT: ERROR!
// CHECK: inside startSynchronously, call rec.async()
// CHECK: NaiveQueueExecutor(recipient-actor-queue) enqueue
// CHECK-NOT: ERROR!
// CHECK: inside startSynchronously, call rec.async() done

// CHECK-NOT: ERROR!
// CHECK: inside startSynchronously, done

final class NaiveQueueExecutor: SerialExecutor {
  let queue: DispatchQueue

  init(queue: DispatchQueue) {
    self.queue = queue
  }

  public func enqueue(_ job: consuming ExecutorJob) {
    let unowned = UnownedJob(job)
    print("NaiveQueueExecutor(\(self.queue.label)) enqueue... [thread:\(getCurrentThreadID())]")
    queue.async {
    print("NaiveQueueExecutor(\(self.queue.label)) enqueue: run [thread:\(getCurrentThreadID())]")
      unowned.runSynchronously(on: self.asUnownedSerialExecutor())
    }
  }
}

actor RecipientOnQueue {
  let executor: NaiveQueueExecutor
  nonisolated let unownedExecutor: UnownedSerialExecutor

  init(queue: DispatchQueue) {
    self.executor = NaiveQueueExecutor(queue: queue)
    self.unownedExecutor = executor.asUnownedSerialExecutor()
  }

  func sync(syncTaskThreadID: ThreadID) {
    self.preconditionIsolated()
    dispatchPrecondition(condition: .onQueue(self.executor.queue))

    print("\(Recipient.self)/\(#function) Current actor thread id = \(getCurrentThreadID()) @ :\(#line)")
    if compareThreadIDs(syncTaskThreadID, .equal, getCurrentThreadID()) {
      print("NOTICE: Actor must not run on the synchronous task's thread :\(#line)")
    }
  }

  func async(syncTaskThreadID: ThreadID) async {
    self.preconditionIsolated()
    dispatchPrecondition(condition: .onQueue(self.executor.queue))

    // Dispatch may end up reusing the thread used to service the queue so we
    // cannot truly assert exact thread identity in such tests.
    // Usually this will be on a different thread by now though.
    print("\(Recipient.self)/\(#function) Current actor thread id = \(getCurrentThreadID()) @ :\(#line)")
    if compareThreadIDs(syncTaskThreadID, .equal, getCurrentThreadID()) {
      print("NOTICE: Actor must not run on the synchronous task's thread :\(#line)")
    }

    await Task {
      self.preconditionIsolated()
    }.value
  }
}
