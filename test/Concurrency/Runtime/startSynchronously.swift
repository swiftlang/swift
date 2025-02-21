// RUN: %empty-directory(%t)
// RUN: %target-build-swift -Xfrontend -disable-availability-checking %s -swift-version 6 -o %t/a.out
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

@_spi(MainActorUtilities) import _Concurrency
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
    try! await Task.sleep(for: .seconds(1))
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
    let tt = Task.startSynchronously { @MyGlobalActor in
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

print("==== ------------------------------------------------------------------")
print("syncOnMyGlobalActor()")

await Task { @MyGlobalActor in
  MyGlobalActor.shared.preconditionIsolated("Should be executing on the global actor here")
  for t in syncOnMyGlobalActor() {
    await t.value
  }
}.value

// CHECK-LABEL: syncOnMyGlobalActor()
// CHECK: Confirmed to be on @MyGlobalActor
// CHECK: schedule Task { @MyGlobalActor }, before startSynchronously [thread:[[CALLING_THREAD:0x.*]]]
// CHECK: before startSynchronously [thread:[[CALLING_THREAD]]]
// CHECK: inside startSynchronously, sleep now
// CHECK: inside Task { @MyGlobalActor }, after sleep
// resume on some other thread
// CHECK: after sleep, inside startSynchronously

print("==== ------------------------------------------------------------------")
var behavior: SynchronousTaskBehavior = .suspend
print("syncOnNonTaskThread(synchronousTask: \(behavior))")
syncOnNonTaskThread(synchronousTask: behavior)

// CHECK-LABEL: syncOnNonTaskThread(synchronousTask: suspend)
// CHECK-NEXT: before startSynchronously [thread:[[CALLING_THREAD2:0x.*]]]
// CHECK: inside startSynchronously [thread:[[CALLING_THREAD2]]]
// CHECK-NEXT: inside startSynchronously, before sleep [thread:[[CALLING_THREAD2]]]
// CHECK-NEXT: after startSynchronously, outside; cancel (wakeup) the synchronous task!  [thread:[[CALLING_THREAD2]]]
// CHECK-NEXT: inside startSynchronously, after sleep

print("==== ------------------------------------------------------------------")
behavior = .dontSuspend
print("syncOnNonTaskThread(synchronousTask: \(behavior))")
syncOnNonTaskThread(synchronousTask: behavior)

// CHECK-LABEL: syncOnNonTaskThread(synchronousTask: dontSuspend)
// CHECK-NEXT: before startSynchronously [thread:[[CALLING_THREAD3:0x.*]]]
// CHECK-NEXT: inside startSynchronously [thread:[[CALLING_THREAD3]]]
// CHECK-NEXT: inside startSynchronously, done [thread:[[CALLING_THREAD3]]]
// CHECK-NEXT: after startSynchronously, outside; cancel (wakeup) the synchronous task!  [thread:[[CALLING_THREAD3]]]

print("==== ------------------------------------------------------------------")
print("callActorFromStartSynchronousTask()")
callActorFromStartSynchronousTask()

// FIXME: this continues using our task on the actor

actor Recipient {
  func sync(syncTaskThreadID: ThreadID) {
    self.preconditionIsolated()
    if compareThreadIDs(syncTaskThreadID, .equal, getCurrentThreadID()) {
      print("ERROR! Sync start thread id = \(syncTaskThreadID)")
      print("ERROR! Current actor thread id = \(getCurrentThreadID())")
      print("ERROR! Actor must not run on the synchronous task's thread")
    }
  }

  func async(syncTaskThreadID: ThreadID) async {
    self.preconditionIsolated()
    if compareThreadIDs(syncTaskThreadID, .equal, getCurrentThreadID()) {
      print("ERROR! Sync start thread id = \(syncTaskThreadID)")
      print("ERROR! Current actor thread id = \(getCurrentThreadID())")
      print("ERROR! Actor must not run on the synchronous task's thread")
    }

    await Task {
      self.preconditionIsolated()
    }.value
  }
}

func callActorFromStartSynchronousTask() {
  let sem1 = DispatchSemaphore(value: 0)
  let sem2 = DispatchSemaphore(value: 0)
  let queue = DispatchQueue(label: "CustomQueue")

  queue.async {
    let outerTID = getCurrentThreadID()
    let tt = Task.startSynchronously {
      let innerTID = getCurrentThreadID()
      precondition(compareThreadIDs(outerTID, .equal, innerTID), "Outer Thread ID must be equal Thread ID inside runSynchronously synchronous part!")
      print("inside startSynchronously [thread:\(getCurrentThreadID())] @ :\(#line)")

      let rec = Recipient()

      print("inside startSynchronously, call rec.sync() [thread:\(getCurrentThreadID())] @ :\(#line)")
      await rec.sync(syncTaskThreadID: innerTID)
      print("inside startSynchronously, call rec.sync() done [thread:\(getCurrentThreadID())] @ :\(#line)")

      // after suspension we are supposed to hop off to the global pool,
      // thus the thread IDs cannot be the same anymore
      if compareThreadIDs(innerTID, .equal, getCurrentThreadID()) {
        print("ERROR! Inner thread id = \(innerTID)")
        print("ERROR! Current thread id = \(getCurrentThreadID())")
        print("ERROR! Task resumed on same thread as it entered the synchronous task!")
      }

      print("inside startSynchronously, call rec.async() [thread:\(getCurrentThreadID())] @ :\(#line)")
      await rec.async(syncTaskThreadID: innerTID)
      print("inside startSynchronously, done [thread:\(getCurrentThreadID())] @ :\(#line)")

      if compareThreadIDs(innerTID, .equal, getCurrentThreadID()) {
        print("ERROR! Inner thread id = \(innerTID)")
        print("ERROR! Current thread id = \(getCurrentThreadID())")
        print("ERROR! Task resumed on same thread as it entered the synchronous task!")
      }
      print("inside startSynchronously, done [thread:\(getCurrentThreadID())] @ :\(#line)")
    }

    print("after startSynchronously [thread:\(getCurrentThreadID())] @ :\(#line)")
  }

//  sem2.wait()
}

try? await Task.sleep(for: .seconds(3))