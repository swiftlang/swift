// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-concurrency) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency

// REQUIRES: OS=macosx || OS=ios
// FIXME: should not require Darwin to run this test once we have async main!

// for exit(:Int)
#if canImport(Darwin)
import Darwin
#elseif canImport(Glibc)
import Glibc
#endif

#if os(macOS) || os(iOS) || os(tvOS) || os(watchOS)
  import Dispatch
#endif

/// @returns true iff the expected answer is actually the case, i.e., correct.
/// If the current queue does not match expectations, this function may return
/// false or just crash the program with non-zero exit code, depending on SDK.
func checkIfMainQueue(expectedAnswer expected: Bool) -> Bool {
  // FIXME: until we start using dispatch on Linux, we only check
  // which queue we're on with Darwin platforms.
#if os(macOS) || os(iOS) || os(tvOS) || os(watchOS)
  if #available(macOS 10.12, iOS 10, tvOS 10, watchOS 3, *) {
    dispatchPrecondition(condition: expected ? .onQueue(DispatchQueue.main) 
                                             : .notOnQueue(DispatchQueue.main))
    
  }
#endif
  return true
}

actor class A {
  func onCorrectQueue() -> Bool {
    if checkIfMainQueue(expectedAnswer: false) {
      print("on actor instance's queue")
      return true
    }
    print("ERROR: not on actor instance's queue")
    return false
  }
}

@MainActor func exitTest(success: Bool) -> Never {
  if !success {
    exit(EXIT_FAILURE)
  }

  if checkIfMainQueue(expectedAnswer: true) {
    print("on main queue again!")
  } else {
    print("ERROR: left the main queue?")
  }

  exit(EXIT_SUCCESS)
}

@MainActor func enterMainActor() async -> Never {
  var ok = checkIfMainQueue(expectedAnswer: true)
  if ok {
    print("hello from main actor!")
  } else {
    print("ERROR: not on correct queue!")
  }

  // try calling a function on another actor.
  let someActor = A()
  let successfulActorSwitch = await someActor.onCorrectQueue()
  ok = ok && successfulActorSwitch

  exitTest(success: ok)
}

func someFunc() async {
  guard checkIfMainQueue(expectedAnswer: false) else {
    print("ERROR: did not expect detatched task to run on main queue!")
    exit(EXIT_FAILURE)
  }
  await enterMainActor()
}


// CHECK: starting
// CHECK-NOT: ERROR
// CHECK: hello from main actor!
// CHECK-NOT: ERROR
// CHECK: on actor instance's queue
// CHECK-NOT: ERROR
// CHECK: on main queue again!

import CoreFoundation

print("starting")
Task.runDetached(operation: someFunc)
CFRunLoopRun()

// FIXME: remove the use of CFRunLoopRun and the CoreFoundation import
// in favor of the below once we have async main support.
// don't forget to add -parse-as-library to the RUN line
/*
@main struct RunIt {
  static func main() async {
    print("starting")
    await someFunc()
  }
}
*/
