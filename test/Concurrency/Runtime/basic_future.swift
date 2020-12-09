// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-concurrency  %import-libdispatch)

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: libdispatch

import Dispatch

#if canImport(Darwin)
import Darwin
#elseif canImport(Glibc)
import Glibc
#endif

enum HomeworkError: Error, Equatable {
  case dogAteIt(String)
}

func formGreeting(name: String) async -> String {
  return "Hello \(name) from async world"
}

func testSimple(
  name: String, dogName: String, shouldThrow: Bool, doSuspend: Bool
) {
  print("Testing name: \(name), dog: \(dogName), shouldThrow: \(shouldThrow) doSuspend: \(doSuspend)")

  var completed = false

  let taskHandle = Task.runDetached { () async throws -> String in
    let greeting = await formGreeting(name: name)

    // If the intent is to test suspending, wait a bit so the second task
    // can complete.
    if doSuspend {
      print("- Future sleeping")
      sleep(1)
    }

    if (shouldThrow) {
      print("- Future throwing")
      throw HomeworkError.dogAteIt(dogName + " the dog")
    }

    print("- Future returning normally")
    return greeting + "!"
  }

  runAsyncAndBlock {
    // If the intent is not to test suspending, wait a bit so the first task
    // can complete.
    if !doSuspend {
      print("+ Reader sleeping")
      sleep(1)
    }

    do {
      print("+ Reader waiting for the result")
      let result = await try taskHandle.get()
      completed = true
      print("+ Normal return: \(result)")
      assert(result == "Hello \(name) from async world!")
    } catch HomeworkError.dogAteIt(let badDog) {
      completed = true
      print("+ Error return: HomeworkError.dogAteIt(\(badDog))")
      assert(badDog == dogName + " the dog")
    } catch {
      fatalError("Caught a different exception?")
    }
  }

  assert(completed)
  print("Finished test")
}

testSimple(name: "Ted", dogName: "Hazel", shouldThrow: false, doSuspend: false)
testSimple(name: "Ted", dogName: "Hazel", shouldThrow: true, doSuspend: false)
testSimple(name: "Ted", dogName: "Hazel", shouldThrow: false, doSuspend: true)
testSimple(name: "Ted", dogName: "Hazel", shouldThrow: true, doSuspend: true)

print("Done")
