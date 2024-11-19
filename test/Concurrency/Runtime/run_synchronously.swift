// RUN: %target-run-simple-swift

// REQUIRES: executable_test
// REQUIRES: concurrency

// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime

// This test exercises the Actor.runSynchronously() operation that runs a
// synchronous block on an actor from synchronous code, blocking the thread
// until it has completed.

@_spi(Experimental) import _Concurrency

#if canImport(Darwin)
import Darwin
#endif

class Box<T> {
  var value: T
  init(value: T) {
    self.value = value
  }
}

/// Actor whose sole purpose is to count calls to nextCount(), and also build up
/// a string representation of all of the values it as counted.
actor CountingActor {
  var count = 0

  // Put the string representation of the count into a box so that we get
  // dynamic exclusivity checking for the string value in there that is
  // being constantly mutated. This will help catch bugs where we aren't
  // providing mutual exclusivity on the actor.
  let stringCount: Box<String> = Box(value: "0")

  func nextCount() -> Int {
    count = count + 1
    stringCount.value += " \(count)"
    return count
  }
}

/// Blocking synchronous call to the actor's nextCount().
@available(SwiftStdlib 6.1, *)
func blockingNextCount(_ myActor: CountingActor, offsetValue: Int) -> Int {
  return myActor.runSynchronously { myActor in
    myActor.nextCount()
  }
}

/// Perform blocking calls to nextCount() on the given actor `iterations` times.
@available(SwiftStdlib 6.1, *)
func syncQueryActor(_ myActor: CountingActor, iterations: Int, offsetValue: Int) {
  var previous = blockingNextCount(myActor, offsetValue: offsetValue)
  for _ in 1..<iterations {
    let current = blockingNextCount(myActor, offsetValue: offsetValue)
    assert(current > previous)
    previous = current
  }
}

/// Perform asynchronous calls to nextCount() on the given actor `iterations`
/// times.
func asyncQueryActor(_ myActor: CountingActor, iterations: Int, offsetValue: Int) async {
  var previous = await myActor.nextCount()
  for _ in 1..<iterations {
    let current = await myActor.nextCount()
    if current <= previous {
      print("Current \(current) vs. previous \(previous)")
    }
    
    //assert(current > previous, "Current \(current) vs. previous \(previous)")
    previous = current
  }
}

// Test configuration.
let tasksPerPriorityBucket = 500
let iterationsPerTask = 100

let myActor = CountingActor()

// Kick off a whole bunch of tasks at various priority levels. Most of them
// asynchronously call the nextCount() operation on the actor repeatedly, but we
// have one task per priority bucket perform blocking synchronous operations.
var allTasks: [Task<Void, Never>] = []
for priority: TaskPriority in [.background, .utility, .low, .medium, .high, .userInitiated] {
  for i in 0..<tasksPerPriorityBucket {
    allTasks.append(Task.detached(priority: priority) {
        if i < 1, #available(SwiftStdlib 6.1, *) {
          syncQueryActor(myActor, iterations: iterationsPerTask, offsetValue: i)
        } else {
          await asyncQueryActor(myActor, iterations: iterationsPerTask, offsetValue: i)
        }
      }
    )
  }
}

func flush() {
#if canImport(Darwin)
  fflush(stdout)
#endif
}

// Have the main thread synchronously call the nextCount() operation on the
// actor a lot.
if #available(SwiftStdlib 6.1, *) {
  syncQueryActor(myActor, iterations: iterationsPerTask, offsetValue: 17)
} else {
  await asyncQueryActor(myActor, iterations: iterationsPerTask, offsetValue: 17)
}
print("Finished main actor tasks")

// Wait for all of the tasks to finish up.
print("[", terminator: "")
flush()

let progressBarWidth = 60
let numTasks = allTasks.count
allTasks.shuffle()
for (index, task) in allTasks.enumerated() {
  await task.value
  
  let priorPrinted = if index > 0 {
    ((index - 1) * progressBarWidth) / numTasks
  } else {
    0
  }
  let printed = index * progressBarWidth / numTasks

  for _ in priorPrinted..<printed {
    print("#", terminator: "")
    flush()
  }
}
print("]")
print("Verifying result...", terminator: "")


let expectedCount = (numTasks + 1) * iterationsPerTask
var expectedString = "0"
for i in 1...expectedCount {
  expectedString += " "
  expectedString += String(i)
}
let actualString = await myActor.stringCount.value
if actualString != expectedString {
  print("Expected: \(expectedString)")
  print("Actual: \(actualString)")
}
assert(actualString == expectedString)

print(" DONE!")
