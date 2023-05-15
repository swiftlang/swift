// RUN: %target-run-simple-swift( -Xfrontend -sil-verify-all -Xfrontend -disable-availability-checking %import-libdispatch -parse-as-library)

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: freestanding

// rdar://76038845
// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime

@_spi(_TaskToThreadModel) import _Concurrency
import StdlibUnittest
import Darwin

@available(SwiftStdlib 5.1, *)
actor Counter {
  private var value = 0
  private let scratchBuffer: UnsafeMutableBufferPointer<Int>

  init(maxCount: Int) {
    scratchBuffer = .allocate(capacity: maxCount)
    scratchBuffer.initialize(repeating: 0)
  }

  func next() -> Int {
    let current = value

    // Make sure we haven't produced this value before
    assert(scratchBuffer[current] == 0)
    scratchBuffer[current] = 1

    value = value + 1
    return current
  }

  deinit {
      for i in 0..<value {
          assert(scratchBuffer[i] == 1)
      }
  }
}

var counters: [Counter] = []
var iterations = 0

// Produce a random priority.
nonisolated var randomPriority: TaskPriority? {
  let priorities: [TaskPriority?] = [ .background, .low, .medium, .high, nil ]
  return priorities.randomElement()!
}

@available(SwiftStdlib 5.1, *)
func worker_async(workerIndex: Int) async {
  for _ in 0..<iterations {
    let counterIndex = Int.random(in: 0 ..< counters.count)
    let counter = counters[counterIndex]
    let nextValue = await counter.next()
    print("Worker \(workerIndex) calling counter \(counterIndex) produced \(nextValue)")
  }
}

func worker(context: UnsafeMutableRawPointer) -> UnsafeMutableRawPointer? {
  Task.runInline {
    await worker_async(workerIndex: Int(bitPattern: context))
    return
  }
  return nil
}

@available(SwiftStdlib 5.1, *)
func runTest(numCounters: Int, numWorkers: Int) {
  // Create counter actors.
  for _ in 0..<numCounters {
    counters.append(Counter(maxCount: numWorkers * iterations))
  }

  var workers: [pthread_t] = []
  // Create a bunch of worker threads.
  for workerIndex in 0..<numWorkers {
    var thread : pthread_t? = nil
    guard pthread_create(&thread, nil, worker, UnsafeMutableRawPointer(bitPattern: workerIndex)) == 0 else {
      fatalError("pthread_create failed")
    }
    workers.append(thread!)
  }

  // Wait until all of the workers have finished.
  for worker in workers {
    guard pthread_join(worker, nil) == 0 else {
      fatalError("pthread_join failed")
    }
  }

  print("DONE!")
}

@available(SwiftStdlib 5.1, *)
@main struct Main {
  static func main() {
    let numCounters = 10
    iterations = 1000
    let workers = 100
    print("counters: \(counters), workers: \(workers), iterations: \(iterations)")
    runTest(numCounters: numCounters, numWorkers: workers)
  }
}
