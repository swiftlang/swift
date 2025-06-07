// RUN: %target-run-simple-swift( -Xfrontend -sil-verify-all -target %target-swift-5.1-abi-triple %import-libdispatch -parse-as-library)

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: libdispatch

// rdar://76038845
// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime

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

// Produce a random priority.
nonisolated var randomPriority: TaskPriority? {
  let priorities: [TaskPriority?] = [ .background, .low, .medium, .high, nil ]
  return priorities.randomElement()!
}

@available(SwiftStdlib 5.1, *)
func worker(identity: Int, counters: [Counter], numIterations: Int) async {
  for _ in 0..<numIterations {
    let counterIndex = Int.random(in: 0 ..< counters.count)
    let counter = counters[counterIndex]
    let nextValue = await counter.next()
    print("Worker \(identity) calling counter \(counterIndex) produced \(nextValue)")
  }
}

@available(SwiftStdlib 5.1, *)
func runTest(numCounters: Int, numWorkers: Int, numIterations: Int) async {
  // Create counter actors.
  var counters: [Counter] = []
  for _ in 0..<numCounters {
    counters.append(Counter(maxCount: numWorkers * numIterations))
  }

  // Create a bunch of worker threads.
  var workers: [Task<Void, Error>] = []
  for i in 0..<numWorkers {
    workers.append(
      Task.detached(priority: randomPriority) { [counters] in
        try! await Task.sleep(nanoseconds: UInt64.random(in: 0..<100) * 1_000_000)
        await worker(identity: i, counters: counters, numIterations: numIterations)
      }
    )
  }

  // Wait until all of the workers have finished.
  for worker in workers {
    try! await worker.value
  }

  print("DONE!")
}

@available(SwiftStdlib 5.1, *)
@main struct Main {
  static func main() async {
    // Useful for debugging: specify counter/worker/iteration counts
    let args = CommandLine.arguments
    let counters = args.count >= 2 ? Int(args[1])! : 10
    let workers = args.count >= 3 ? Int(args[2])! : 100
    let iterations = args.count >= 4 ? Int(args[3])! : 1000
    print("counters: \(counters), workers: \(workers), iterations: \(iterations)")
    await runTest(numCounters: counters, numWorkers: workers, numIterations: iterations)
  }
}
