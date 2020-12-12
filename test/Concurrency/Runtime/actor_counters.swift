// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-concurrency %import-libdispatch)

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: libdispatch

#if canImport(Darwin)
import Darwin
#elseif canImport(Glibc)
import Glibc
#endif

actor class Counter {
  private var value = 0
  private let scratchBuffer: UnsafeMutableBufferPointer<Int>

  init(maxCount: Int) {
    scratchBuffer = .allocate(capacity: maxCount)
  }

  func next() -> Int {
    let current = value

    // Make sure we haven't produced this value before
    assert(scratchBuffer[current] == 0)
    scratchBuffer[current] = 1

    value = value + 1
    return current
  }
}


func worker(
  identity: Int, counters: [Counter], numIterations: Int,
  scratchBuffer: UnsafeMutableBufferPointer<Int>
) async {
  for i in 0..<numIterations {
    let counterIndex = Int.random(in: 0 ..< counters.count)
    let counter = counters[counterIndex]
    let nextValue = await counter.next()
    print("Worker \(identity) calling counter \(counterIndex) produced \(nextValue)")
  }
}

func runTest(numCounters: Int, numWorkers: Int, numIterations: Int) async {
  let scratchBuffer = UnsafeMutableBufferPointer<Int>.allocate(
    capacity: numCounters * numWorkers * numIterations
  )

  // Create counter actors.
  var counters: [Counter] = []
  for i in 0..<numCounters {
    counters.append(Counter(maxCount: numWorkers * numIterations))
  }

  // Create a bunch of worker threads.
  var workers: [Task.Handle<Void>] = []
  for i in 0..<numWorkers {
    workers.append(
      Task.runDetached {
        usleep(UInt32.random(in: 0..<100) * 1000)
        await worker(
          identity: i, counters: counters, numIterations: numIterations,
          scratchBuffer: scratchBuffer
        )
      }
    )
  }

  // Wait until all of the workers have finished.
  for worker in workers {
    await try! worker.get()
  }

  // Clear out the scratch buffer.
  scratchBuffer.deallocate()
  print("DONE!")
}

runAsyncAndBlock {
  await runTest(numCounters: 10, numWorkers: 100, numIterations: 1000)
}
