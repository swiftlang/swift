// RUN: %target-swiftc_driver %s -Xfrontend -enable-experimental-concurrency -parse-as-library %import-libdispatch -target %sanitizers-target-triple -g -sanitize=thread -o %t
// RUN: %target-codesign %t
// RUN: env %env-TSAN_OPTIONS="abort_on_error=0" not %target-run %t 2>&1 | %swift-demangle --simplified | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: libdispatch
// REQUIRES: tsan_runtime

// rdar://76038845
// UNSUPPORTED: use_os_stdlib

// rdar://75365575 (Failing to start atos external symbolizer)
// UNSUPPORTED: OS=watchos

// REQUIRES: rdar76542113

var globalCounterValue = 0

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
actor Counter {
  func next() -> Int {
    let current = globalCounterValue
    globalCounterValue += 1
    return current
  }
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
func worker(identity: Int, counters: [Counter], numIterations: Int) async {
  for _ in 0..<numIterations {
    let counterIndex = Int.random(in: 0 ..< counters.count)
    let counter = counters[counterIndex]
    let nextValue = await counter.next()
    print("Worker \(identity) calling counter \(counterIndex) produced \(nextValue)")
  }
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
func runTest(numCounters: Int, numWorkers: Int, numIterations: Int) async {
  // Create counter actors.
  var counters: [Counter] = []
  for _ in 0..<numCounters {
    counters.append(Counter())
  }

  // Create a bunch of worker threads.
  var workers: [Task.Handle<Void, Error>] = []
  for i in 0..<numWorkers {
    workers.append(
      detach { [counters] in
        await worker(identity: i, counters: counters, numIterations: numIterations)
      }
    )
  }

  // Wait until all of the workers have finished.
  for worker in workers {
    try! await worker.get()
  }

  print("DONE!")
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
@main struct Main {
  static func main() async {
    // Useful for debugging: specify counter/worker/iteration counts
    let args = CommandLine.arguments
    let counters = args.count >= 2 ? Int(args[1])! : 10
    let workers = args.count >= 3 ? Int(args[2])! : 10
    let iterations = args.count >= 4 ? Int(args[3])! : 100
    print("counters: \(counters), workers: \(workers), iterations: \(iterations)")
    await runTest(numCounters: counters, numWorkers: workers, numIterations: iterations)
  }
}

// CHECK: ThreadSanitizer: {{(Swift access|data)}} race
// CHECK: Location is global 'globalCounterValue'
