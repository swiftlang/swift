// RUN: %target-run-simple-swift(-parse-as-library) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime
// REQUIRES: OS=wasip1
// REQUIRES: wasi_threads

// The wasm32-unknown-wasip1-threads global executor: work fans out across a
// pool of wasi threads (child tasks observe more than one thread), `@MainActor`
// continuations resumed from the pool land back on the main thread, and
// delayed jobs go through the pool's timer list.

import WASILibc

@inline(never)
func currentThreadID() -> UInt {
  return UInt(bitPattern: Int(bitPattern: pthread_self()))
}

nonisolated func spin(_ iterations: Int) -> Int {
  var x = 0
  for i in 0..<iterations { x = (x &+ i) & 0xffff }
  return x
}

@MainActor var mainThreadID = currentThreadID()

@main struct Main {
  static func main() async {
    let main = await mainThreadID

    // Enough CPU-bound children to keep several workers busy at once.
    let threads = await withTaskGroup(of: (UInt, Int).self) { group in
      for _ in 0..<32 {
        group.addTask { (currentThreadID(), spin(200_000)) }
      }
      var seen = Set<UInt>()
      for await (thread, _) in group { seen.insert(thread) }
      return seen
    }
    // CHECK: ran on the main thread: false
    print("ran on the main thread: \(threads.contains(main))")
    // CHECK: parallel: true
    print("parallel: \(threads.count > 1)")

    // A continuation resumed from a pool worker returns to the main thread.
    let resumedOn: UInt = await withCheckedContinuation { continuation in
      Task.detached {
        _ = spin(1000)
        continuation.resume(returning: currentThreadID())
      }
    }
    // CHECK: resumed from a worker: true
    print("resumed from a worker: \(resumedOn != main)")
    // CHECK: back on main: true
    print("back on main: \(currentThreadID() == main)")

    // Delayed scheduling goes through the pool's timer list.
    let clock = ContinuousClock()
    let start = clock.now
    try? await Task.sleep(for: .milliseconds(50))
    // CHECK: slept: true
    print("slept: \(clock.now - start >= .milliseconds(45))")
  }
}
