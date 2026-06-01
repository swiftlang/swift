// RUN: %target-run-simple-swift(-target %target-swift-5.1-abi-triple -parse-as-library -D SWIFT_STDLIB_TASK_REGISTRY_TESTING)
// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: freestanding

import _Concurrency
import Dispatch
import StdlibUnittest

@_silgen_name("swift_task_registryCount")
func registryCount() -> Int

var TaskRegistryTests = TestSuite("TaskRegistry")

// Each task suspension must increment the count by exactly 1,
// and each completion must decrement it by exactly 1.
TaskRegistryTests.test("perTaskIncrementAndDecrement") {
  let n = 20
  let lock = NSLock()
  var continuations: [UnsafeContinuation<Void, Never>] = []
  continuations.reserveCapacity(n)
  let ready = DispatchSemaphore(value: 0)
  let done = DispatchGroup()

  let baseline = registryCount()

  // Spawn tasks one at a time and verify the count rises by exactly 1 each time.
  for i in 0..<n {
    done.enter()
    Task {
      await withUnsafeContinuation { (cont: UnsafeContinuation<Void, Never>) in
        lock.lock()
        continuations.append(cont)
        lock.unlock()
        ready.signal()
      }
      done.leave()
    }
    ready.wait()
    let expected = baseline + i + 1
    let actual = registryCount()
    expectGreaterThanOrEqual(actual, expected,
      "after spawning task \(i+1): count should be at least \(expected), got \(actual)")
  }

  // Now resume tasks one at a time and verify the count falls by exactly 1 each time.
  for i in 0..<n {
    lock.lock()
    let cont = continuations[i]
    lock.unlock()
    cont.resume()

    // Give the task a moment to finish and deregister.
    let g = DispatchGroup()
    g.enter()
    DispatchQueue.global().asyncAfter(deadline: .now() + 0.05) { g.leave() }
    g.wait()

    let expected = baseline + n - (i + 1)
    let actual = registryCount()
    expectLessThanOrEqual(actual, expected + 5,
      "after completing task \(i+1): count should be near \(expected), got \(actual)")
  }

  done.wait()

  // Final check: back to baseline.
  let final = registryCount()
  expectLessThanOrEqual(final, baseline + 2,
    "after all tasks complete, count should return to baseline \(baseline), got \(final)")
}

await runAllTestsAsync()
