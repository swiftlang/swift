// RUN: %target-run-simple-swift( -Xfrontend -disable-availability-checking %import-libdispatch -parse-as-library) | %FileCheck %s
// Also run with -O, which is where the tuple-return-across-suspension miscompile appeared.
// RUN: %target-run-simple-swift( -Xfrontend -disable-availability-checking %import-libdispatch -parse-as-library -O) | %FileCheck %s
// RUN: %target-run-simple-swift( -Xfrontend -disable-availability-checking %import-libdispatch -parse-as-library -swift-version 5 -strict-concurrency=complete -enable-upcoming-feature NonisolatedNonsendingByDefault) | %FileCheck %s
// REQUIRES: swift_feature_NonisolatedNonsendingByDefault

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: libdispatch

// rdar://113915243 - flaky test on watchos
// UNSUPPORTED: OS=watchos

// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime

import Dispatch

func completeSlowly(n: Int) async -> Int {
  try? await Task.sleep(for: .milliseconds(n * 300))
  return n
}

/// Tasks complete AFTER they are next() polled.
func test_sum_nextOnPending() async {
  let numbers = [1, 2, 3]
  let expected = 6

  let sum = try! await withTaskGroup(of: Int.self) { (group) async -> Int in
    for n in numbers {
      group.addTask {
        await completeSlowly(n: n)
      }
    }

    var sum = 0
    print("before group.next(), sum: \(sum)")
    while let n = try! await group.next() {
      assert(numbers.contains(n), "Unexpected value: \(n)! Expected any of \(numbers)")
      print("next: \(n)")
      sum += n
      print("before group.next(), sum: \(sum)")
    }

    print("task group returning: \(sum)")
    return sum
  }

  // The completions are set apart by n seconds, so we expect them to arrive
  // in the order as the numbers (and delays) would suggest:

  // CHECK: task group returning: 6

  // CHECK: result: 6
  print("result: \(sum)")
  assert(sum == expected, "Expected: \(expected), got: \(sum)")
}

enum ReproError: Error { case boom }

func echoIndex(_ index: Int) async throws -> Int {
  await Task.yield()
  if index < 0 { throw ReproError.boom }
  return index
}

func test_tuple_result_across_suspend() async {
  var results: [Int: Int] = [:]
  await withTaskGroup(of: (Int, Result<Int, any Error>).self) { group in
    for index in 0..<6 {
      group.addTask {
        // Note that the index is readily available, and would previously be attempted to
        // be written before suspending for the echoIndex call. This would result in corrupting
        // the tuple value when the write completes after the suspension.
        do {
          return (index, .success(try await echoIndex(index)))
        } catch {
          return (index, .failure(error))
        }
      }
    }
    while let (index, result) = await group.next() {
      if case .success(let value) = result {
        results[index] = value
      }
    }
  }

  // Check values returned through tuples were not corrupted:
  // CHECK: tuple-result keys=[0, 1, 2, 3, 4, 5]
  print("tuple-result keys=\(results.keys.sorted())")
}

@main struct Main {
  static func main() async {
    await test_sum_nextOnPending()
    await test_tuple_result_across_suspend()
  }
}
