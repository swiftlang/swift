// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-concurrency) | %FileCheck %s --dump-input=always
// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: OS=macosx
// REQUIRES: CPU=x86_64

import Dispatch
import Darwin

func completeSlowly(n: Int) async -> Int {
  sleep(UInt32(n + 1))
  print("  complete group.add { \(n) }")
  return n
}

/// Tasks complete AFTER they are next() polled.
func test_sum_nextOnPending() async {
  let numbers = [1, 2, 3]
  let expected = numbers.reduce(0, +)

  let sum = await try! Task.withGroup(resultType: Int.self) { (group) async -> Int in
    for n in numbers {
      await group.add {
        let res = await completeSlowly(n: n)
        return res
      }
    }

    var sum = 0
    print("before group.next(), sum: \(sum)")
    while let n = await try! group.next() {
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
  //
  // CHECK: complete group.add { 1 }
  // CHECK: next: 1
  // CHECK: complete group.add { 2 }
  // CHECK: next: 2
  // CHECK: complete group.add { 3 }
  // CHECK: next: 3

  // CHECK: task group returning: 6

  // CHECK: result: 6
  print("result: \(sum)")
  assert(sum == expected, "Expected: \(expected), got: \(sum)")
}

runAsyncAndBlock(test_sum_nextOnPending)
