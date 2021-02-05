// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-concurrency -parse-as-library) | %FileCheck %s --dump-input=always
// REQUIRES: executable_test
// REQUIRES: concurrency
// XFAIL: windows
// XFAIL: linux
// XFAIL: openbsd

import Dispatch
import Darwin

func completeImmediately(n: Int) async -> Int {
  print("complete group.add { \(n) }")
  return n
}

func completeSlowly(n: Int) async -> Int {
  sleep(3)
  print("complete group.add { \(n) }")
  return n
}

/// Tasks complete AFTER they are next() polled.
func test_sum_nextOnPending() async {
  let sum = try! await Task.withGroup(resultType: Int.self) { (group) async -> Int in
    let firstHandle = await group.add {
      let res = await completeSlowly(n: 1)
      return res
    }
    let secondHandle = await group.add {
      let res = await completeImmediately(n: 2)
      return res
    }
    let thirdHandle = await group.add {
      let res = await completeSlowly(n: 3)
      return res
    }

    let first = try! await firstHandle.get()
    print("firstHandle.get(): \(first)")
    let second = try! await secondHandle.get()
    print("secondHandle.get(): \(second)")
    let third = try! await thirdHandle.get()
    print("thirdHandle.get(): \(third)")

    var sum = 0
    print("before group.next(), sum: \(sum)")
    while let n = try! await group.next() {
      assert(n <= 3, "Unexpected value: \(n)! Expected <= 3")
      print("next: \(n)")
      sum += n
      print("before group.next(), sum: \(sum)")
    }

    print("task group returning: \(sum)")
    return sum
  }

  // CHECK: firstHandle.get(): 1
  // CHECK: secondHandle.get(): 2
  // CHECK: thirdHandle.get(): 3

  // CHECK: task group returning: 6

  // CHECK: result: 6
  print("result: \(sum)")
  assert(sum == 6, "Expected \(6) but got \(sum)")
}

@main struct Main {
  static func main() async {
    await test_sum_nextOnPending()
  }
}
