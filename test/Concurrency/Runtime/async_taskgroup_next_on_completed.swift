// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-concurrency %import-libdispatch -parse-as-library) | %FileCheck %s --dump-input=always

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: libdispatch

// rdar://76038845
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

import Dispatch

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
func test_sum_nextOnCompleted() async {
  let numbers = [1, 2, 3, 4, 5]
  let expected = 15 // FIXME: numbers.reduce(0, +) this hangs?

  let sum = try! await withTaskGroup(of: Int.self) {
    (group) async -> Int in
    for n in numbers {
      group.spawn {
        () async -> Int in
        print("  complete group.spawn { \(n) }")
        return n
      }
    }

    // We specifically want to await on completed child tasks in this test,
    // so give them some time to complete before we hit group.next()
    await Task.sleep(2_000_000_000)

    var sum = 0
    do {
      while let r = try await group.next() {
        print("next: \(r)")
        sum += r
        print("sum: \(sum)")
      }
    } catch {
      print("ERROR: \(error)")
    }

    assert(group.isEmpty, "Group must be empty after we consumed all tasks")

    print("task group returning: \(sum)")
    return sum
  }

  // The completions may arrive in any order, we make no strong guarantees about it:
  // CON: CHECK-DAG: complete group.spawn { [[N1:[0-9]+]] }
  // CON: CHECK-DAG: complete group.spawn { [[N2:[0-9]+]] }
  // CON: CHECK-DAG: complete group.spawn { [[N3:[0-9]+]] }
  // CON: CHECK-DAG: complete group.spawn { [[N4:[0-9]+]] }
  // CON: CHECK-DAG: complete group.spawn { [[N5:[0-9]+]] }
  // CON: CHECK-DAG: next: [[N1]]
  // CON: CHECK-DAG: next: [[N2]]
  // CON: CHECK-DAG: next: [[N3]]
  // CON: CHECK-DAG: next: [[N4]]
  // CON: CHECK-DAG: next: [[N5]]

  // CHECK: sum: 15
  //
  // CHECK: task group returning: 15

  // CHECK: result: 15
  print("result: \(sum)")
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
@main struct Main {
  static func main() async {
    await test_sum_nextOnCompleted()
  }
}
