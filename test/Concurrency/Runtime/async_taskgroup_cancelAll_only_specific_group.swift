// RUN: %target-run-simple-swift( -target %target-swift-5.1-abi-triple %import-libdispatch -parse-as-library) | %FileCheck %s --dump-input always

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: libdispatch

// rdar://76038845
// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime

import Dispatch

@available(SwiftStdlib 5.1, *)
func asyncEcho(_ value: Int) async -> Int {
  value
}

/// Tests that only the specific group we cancelAll on is cancelled,
/// and not accidentally all tasks in all groups within the given parent task.
@available(SwiftStdlib 5.1, *)
func test_taskGroup_cancelAll_onlySpecificGroup() async {
  async let g1: Int = withTaskGroup(of: Int.self) { group in

    for i in 1...5 {
      group.spawn {
        await Task.sleep(1_000_000_000)
        let c = Task.isCancelled
        print("add: \(i) (cancelled: \(c))")
        return i
      }
    }

    var sum = 0
    while let got = try! await group.next() {
      print("next: \(got)")
      sum += got
    }

    let c = Task.isCancelled
    print("g1 task cancelled: \(c)")
    let cc = group.isCancelled
    print("g1 group cancelled: \(cc)")

    return sum
  }

  // The cancellation os g2 should have no impact on g1
  let g2: Int = try! await withTaskGroup(of: Int.self) { group in
    for i in 1...3 {
      group.spawn {
        await Task.sleep(1_000_000_000)
        let c = Task.isCancelled
        print("g1 task \(i) (cancelled: \(c))")
        return i
      }
    }

    print("cancelAll")
    group.cancelAll()

    let c = Task.isCancelled
    print("g2 task cancelled: \(c)")
    let cc = group.isCancelled
    print("g2 group cancelled: \(cc)")
    return 0
  }

  let result1 = try! await g1
  let result2 = try! await g2

  // CHECK: g2 task cancelled: false
  // CHECK: g2 group cancelled: true
  // CHECK: g1 task cancelled: false
  // CHECK: g1 group cancelled: false

  print("g1: \(result1)") // CHECK: g1: 15
  print("g2: \(result2)") // CHECK: g2: 0
}



@available(SwiftStdlib 5.1, *)
@main struct Main {
  static func main() async {
    await test_taskGroup_cancelAll_onlySpecificGroup()
  }
}
