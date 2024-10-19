// RUN: %target-run-simple-swift( -target %target-swift-5.1-abi-triple %import-libdispatch -parse-as-library) | %FileCheck %s --dump-input=always

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: libdispatch

// rdar://76038845
// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime

import Dispatch

@available(SwiftStdlib 5.1, *)
func test_skipCallingNext() async {
  let numbers = [1, 1]

  let result = await withTaskGroup(of: Int.self) { (group) async -> Int in
    for n in numbers {
      print("group.spawn { \(n) }")
      group.spawn { () async -> Int in
        await Task.sleep(1_000_000_000)
        let c = Task.isCancelled
        print("  inside group.spawn { \(n) } (canceled: \(c))")
        return n
      }
    }

    // return immediately; the group should wait on the tasks anyway
    let c = Task.isCancelled
    print("return immediately 0 (canceled: \(c))")
    return 0
  }

  // CHECK: group.spawn { 1 }
  // CHECK: group.spawn { 1 }
  // CHECK: return immediately 0 (canceled: false)

  // CHECK: inside group.spawn { 1 } (canceled: false)
  // CHECK: inside group.spawn { 1 } (canceled: false)

  // CHECK: result: 0
  print("result: \(result)")
  assert(result == 0)
}

@available(SwiftStdlib 5.1, *)
@main struct Main {
  static func main() async {
    await test_skipCallingNext()
  }
}

