// RUN: %target-run-simple-swift( -target %target-swift-5.1-abi-triple %import-libdispatch -parse-as-library) | %FileCheck %s

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

@available(SwiftStdlib 5.1, *)
func test_taskGroup_isEmpty() async {
  print("before all")
  let result = await withTaskGroup(of: Int.self, returning: Int.self) { group in
    // CHECK: before add: isEmpty=true
    print("before add: isEmpty=\(group.isEmpty)")

    group.async {
      await Task.sleep(2_000_000_000)
      return await asyncEcho(1)
    }

    // CHECK: while add running, outside: isEmpty=false
    print("while add running, outside: isEmpty=\(group.isEmpty)")

    // CHECK: next: 1
    while let value = try! await group.next() {
      print("next: \(value)")
    }

    // CHECK: after draining tasks: isEmpty=true
    print("after draining tasks: isEmpty=\(group.isEmpty)")
    return 42
  }

  // CHECK: result: 42
  print("result: \(result)")
}

@available(SwiftStdlib 5.1, *)
@main struct Main {
  static func main() async {
    await test_taskGroup_isEmpty()
  }
}
