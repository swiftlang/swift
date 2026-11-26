// RUN: %target-run-simple-swift( -target %target-swift-5.1-abi-triple -parse-as-library) | %FileCheck %s
// RUN: %target-run-simple-swift( -target %target-swift-5.1-abi-triple -parse-as-library -O) | %FileCheck %s

// REQUIRES: concurrency
// REQUIRES: executable_test
// REQUIRES: concurrency_runtime

// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: freestanding

// Consuming `#isolation` in a `nonisolated(nonsending)` function must not
// release the caller's actor (https://github.com/swiftlang/swift/issues/92799).

actor Worker {
  deinit { print("Worker.deinit") }

  func run() async -> String { await describeIsolation() }
  func wrap() async -> Bool { await wrapIsolation() === self }
}

nonisolated(nonsending) func describeIsolation() async -> String {
  String(describing: #isolation)
}

nonisolated(nonsending) func wrapIsolation() async -> (any Actor)? {
  let wrapped: (any Actor)?? = .some(#isolation)
  return wrapped!
}

func test() async {
  let worker = Worker()
  for i in 1...3 {
    print("\(i):", await worker.run(), await worker.wrap())
  }
  print("end of test()")
}

@main struct Main {
  static func main() async {
    // CHECK-NOT: Worker.deinit
    // CHECK: 1: Optional({{.*}}Worker) true
    // CHECK-NOT: Worker.deinit
    // CHECK: 2: Optional({{.*}}Worker) true
    // CHECK-NOT: Worker.deinit
    // CHECK: 3: Optional({{.*}}Worker) true
    // CHECK-NOT: Worker.deinit
    // CHECK: end of test()
    // CHECK-NEXT: Worker.deinit
    // CHECK-NEXT: done
    await test()
    print("done")
  }
}
