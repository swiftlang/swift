// RUN: %target-run-simple-swift( -target %target-swift-5.1-abi-triple -parse-as-library) | %FileCheck %s
// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: freestanding

import _Concurrency

@available(SwiftStdlib 5.7, *)
@main struct Main {
  static func main() async {
    // Sleeping for a duration whose deadline exceeds Int64 seconds must not
    // trap; it should simply sleep until cancelled.
    let task = Task {
      do {
        try await Task.sleep(for: .seconds(Int64.max))
        print("woke")
      } catch {
        print("cancelled")
      }
    }
    try! await Task.sleep(for: .milliseconds(100))
    task.cancel()
    await task.value
    // CHECK: cancelled
  }
}
