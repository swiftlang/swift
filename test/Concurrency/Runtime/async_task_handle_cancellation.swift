// RUN: %target-run-simple-swift( -target %target-swift-5.1-abi-triple %import-libdispatch -parse-as-library) | %FileCheck %s --dump-input=always

// REQUIRES: executable_test
// REQUIRES: concurrency
// UNSUPPORTED: freestanding

// rdar://76038845
// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime

@available(SwiftStdlib 5.1, *)
@main struct Main {
  static func main() async {
    let task = Task.detached {
      while (!Task.isCancelled) { // no need for await here, yay
        print("waiting")
      }

      print("inside: Task.isCancelled = \(Task.isCancelled)")
    }

    task.cancel()

    await task.value
    print("outside: task.isCancelled = \(task.isCancelled)")
    print("outside: Task.isCancelled = \(Task.isCancelled)")

    // CHECK-DAG: inside: Task.isCancelled = true
    // CHECK-DAG: outside: task.isCancelled = true
    // CHECK-DAG: outside: Task.isCancelled = false
  }
}
