// RUN: %target-run-simple-swift(-parse-as-library -target %target-swift-5.1-abi-triple) | %FileCheck %s
// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime
// UNSUPPORTED: freestanding

// UNSUPPORTED: back_deployment_runtime

class C {}

@main
enum Main {
  static func main() async {
    weak var weakRef: C?
    do {
      let c = C()
      let t = Task.detached { return c }
      weakRef = c
    }
    Task.detached {
      try await Task.sleep(nanoseconds: 10_000_000_000)
      fatalError("Fail!")
    }
    
    while weakRef != nil {
      await Task.yield()
    }
    // CHECK: Success
    print("Success!")
  }
}
