// RUN: %target-run-simple-swift(-parse-as-library -Xfrontend -disable-availability-checking) | %FileCheck %s
// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime

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
