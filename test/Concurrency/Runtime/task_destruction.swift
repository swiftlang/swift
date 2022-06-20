// RUN: %target-run-simple-swift(-parse-as-library -Xfrontend -disable-availability-checking) | %FileCheck %s
// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime

// UNSUPPORTED: back_deployment_runtime

class C {}

@_silgen_name("exit")
func exit(_ code : UInt32) -> Never

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
      print("Fail!")
      exit(1)
    }
    
    while weakRef != nil {
      await Task.yield()
    }
    // CHECK: Success
    print("Success!")
  }
}
