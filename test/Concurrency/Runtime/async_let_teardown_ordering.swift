// https://github.com/swiftlang/swift/issues/81771
// Verify that multiple async let bindings produce correct LIFO teardown
// ordering (finish/dealloc/finish/dealloc), not finish/finish/dealloc/dealloc.
// The incorrect ordering causes a runtime crash ("freed pointer was not the
// last allocation") when targeting pre-macOS 13 / pre-iOS 16 or when the
// result type is large.

// RUN: %target-run-simple-swift(-swift-version 6 -parse-as-library) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime

struct MyData {
  var p1, p2, p3, p4, p5, p6: Int
  var p7: Int
}

struct MyWrapper: ~Copyable {
  var payload: MyData?
}

func async_let_crash() async {
  async let one: Never? = Never?.none
  async let two: Never? = Never?.none
  if let _ = await one {}
  _ = await two
  _ = try? MyWrapper()
}

@main
enum App {
  static func main() async {
    await async_let_crash()
    // CHECK: ok
    print("ok")
  }
}
