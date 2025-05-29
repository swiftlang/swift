// RUN: %target-run-simple-swift( -target %target-swift-5.1-abi-triple -parse-as-library) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: reflection

// rdar://76038845
// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime

class X {
  init() {
    print("X: init")
  }
  deinit {
    print("X: deinit")
  }
}

struct Boom: Error {}

@available(SwiftStdlib 5.1, *)
func test_detach() async {
  let x = X()
  let h = detach {
    print("inside: \(x)")
  }
  await h.get()
  // CHECK: X: init
  // CHECK: inside: main.X
  // CHECK: X: deinit
}

@available(SwiftStdlib 5.1, *)
func test_detach_throw() async {
  let x = X()
  let h = detach {
    print("inside: \(x)")
    throw Boom()
  }
  do {
    try await h.get()
  } catch {
    print("error: \(error)")
  }
  // CHECK: X: init
  // CHECK: inside: main.X
  // CHECK: error: Boom()
}


@available(SwiftStdlib 5.1, *)
@main struct Main {
  static func main() async {
    await test_detach()
    await test_detach_throw()
  }
}
