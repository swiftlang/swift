// RUN: %target-run-simple-swift(-parse-as-library -target %target-swift-6.2-abi-triple -enable-experimental-feature SendCalls) | %FileCheck %s

// REQUIRES: concurrency
// REQUIRES: executable_test
// REQUIRES: concurrency_runtime
// REQUIRES: swift_feature_SendCalls

// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: use_os_stdlib

// A 'send' call consumes its noncopyable arguments. A 'consuming' parameter
// takes the value, a 'borrowing' parameter borrows it for the call on the
// actor, after which the value is destroyed

struct Handle: ~Copyable {
  let id: Int
  init(_ id: Int) { self.id = id }
  deinit { print("deinit \(id)") }
}

actor Logger {
  func retarget(to handle: consuming Handle) {
    print("retarget \(handle.id)")
  }
  func analyze(_ handle: borrowing Handle) {
    print("analyze \(handle.id)")
  }
  func retargetAsync(to handle: consuming Handle) async {
    print("retargetAsync \(handle.id)")
  }
  func analyzeAsync(_ handle: borrowing Handle) async {
    print("analyzeAsync \(handle.id)")
  }
  func flush() {}
}

func sendAll(_ logger: Logger) {
  send logger.retarget(to: Handle(1))
  let two = Handle(2)
  send logger.analyze(two)
  send logger.retargetAsync(to: Handle(3))
  let four = Handle(4)
  send logger.analyzeAsync(four)
}

@main struct Main {
  static func main() async {
    let logger = Logger()
    sendAll(logger)
    await logger.flush()
    // Let the 'async' callees, which run in tasks, finish
    try? await Task.sleep(for: .milliseconds(200))
    print("done")
  }
}

// The calls are enqueued on the actor in program order, and none of the
// callees suspends. A borrowed argument is destroyed after its call returned
// CHECK: retarget 1
// CHECK: deinit 1
// CHECK: analyze 2
// CHECK: deinit 2
// CHECK: retargetAsync 3
// CHECK: deinit 3
// CHECK: analyzeAsync 4
// CHECK: deinit 4
// CHECK: done
