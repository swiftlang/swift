// RUN: %target-run-simple-swift(-parse-as-library -target %target-swift-6.2-abi-triple -enable-experimental-feature SendCalls) | %FileCheck %s

// REQUIRES: concurrency
// REQUIRES: executable_test
// REQUIRES: concurrency_runtime
// REQUIRES: swift_feature_SendCalls

// UNSUPPORTED: back_deployment_runtime

// 'send' evaluates the receiver and the arguments at the call site, in
// program order, and sends the resulting values to the new task, rather than
// evaluating them later in the task

// The caller runs on the main actor, and the operation closure inherits that
// isolation, so it cannot run before the caller suspends. That would keep a
// later evaluation in the task deterministic, rather than flaky

actor Logger {
  var logged: [Int] = []

  func log(_ value: Int) {
    logged.append(value)
  }

  func drain() -> [Int] { logged }
}

func receiver(_ logger: Logger) -> Logger {
  print("evaluated receiver")
  return logger
}

func argument(_ value: Int) -> Int {
  print("evaluated argument \(value)")
  return value
}

@main struct Main {
  static func main() async {
    let logger = Logger()

    // The receiver and the arguments are evaluated before the next statement
    print("before send")
    send receiver(logger).log(argument(1))
    print("after send")
    // CHECK: before send
    // CHECK-NEXT: evaluated receiver
    // CHECK-NEXT: evaluated argument 1
    // CHECK-NEXT: after send

    // A captured variable is read at the call site, so the call does not
    // observe a later write
    var counter = 10
    send logger.log(counter)
    counter += 1

    var logged = await logger.drain()
    var attempts = 0
    while logged.count < 2 && attempts < 10_000 {
      await Task.yield()
      logged = await logger.drain()
      attempts += 1
    }
    print("logged \(logged.sorted()), counter \(counter)")
    // CHECK: logged [1, 10], counter 11
  }
}
