// RUN: %target-run-simple-swift(-parse-as-library -target %target-swift-6.2-abi-triple -enable-experimental-feature SendCalls) | %FileCheck %s

// REQUIRES: concurrency
// REQUIRES: executable_test
// REQUIRES: concurrency_runtime
// REQUIRES: swift_feature_SendCalls

// UNSUPPORTED: back_deployment_runtime

// A 'send' of an 'async' method runs the call in a new task. The receiver
// and the arguments are still evaluated at the call site, also in an optional
// chain, which evaluates nothing after the link that fails, also through an
// existential receiver. A function value is
// read at the call site too, so the task does not observe a later write.
// See 'send_evaluation_timing.swift' for the synchronous lowering

protocol Loggable: Actor {
  func log(_ value: Int) async
  func logSync(_ value: Int)
  nonisolated var next: (any Loggable)? { get }
}

protocol LoggerFactory {
  func find() -> (any Loggable)?
}

actor Logger: Loggable {
  var logged: [Int] = []
  nonisolated let next: (any Loggable)?

  init(next: (any Loggable)? = nil) {
    self.next = next
  }

  func log(_ value: Int) async {
    logged.append(value)
  }

  func logSync(_ value: Int) {
    logged.append(value)
  }

  func drain() -> [Int] { logged }
}

struct Factory: LoggerFactory {
  let logger: (any Loggable)?

  func find() -> (any Loggable)? {
    print("evaluated factory")
    return logger
  }
}

func existentialReceiver(_ logger: (any Loggable)?) -> (any Loggable)? {
  print("evaluated existential receiver")
  return logger
}

func factory(_ logger: (any Loggable)?) -> any LoggerFactory {
  print("evaluated factory receiver")
  return Factory(logger: logger)
}

func receiver(_ logger: Logger?) -> Logger? {
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

    // A non-nil optional chain evaluates the receiver and the arguments
    // before the next statement
    print("before send")
    send receiver(logger)?.log(argument(1))
    print("after send")
    // CHECK: before send
    // CHECK-NEXT: evaluated receiver
    // CHECK-NEXT: evaluated argument 1
    // CHECK-NEXT: after send

    // A nil optional chain evaluates the receiver only
    print("before nil send")
    send receiver(nil)?.log(argument(2))
    print("after nil send")
    // CHECK-NEXT: before nil send
    // CHECK-NEXT: evaluated receiver
    // CHECK-NEXT: after nil send

    // A function value is read at the call site
    var handler: @MainActor (Int) -> Void = { value in
      print("first handler \(value)")
    }
    send handler(argument(3))
    handler = { value in print("second handler \(value)") }
    _ = handler
    // CHECK-NEXT: evaluated argument 3

    // An optional existential receiver, with an 'async' and a synchronous
    // method
    send existentialReceiver(logger)?.log(argument(4))
    send existentialReceiver(logger)?.logSync(argument(5))
    send existentialReceiver(nil)?.log(argument(-1))
    // CHECK-NEXT: evaluated existential receiver
    // CHECK-NEXT: evaluated argument 4
    // CHECK-NEXT: evaluated existential receiver
    // CHECK-NEXT: evaluated argument 5
    // CHECK-NEXT: evaluated existential receiver

    // An optional actor returned by a method of an existential receiver
    send factory(logger).find()?.log(argument(6))
    send factory(nil).find()?.log(argument(-2))
    // CHECK-NEXT: evaluated factory receiver
    // CHECK-NEXT: evaluated factory
    // CHECK-NEXT: evaluated argument 6
    // CHECK-NEXT: evaluated factory receiver
    // CHECK-NEXT: evaluated factory

    // A chain with two optional existentials, which fails at the second one
    // or not
    let chained = Logger(next: logger)
    send existentialReceiver(chained)?.next?.log(argument(7))
    send existentialReceiver(logger)?.next?.log(argument(-3))
    // CHECK-NEXT: evaluated existential receiver
    // CHECK-NEXT: evaluated argument 7
    // CHECK-NEXT: evaluated existential receiver

    var logged = await logger.drain()
    var attempts = 0
    while logged.count < 5 && attempts < 10_000 {
      await Task.yield()
      logged = await logger.drain()
      attempts += 1
    }
    print("logged \(logged.sorted())")
    // CHECK-DAG: first handler 3
    // CHECK-DAG: logged [1, 4, 5, 6, 7]
  }
}
