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

  @oneway func log(_ value: Int) {
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

// Magic literals and '#isolation' in the arguments of a 'send', explicit or as
// caller-side default arguments, have the same values as in an 'await' of the
// same call: they are evaluated in the caller, not in the closure which the
// 'send' is lowered to. Each 'await' and 'send' pair is on one line, so that
// '#line' is the same too
actor Recorder {
  var values: [String: String] = [:]

  func explicit(_ label: String, _ line: Int, _ function: String,
                _ fileID: String) {
    values[label] = "line=\(line) function=\(function) fileID=\(fileID)"
  }

  func defaults(_ label: String, line: Int = #line,
                function: String = #function, fileID: String = #fileID) {
    values[label] = "line=\(line) function=\(function) fileID=\(fileID)"
  }

  func isolation(_ label: String, _ isolation: (any Actor)?) {
    values[label] = describe(isolation)
  }

  func isolationDefault(_ label: String,
                        isolation: (any Actor)? = #isolation) {
    values[label] = describe(isolation)
  }

  // An async callee is lowered to a task rather than enqueued directly
  func asyncIsolation(_ label: String, _ isolation: (any Actor)?) async {
    values[label] = describe(isolation)
  }

  func asyncIsolationDefault(_ label: String,
                             isolation: (any Actor)? = #isolation) async {
    values[label] = describe(isolation)
  }

  func store(_ label: String, _ value: String) { values[label] = value }

  func drain() -> [String: String] { values }
}

// A function isolated to its 'isolated' parameter, which defaults to the
// caller's isolation, runs on the caller's actor for a 'send' as well
func onCallerIsolation(_ r: Recorder, _ label: String,
                       isolation: isolated (any Actor)? = #isolation) async {
  await r.store(label, describe(isolation))
}

actor Caller {
  func run(_ r: Recorder) async {
    await r.explicit("await", #line, #function, #fileID); send r.explicit("send", #line, #function, #fileID)
    await r.defaults("await defaults"); send r.defaults("send defaults")
    await r.isolation("await isolation", #isolation); send r.isolation("send isolation", #isolation)
    await r.isolationDefault("await isolationDefault"); send r.isolationDefault("send isolationDefault")
    await r.asyncIsolation("await asyncIsolation", #isolation); send r.asyncIsolation("send asyncIsolation", #isolation)
    await r.asyncIsolationDefault("await asyncIsolationDefault"); send r.asyncIsolationDefault("send asyncIsolationDefault")
    await onCallerIsolation(r, "await isolatedParameter"); send onCallerIsolation(r, "send isolatedParameter")

    var values = await r.drain()
    var attempts = 0
    while values.count < 14 && attempts < 10_000 {
      await Task.yield()
      values = await r.drain()
      attempts += 1
    }
    for kind in ["", " defaults", " isolation", " isolationDefault",
                 " asyncIsolation", " asyncIsolationDefault",
                 " isolatedParameter"] {
      let awaited = values["await" + kind] ?? "missing"
      let sent = values["send" + kind] ?? "missing"
      print("send\(kind): \(sent), same as await: \(sent == awaited)")
    }
  }
}

func describe(_ isolation: (any Actor)?) -> String {
  switch isolation {
  case nil: return "nil"
  case is Caller: return "Caller"
  case is Recorder: return "Recorder"
  case .some: return "other"
  }
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

    await Caller().run(Recorder())
    // CHECK: send: line={{[0-9]+}} function=run(_:) fileID=main/send_evaluation_timing.swift, same as await: true
    // CHECK-NEXT: send defaults: line={{[0-9]+}} function=run(_:) fileID=main/send_evaluation_timing.swift, same as await: true
    // CHECK-NEXT: send isolation: Caller, same as await: true
    // CHECK-NEXT: send isolationDefault: Caller, same as await: true
    // CHECK-NEXT: send asyncIsolation: Caller, same as await: true
    // CHECK-NEXT: send asyncIsolationDefault: Caller, same as await: true
    // CHECK-NEXT: send isolatedParameter: Caller, same as await: true
  }
}
