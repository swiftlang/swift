// RUN: %target-typecheck-verify-swift -swift-version 6 -target %target-swift-6.2-abi-triple -enable-experimental-feature SendCalls

// REQUIRES: concurrency
// REQUIRES: swift_feature_SendCalls

// Only an 'await' of a '@oneway' function which crosses into another
// isolation domain is diagnosed. A call from within the function's own
// isolation domain is an ordinary call

actor Logger {
  @oneway func log(_ value: Int) {} // expected-note{{calls to instance method 'log' from outside of its actor context are implicitly asynchronous}}
  @oneway func logAsync(_ value: Int) async {}

  func plain() {
    log(1)
    self.log(2)
  }

  func awaitedAsyncInPlace() async {
    await logAsync(3)
    await self.logAsync(4)
  }

  func sendToSelf() {
    send log(5)
  }

  func inTask() {
    Task {
      await self.logAsync(6)
    }
  }

  func toOther(_ other: Logger) async {
    await other.log(7) // expected-warning{{'@oneway' instance method 'log' is not intended to be awaited on, use 'send' instead}}{{5-10=send}}
    send other.log(8)
  }
}

@MainActor @oneway func mainLog(_ value: Int) {}

@MainActor func onMain() async {
  mainLog(9)
}

func outside(_ logger: Logger) async {
  await logger.log(10) // expected-warning{{'@oneway' instance method 'log' is not intended to be awaited on, use 'send' instead}}{{3-8=send}}
  await logger.logAsync(11) // expected-warning{{'@oneway' instance method 'logAsync' is not intended to be awaited on, use 'send' instead}}{{3-8=send}}
  await mainLog(12) // expected-warning{{'@oneway' global function 'mainLog' is not intended to be awaited on, use 'send' instead}}{{3-8=send}}
  send logger.log(13)
}

func outsideSync(_ logger: Logger) {
  logger.log(14) // expected-error{{call to actor-isolated instance method 'log' in a synchronous nonisolated context}}
  send logger.log(15)
}
