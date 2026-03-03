// RUN: %target-typecheck-verify-swift -swift-version 6

// REQUIRES: VENDOR=apple
// REQUIRES: concurrency

import OSLogTestHelper

class Log {
  @_semantics("oslog.requires_constant_arguments")
  func debug(_: OSLogMessage) {}
}

@_semantics("constant_evaluable")
@_transparent
public func __outputFormatter(_ key: String, fallback: OSLogMessage) -> OSLogMessage {
    fallback
}

@MainActor
@_semantics("constant_evaluable")
@_transparent
public func __isolatedOutputFormatter(_ key: String, fallback: OSLogMessage) -> OSLogMessage {
    fallback
}

func test(log: Log) {
  log.debug(__outputFormatter("1", fallback: "msg")) // Ok
}

@MainActor
func isolatedTest(log: Log) {
  log.debug(__isolatedOutputFormatter("1", fallback: "msg")) // Ok
}
