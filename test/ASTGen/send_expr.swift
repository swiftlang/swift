// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend-dump-parse \
// RUN:   -enable-experimental-feature SendCalls \
// RUN:   -enable-experimental-feature ParserASTGen \
// RUN:   | %sanitize-address > %t/astgen.ast

// RUN: %target-swift-frontend-dump-parse \
// RUN:   -enable-experimental-feature SendCalls \
// RUN:   | %sanitize-address > %t/cpp-parser.ast

// RUN: %diff -u %t/astgen.ast %t/cpp-parser.ast

// REQUIRES: concurrency
// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_ParserASTGen
// REQUIRES: swift_feature_SendCalls

// rdar://116686158
// UNSUPPORTED: asan

// ASTGen generates a 'send' call expression like the C++ parser, and 'send'
// stays usable as an identifier where it does not start a call

actor Worker {
  func tick() {}
  func log(_ line: String) {}

  func sendToSelf() {
    send self.tick()
  }
}

@MainActor func ping() {}

func sendCalls(_ worker: Worker, _ line: String) {
  send worker.tick()
  send worker.log(line)
  send ping()
}

func sendAsIdentifier() {
  let send = 1
  _ = send
}

func send(_ value: Int) {}

func callFunctionNamedSend() {
  send(1)
}
