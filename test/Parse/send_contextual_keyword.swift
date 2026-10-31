// RUN: %if swift_feature_SendCalls %{ %target-typecheck-verify-swift -target %target-swift-6.2-abi-triple -enable-experimental-feature SendCalls %}
// RUN: %target-typecheck-verify-swift -target %target-swift-6.2-abi-triple -verify-additional-prefix nofeature-

// REQUIRES: concurrency

// Ensure we parse the 'send' keyword contextually, and that calling methods called 'send' still works.
// Also verify compiler without the experimental feature keeps its unchanged behavior.

func send(_ x: Int) {} // expected-nofeature-note{{found this candidate}}
func send(_ body: () -> Void) {} // expected-nofeature-note{{found this candidate}}

struct Channel {
  func send(_ x: Int) {}
}

actor Logger {
  func log() {} // expected-nofeature-note{{calls to instance method 'log()' from outside of its actor context are implicitly asynchronous}}
}

func callsOfFunctionsNamedSend(_ channel: Channel) {
  send(1)
  channel.send(2)
  send { }
  let closure: (Int) -> Int = { send in send }
  _ = closure(3)
}

func variableNamedSend() {
  var send = 1
  send = 2
  print(send)
  let value = send
  print(value)
}

func sendKeyword(_ logger: Logger) {
  // expected-nofeature-error@+3:3{{ambiguous use of 'send'}}
  // expected-nofeature-error@+2:7{{consecutive statements on a line must be separated by ';'}}
  // expected-nofeature-error@+1:15{{call to actor-isolated instance method 'log()' in a synchronous nonisolated context}}
  send logger.log()
}
