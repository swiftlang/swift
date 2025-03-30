// RUN: %target-swift-frontend -swift-version 6 -emit-sil -o /dev/null -verify -verify-additional-prefix tns- %s

// REQUIRES: concurrency

func send(_ block: sending @escaping @isolated(any) () -> Void) {}

final class SendableClass: Sendable {
  func bar() {}
}

// MARK: -

func mutable_capture_of_sendable_value() {
  // Sendable value but mutable capture
  var x = 0

  Task.detached { x = 1 } // expected-tns-error {{sending value of non-Sendable type '() async -> ()' risks causing data races}}
  // expected-tns-note @-1 {{Passing value of non-Sendable type '() async -> ()' as a 'sending' argument to static method 'detached(priority:operation:)' risks causing races in between local and caller code}}
  x = 2 // expected-tns-note {{access can happen concurrently}}
  _ = x
}

func mutable_capture_of_sendable_class() {
  var x: SendableClass? = SendableClass()
  // expected-error @+2 {{sending value of non-Sendable type '() -> Void' risks causing data races}}
  // expected-note @+1 {{Passing value of non-Sendable type '() -> Void' as a 'sending' argument to global function 'send' risks causing races in between local and caller code}}
  send {
    x?.bar()
    x = nil
  }
  x = nil // expected-note {{access can happen concurrently}}
}
