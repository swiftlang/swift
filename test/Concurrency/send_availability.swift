// RUN: %target-typecheck-verify-swift -target %target-swift-5.9-abi-triple -enable-experimental-feature SendCalls

// REQUIRES: OS=macosx
// REQUIRES: concurrency
// REQUIRES: swift_feature_SendCalls

// The task of a 'send' consumes its operation closure by calling it, which
// requires the Swift 6.1 runtime

actor A {
  func f() {}
}

func tooOld(_ a: A) { // expected-note{{add '@available' attribute to enclosing global function}}
  send a.f() // expected-error{{'send' is only available in macOS 15.4.0 or newer}}
  // expected-note@-1{{add 'if #available' version check}}
}

@available(SwiftStdlib 6.1, *)
func annotated(_ a: A) {
  send a.f()
}

func checked(_ a: A) {
  if #available(SwiftStdlib 6.1, *) {
    send a.f()
  }
}
