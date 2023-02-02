// RUN: %target-swift-frontend -parse-stdlib -target x86_64-apple-macosx10.14 -typecheck -verify %s
// RUN: %target-swift-frontend -parse-stdlib -target x86_64-apple-macosx11 -typecheck %s
// RUN: %target-swift-frontend -parse-stdlib -target x86_64-apple-macosx12 -typecheck %s -DTARGET_MACOS_12
// REQUIRES: OS=macosx

func f() async { } // expected-error{{concurrency is only available in}}
// expected-note@-1{{add @available}}

actor A { }  // expected-error{{concurrency is only available in}}
// expected-note@-1{{add @available}}

// Allow this without any availability for Historical Reasons.
public func swift_deletedAsyncMethodError() async {
}

// Ensure that our synthesis of the actor's unownedExecutor does not cause
// availability errors.
@available(macOS 12.0, *)
struct S {
  actor A {
  }
}

// The synthesized unownedExecutor inside this extension on S should inherit
// availability from S to avoid availability errors.
#if TARGET_MACOS_12
extension S {
  actor A2 {
  }
}
#endif
