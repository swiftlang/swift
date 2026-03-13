// RUN: %target-swift-frontend -parse-stdlib -target x86_64-apple-macosx10.14 -typecheck -verify %s
// RUN: %target-swift-frontend -parse-stdlib -target x86_64-apple-macosx11 -typecheck %s
// RUN: %target-swift-frontend -parse-stdlib -target x86_64-apple-macosx12 -typecheck %s -DTARGET_MACOS_12
// REQUIRES: OS=macosx

import _Concurrency

func f() async { } // expected-error{{concurrency is only available in}}
// expected-note@-1{{add '@available'}}

actor A { }  // expected-error{{concurrency is only available in}}
// expected-note@-1{{add '@available'}}

// Allow this without any availability for Historical Reasons.
public func swift_deletedAsyncMethodError() async {
}

@available(macOS 12.0, *)
struct S {
  // Ensure that our synthesis of the actor's unownedExecutor does not cause
  // availability errors.
  actor NestedActor {
  }

  // The synthesized unownedExecutor inside this actor should inherit the
  // un-availability of UnavailableActor.
  @available(macOS, unavailable)
  actor UnavailableActor {
  }
}

#if TARGET_MACOS_12
// The synthesized unownedExecutor inside this extension on S should inherit
// availability from S to avoid availability errors.
extension S {
  actor ExtensionNestedActor {
  }
}
#endif

// Make sure that the conformances to Actor are actually being synthesized
// since otherwise this test isn't actually testing what it is designed to test.
@available(macOS 10.15, *)
func takesExecutor(_ e: UnownedSerialExecutor) { }

@available(macOS 12.0, *)
func testNestedActorConformance(_ a: S.NestedActor) {
  takesExecutor(a.unownedExecutor)
}

#if TARGET_MACOS_12
func testExtensionNestedActorConformance(_ a: S.ExtensionNestedActor) {
  takesExecutor(a.unownedExecutor)
}
#endif
