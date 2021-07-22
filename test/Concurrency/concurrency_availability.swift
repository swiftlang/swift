// RUN: %target-swift-frontend -parse-stdlib -target x86_64-apple-macosx10.15 -typecheck -verify %s
// RUN: %target-swift-frontend -parse-stdlib -target x86_64-apple-macosx12 -typecheck %s
// REQUIRES: OS=macosx

func f() async { } // expected-error{{concurrency is only available in}}
// expected-note@-1{{add @available}}

actor A { }  // expected-error{{concurrency is only available in}}
// expected-note@-1{{add @available}}

// Allow this without any availability for Historical Reasons.
public func swift_deletedAsyncMethodError() async {
}
