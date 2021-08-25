// RUN: %target-swift-frontend -parse-stdlib -target x86_64-apple-macosx10.13 -typecheck -verify -enable-experimental-back-deploy-concurrency %s
// RUN: %target-swift-frontend -parse-stdlib -target x86_64-apple-macosx10.15 -typecheck -enable-experimental-back-deploy-concurrency %s
// REQUIRES: OS=macosx

func f() async { } // expected-error{{concurrency is only available in}}
// expected-note@-1{{add @available}}

actor A { }  // expected-error{{concurrency is only available in}}
// expected-note@-1{{add @available}}

// Allow this without any availability for Historical Reasons.
public func swift_deletedAsyncMethodError() async {
}
