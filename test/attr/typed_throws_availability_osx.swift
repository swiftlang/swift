// RUN: %swift -typecheck -verify -target x86_64-apple-macosx10.10 %s -enable-experimental-feature TypedThrows

// REQUIRES: OS=macosx

@available(macOS 12, *)
enum MyError: Error {
  case fail
}

@available(macOS 11, *)
func throwMyErrorBadly() throws(MyError) { }
// expected-error@-1{{'MyError' is only available in macOS 12 or newer}}



