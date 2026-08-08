// RUN: %swift -typecheck -verify -target %target-cpu-apple-macosx11 %s

// REQUIRES: OS=macosx

@available(macOS 13, *)
enum MyError: Error {
  case fail
}

@available(macOS 12, *)
func throwMyErrorBadly() throws(MyError) { }
// expected-note@-1 {{update '@available' attribute on enclosing}}
// expected-error@-2 {{'MyError' is only available in macOS 13 or newer}}
