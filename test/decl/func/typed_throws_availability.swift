// RUN: %target-typecheck-verify-swift -swift-version 5 -module-name test

// REQUIRES: OS=macosx

// RUN: %target-typecheck-verify-swift -target %target-cpu-apple-macos14.4

enum MyError: Error {
  case fail
}

func bad() -> Any.Type {
  // expected-note@-1{{add '@available' attribute to enclosing global function}}
  typealias Fn = () throws(MyError) -> ()
  // expected-error@+2{{runtime support for typed throws function types is only available in macOS 15.0.0 or newer}}
  // expected-note@+1{{add 'if #available' version check}}
  return Fn.self
}

func good() -> Any.Type {
  typealias Fn = () throws(any Error) -> ()
  return Fn.self
}
