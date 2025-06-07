// RUN: %target-typecheck-verify-swift -enable-bare-slash-regex -target %target-cpu-apple-macosx12.0

// REQUIRES: swift_swift_parser
// REQUIRES: OS=macosx

import RegexBuilder

// rdar://97533700 – Make sure we can emit an availability diagnostic here.

let _ = Regex { // expected-error {{'Regex' is only available in macOS 13.0 or newer}}
  // expected-error@-1 {{'init(_:)' is only available in macOS 13.0 or newer}}
  // expected-note@-2 2{{add 'if #available' version check}}

  Capture {} // expected-error {{'Capture' is only available in macOS 13.0 or newer}}
  // expected-error@-1 {{'init(_:)' is only available in macOS 13.0 or newer}}
  // expected-note@-2 2{{add 'if #available' version check}}
}

let _ = Regex { // expected-error {{'Regex' is only available in macOS 13.0 or newer}}
  // expected-error@-1 {{'init(_:)' is only available in macOS 13.0 or newer}}
  // expected-error@-2 {{'buildPartialBlock(accumulated:next:)' is only available in macOS 13.0 or newer}}
  // expected-note@-3 3{{add 'if #available' version check}}

  /abc/ // expected-error {{'Regex' is only available in macOS 13.0 or newer}}
  // expected-note@-1 {{add 'if #available' version check}}

  /def/ // expected-error {{'Regex' is only available in macOS 13.0 or newer}}
  // expected-note@-1 {{add 'if #available' version check}}
}
