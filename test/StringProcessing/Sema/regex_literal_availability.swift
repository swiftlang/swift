// RUN: %target-typecheck-verify-swift -enable-bare-slash-regex -target %target-cpu-apple-macosx12.0

// REQUIRES: swift_swift_parser
// REQUIRES: OS=macosx

_ = /x/ // expected-error {{'Regex' is only available in}}
// expected-note@-1 {{add 'if #available' version check}}

_ = #/x/# // expected-error {{'Regex' is only available in}}
// expected-note@-1 {{add 'if #available' version check}}

if #available(SwiftStdlib 5.7, *) {
  _ = /x/
  _ = #/x/#
}
