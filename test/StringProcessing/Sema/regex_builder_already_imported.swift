// RUN: %target-typecheck-verify-swift -enable-bare-slash-regex -disable-availability-checking

import RegexBuilder

extension Regex where Output == Substring {
  init(_ x: String) {} // expected-note {{'init(_:)' declared here}}
}

func foo() {
  // FIXME: This diagnostic could probably be better, it's not clear we should
  // be resolving to init(_ x: String) vs the result builder API and diagnosing
  // the fact that Int isn't a RegexComponent.
  _ = Regex { // expected-error {{trailing closure passed to parameter of type 'String' that does not accept a closure}}
    0
  }
}
