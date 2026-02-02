// RUN: %target-typecheck-verify-swift -verify-ignore-unrelated -enable-bare-slash-regex -disable-availability-checking

import RegexBuilder

extension Regex where Output == Substring {
  init(_ x: String) {}
}

func foo() {
  _ = Regex {
    0 // expected-error {{static method 'buildExpression' requires that 'Int' conform to 'RegexComponent'}}
  }
}
