// RUN: %target-typecheck-verify-swift -enable-bare-slash-regex -disable-availability-checking

// REQUIRES: swift_swift_parser

struct S {
  func foo() {
    Regex { // expected-error {{regex builder requires the 'RegexBuilder' module be imported'}} {{5:1-1=import RegexBuilder\n\n}}
      /abc/
    }
  }
}
