// RUN: %target-typecheck-verify-swift -enable-bare-slash-regex -disable-availability-checking

// REQUIRES: swift_swift_parser

Regex {} // expected-error {{regex builder requires the 'RegexBuilder' module be imported'}} {{5:1-1=import RegexBuilder\n\n}}

Regex { // expected-error {{regex builder requires the 'RegexBuilder' module be imported'}} {{5:1-1=import RegexBuilder\n\n}}
  /abc/
}

Regex { // expected-error {{regex builder requires the 'RegexBuilder' module be imported'}} {{5:1-1=import RegexBuilder\n\n}}
  /abc/
  /def/
}

Regex { // expected-error {{regex builder requires the 'RegexBuilder' module be imported'}} {{5:1-1=import RegexBuilder\n\n}}
  /abc/
  "def"
  /g(h)(i)/
}

Regex { // expected-error {{regex builder requires the 'RegexBuilder' module be imported'}}
  Capture { // expected-error {{cannot find 'Capture' in scope}}
    /abc/
  }
}

// It's not clear what the user is doing here so fall back to regular diagnostics.
Regex(a: 0) {
  // expected-error@-1 {{cannot convert value of type 'Int' to expected argument type 'String'}}
  // expected-error@-2 {{extra trailing closure passed in call}}
  /abc/
}
