// RUN: %target-typecheck-verify-swift -enable-bare-slash-regex -disable-availability-checking

// REQUIRES: swift_in_compiler

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

// FIXME: Unfortunately we bail from CSGen if we end up with an ErrorExpr, so
// don't get a chance to diagnose. We ought to try solving with holes.
// For now at least, this error should at least hopefully nudge users into
// realizing they have a missing import.
Regex {
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
