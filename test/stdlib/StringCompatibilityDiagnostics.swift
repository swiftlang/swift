// RUN: %swift -typecheck -swift-version 4 %s -verify

func testPopFirst() {
  var str = "abc"
  _ = str.popFirst() // expected-error{{'popFirst()' is unavailable: Please use 'first', 'dropFirst()', or 'Substring.popFirst()'}}
  _ = str.characters.popFirst() // TODO: deprecate the view, and update the error here
  _ = str.unicodeScalars.popFirst() // expected-error{{'popFirst()' is unavailable: Please use 'first', 'dropFirst()', or 'Substring.UnicodeScalarView.popFirst()'}}

  var substr = str[...]
  _ = substr.popFirst() // ok
  _ = substr.characters.popFirst() // ok
  _ = substr.unicodeScalars.popFirst() // ok
}

