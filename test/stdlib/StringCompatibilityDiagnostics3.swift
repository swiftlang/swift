// RUN: %swift -typecheck -swift-version 3 %s -verify

func testPopFirst() {
  var str = "abc"
  _ = str.popFirst() // expected-warning{{'popFirst()' is deprecated: Please use 'first', 'dropFirst()', or 'Substring.popFirst()'}}
  _ = str.characters.popFirst() // expected-warning{{'popFirst()' is deprecated: Please use 'first', 'dropFirst()', or 'Substring.CharacterView.popFirst()'}}
    // TODO: ^^^ deprecate the view, and update the warning here
  _ = str.unicodeScalars.popFirst() // expected-warning{{'popFirst()' is deprecated: Please use 'first', 'dropFirst()', or 'Substring.UnicodeScalarView.popFirst()'}}

  var substr = str[...]
  _ = substr.popFirst() // ok
  _ = substr.characters.popFirst() // ok
  _ = substr.unicodeScalars.popFirst() // ok
}



