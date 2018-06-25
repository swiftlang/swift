// RUN: %target-swift-frontend -typecheck -swift-version 5 %s -verify

func testPopFirst() {
  var str = "abc"
  _ = str.popFirst() // expected-error{{'popFirst()' is unavailable: Please use 'first', 'dropFirst()', or 'Substring.popFirst()'}}
  _ = str.characters.popFirst() // expected-error{{'characters' is unavailable: Please use String directly}}
  // expected-error@-1{{'popFirst()' is unavailable: Please use 'first', 'dropFirst()', or 'Substring.popFirst()'}}
  _ = str.unicodeScalars.popFirst() // expected-error{{'popFirst()' is unavailable: Please use 'first', 'dropFirst()', or 'Substring.UnicodeScalarView.popFirst()'}}

  var charView: String.CharacterView // expected-error{{'CharacterView' is unavailable: Please use String directly}}
  _ = str.characters // expected-error{{'characters' is unavailable: Please use String directly}}
  dump(charView)

  var substr = str[...]
  _ = substr.popFirst() // ok
  _ = substr.characters.popFirst() // expected-error{{'characters' is unavailable: Please use Substring directly}}
  _ = substr.unicodeScalars.popFirst() // ok

  var charSubView: Substring.CharacterView // expected-error{{'CharacterView' is unavailable: Please use Substring directly}}
  _ = substr.characters // expected-error{{'characters' is unavailable: Please use Substring directly}}
  dump(charSubView)
}

