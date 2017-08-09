// RUN: %swift -typecheck -swift-version 3 %s -verify

func testPopFirst() {
  var str = "abc"
  _ = str.popFirst() // expected-warning{{'popFirst()' is deprecated: Please use 'first', 'dropFirst()', or 'Substring.popFirst()'}}
  _ = str.characters.popFirst() // expected-warning{{'characters' is deprecated: Please use String or Substring directly}}
    // expected-warning@-1{{'popFirst()' is deprecated: Please use 'first', 'dropFirst()', or 'Substring.CharacterView.popFirst()'}}
  _ = str.unicodeScalars.popFirst() // expected-warning{{'popFirst()' is deprecated: Please use 'first', 'dropFirst()', or 'Substring.UnicodeScalarView.popFirst()'}}

  var charView: String.CharacterView // expected-warning{{'CharacterView' is deprecated: Please use String or Substring directly}}
  charView = str.characters // expected-warning{{'characters' is deprecated: Please use String or Substring directly}}
  dump(charView)

  var substr = str[...]
  _ = substr.popFirst() // ok
  _ = substr.characters.popFirst() // expected-warning{{'characters' is deprecated: Please use String or Substring directly}}
  _ = substr.unicodeScalars.popFirst() // ok

  var charSubView: Substring.CharacterView // expected-warning{{'CharacterView' is deprecated: Please use String or Substring directly}}
  charSubView = substr.characters // expected-warning{{'characters' is deprecated: Please use String or Substring directly}}
  dump(charSubView)
}



