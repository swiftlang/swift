// RUN: %target-swift-frontend -typecheck -swift-version 4 %s -verify

func testPopFirst() {
  let str = "abc"
  var charView: String.CharacterView // expected-warning{{'CharacterView' is deprecated: Please use String directly}}{{documentation-file=deprecated-declaration}}
  charView = str.characters // expected-warning{{'characters' is deprecated: Please use String directly}}{{documentation-file=deprecated-declaration}}
  dump(charView)

  var substr = str[...]
  _ = substr.popFirst() // ok
  _ = substr.characters.popFirst() // expected-warning{{'characters' is deprecated: Please use Substring directly}}{{documentation-file=deprecated-declaration}}
  _ = substr.unicodeScalars.popFirst() // ok

  var charSubView: Substring.CharacterView // expected-warning{{'CharacterView' is deprecated: Please use Substring directly}}{{documentation-file=deprecated-declaration}}
  charSubView = substr.characters // expected-warning{{'characters' is deprecated: Please use Substring directly}}{{documentation-file=deprecated-declaration}}
  dump(charSubView)

  var _ = String(str.utf8) ?? "" // expected-warning{{left side of nil coalescing operator '??' has non-optional type 'String', so the right side is never used}}
  var _: String = String(str.utf8) // ok
}



