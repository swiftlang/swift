// RUN: %target-typecheck-verify-swift

struct UnicodeScalarExpressedUnicodeScalar : ExpressibleByUnicodeScalarLiteral {
  init(unicodeScalarLiteral value: UnicodeScalar) {}
}
struct CharacterExpressedUnicodeScalar : ExpressibleByUnicodeScalarLiteral {
  init(unicodeScalarLiteral value: Character) {}
}
struct StringExpressedUnicodeScalar : ExpressibleByUnicodeScalarLiteral {
  init(unicodeScalarLiteral value: String) {}
}
struct StaticStringExpressedUnicodeScalar : ExpressibleByUnicodeScalarLiteral {
  init(unicodeScalarLiteral value: StaticString) {}
}

func customUnicodeScalarLiterals {
  var a: UnicodeScalarExpressedUnicodeScalar = "å"
  a = "a"
  
  var e: CharacterExpressedUnicodeScalar = "é"
  e = "e"
  
  var i: StringExpressedUnicodeScalar = "ï"
  i = "i"
  
  var o: StaticStringExpressedUnicodeScalar = "ø"
  o = "o"
}
this is not compiled
