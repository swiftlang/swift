// RUN: %target-parse-verify-swift -enable-character-literals

func testTypeInference() {
  func isUnicodeScalar(inout _: UnicodeScalar) {}
  func isCharacter(inout _: Character) {}
  func isString(inout _: String) {}

  var singleQuoted1 = 'a' // expected-error {{expression does not conform to type 'CharacterLiteralConvertible'}}
  var singleQuoted2 = 'あ' // expected-error {{expression does not conform to type 'CharacterLiteralConvertible'}}

  var us1: UnicodeScalar = "" // expected-error {{'String' is not convertible to 'UnicodeScalar'}}
  var us2: UnicodeScalar = "a"
  isUnicodeScalar(&us2)
  var us3: UnicodeScalar = "あ"
  isUnicodeScalar(&us3)
  var us4: UnicodeScalar = "例"
  isUnicodeScalar(&us4)
  var us5: UnicodeScalar = "\u{304b}\u{3099}" // expected-error {{'String' is not convertible to 'UnicodeScalar'}}
  var us6: UnicodeScalar = "ab" // expected-error {{'String' is not convertible to 'UnicodeScalar'}}

  var ch1: Character = "" // expected-error {{'String' is not convertible to 'Character'}}
  var ch2: Character = "a"
  isCharacter(&ch2)
  var ch3: Character = "あ"
  isCharacter(&ch3)
  var ch4: Character = "例"
  isCharacter(&ch4)
  var ch5: Character = "\u{304b}\u{3099}"
  isCharacter(&ch5)
  var ch6: Character = "ab" // expected-error {{'String' is not convertible to 'Character'}}

  var s1 = ""
  isString(&s1)
  var s2 = "a"
  isString(&s2)
  var s3 = "ab"
  isString(&s3)
  var s4 = "あ"
  isString(&s4)
  var s5 = "例"
  isString(&s5)
  var s6 = "\u{304b}\u{3099}"
  isString(&s6)
  var s7 = "ab"
  isString(&s7)
}
testTypeInference()

func overloaded(a: UnicodeScalar) {}
func overloaded(a: Character) {}
func overloaded(a: String) {}

func testOverloads() {
  overloaded("")
  overloaded("a")
  overloaded("あ")
  overloaded("\u{304b}\u{3099}")
  overloaded("abc")

  // Ensure that one can force a particular overload.
  overloaded(UnicodeScalar("a"))
  overloaded(Character("a"))
}

func testAddCharacters(a: Character) {
  a + a // expected-error {{binary operator '+' cannot be applied to two Character operands}}
}

