// RUN: %swift %s -verify

func testTypeInference() {
  func isUnicodeScalar(inout _: UnicodeScalar) {}
  func isCharacter(inout _: Character) {}
  func isString(inout _: String) {}

  var singleQuoted1 = 'a' // expected-error {{cannot convert the expression's type '$T0' to type '$T1'}}
  var singleQuoted2 = 'あ' // expected-error {{cannot convert the expression's type '$T0' to type '$T1'}}

  var us1: UnicodeScalar = "" // expected-error {{cannot convert the expression's type 'String' to type 'UnicodeScalar'}}
  var us2: UnicodeScalar = "a"
  isUnicodeScalar(&us1)
  var us3: UnicodeScalar = "あ"
  isUnicodeScalar(&us2)

  var ch1: Character = "" // expected-error {{cannot convert the expression's type 'String' to type 'Character'}}
  var ch2: Character = "a"
  isCharacter(&ch1)
  var ch3: Character = "あ"
  isCharacter(&ch2)
  var ch4: Character = "例"
  isCharacter(&ch3)
  // FIXME: this should pass when we implement grapheme cluster extraction
  // correctly.
  var ch5: Character = "\u304b\u3099" // expected-error {{cannot convert the expression's type 'String' to type 'Character'}}
  isCharacter(&ch4)

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
  var s6 = "\u304b\u3099"
  isString(&s6)
}
testTypeInference()

func overloaded(a: UnicodeScalar) {}
func overloaded(a: Character) {}
func overloaded(a: String) {}

func testOverloads() {
  overloaded("")
  overloaded("a")
  overloaded("あ")
  overloaded("\u304b\u3099")
  overloaded("abc")

  // Ensure that one can force a particular overload.
  overloaded(UnicodeScalar("a"))
  overloaded(Character("a"))
}

