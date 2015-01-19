// RUN: %target-parse-verify-swift -parse-stdlib -disable-access-control -enable-character-literals

import Swift

typealias CharacterLiteralType = MyCharacter

struct MyCharacter :
    _BuiltinCharacterLiteralConvertible, CharacterLiteralConvertible,
    Equatable {

  init(_builtinCharacterLiteral value: Builtin.Int32) { }
  init(characterLiteral value: MyCharacter) { }
}

func ==(lhs: MyCharacter, rhs: MyCharacter) -> Bool {
  return true
}

//===----------------------------------------------------------------------===//
// Character Literals
//===----------------------------------------------------------------------===//

var ch_nul = '\0'
var ch_1 = '\1'     // expected-error {{invalid escape sequence in literal}}
var ch_at = '\@'    // expected-error {{invalid escape sequence in literal}}
var ch_1_ = '\1     // expected-error {{invalid escape sequence in literal}}
var ch_01_ = '\01   // expected-error {{unterminated character literal}}
var ch_01a_ = '\01a // expected-error {{unterminated character literal}}
var ch_at_ = '\@    // expected-error {{invalid escape sequence in literal}}
var ch_01 = '\01'   // expected-error {{invalid multiple-code-point character literal}}
var ch_011 = '\011' // expected-error {{invalid multiple-code-point character literal}}
var ch_0a = '\0a'   // expected-error {{invalid multiple-code-point character literal}}
var ch_01a = '\01a' // expected-error {{invalid multiple-code-point character literal}}
var ch_0z = '\0z'   // expected-error {{invalid multiple-code-point character literal}}
var ch_01z = '\01z' // expected-error {{invalid multiple-code-point character literal}}
var ch_0at = '\0@'   // expected-error {{invalid multiple-code-point character literal}}
var ch_01at = '\01@' // expected-error {{invalid multiple-code-point character literal}}
var ch_US = '🇺🇸'    // expected-error {{invalid multiple-code-point character literal}}

var ch_Joker = '🃏'

var ch_n = '\n'
var ch_r = '\r'
var ch_t = '\t'
var ch_x = '\u{7f}'   // DEL
var ch_q = ''     // expected-error {{unprintable ASCII character found in source file}}
var ch_u = '\u{014d}' // 'ō'
var ch_U = '\u{0001D41F}' // '𝐁' a.k.a. "Math alphanumeric B"

var ch_x_too_big = '\u{80}'
var ch_U_too_big = '\u{12345678}' // expected-error {{invalid unicode scalar}}

var ch_x_too_short = '\u{1}'

// Recovery for invalid character literals.
func isSpace(c: MyCharacter) -> Bool {
  return (c == ' ' ||
          c == '' ||      // expected-error {{invalid character literal}}
          c == '\v' ||    // expected-error {{invalid escape sequence in literal}}
          c == '\f' ||    // expected-error {{invalid escape sequence in literal}}
          c == '\t')
}

func isEnglishSpeaking(c: MyCharacter) -> Bool {
  return c == '🇺🇸' // expected-error {{invalid multiple-code-point character literal}}
      || c == '🇬🇧' // expected-error {{invalid multiple-code-point character literal}}
}

