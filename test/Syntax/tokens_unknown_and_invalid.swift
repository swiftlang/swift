// To embed test byte sequence,
// this source replace marker to byte sequence first in runtime.
// Marker(N) have `ZN` style format. Z is Z, N is number.
// Byte sequence is represented in escape sequence.
// To avoid replace marker in sed command by sed itself,
// marker is also represented in escape sequence.

// RUN: cat %s | sed \

// [0xC2] is utf8 2 byte character start byte.
// 0xC2 without second byte is invalid UTF-8 sequence.
// It becomes garbage text trivia.
// Marker(1) is replaced to this sequence.

// RUN: -e 's/'$(echo -ne "\x5a1")'/'$(echo -ne "\xc2")'/g' \

// [0xCC, 0x82] in UTF-8 is U+0302.
// This character is invalid for identifier start, but valid for identifier body.
// It becomes unknown token.
// If this type characters are conitguous, they are concatenated to one long unknown token.
// Marker(2) is replaced to this sequence.

// RUN: -e 's/'$(echo -ne "\x5a2")'/'$(echo -ne "\xcc\x82")'/g' \

// [0xE2, 0x80, 0x9C] in UTF-8 is U+201C, left quote.
// It becomes single character unknown token.
// If this left quote and right quote enclosure text,
// they become one long unknown token.
// Marker(3) is replaced to this sequence.

// RUN: -e 's/'$(echo -ne "\x5a3")'/'$(echo -ne "\xe2\x80\x9c")'/g' \

// [0xE2, 0x80, 0x9D] in UTF-8 is U+201D, right quote.
// It becomes single character unknown token.
// Marker(4) is replaced to this sequence.

// RUN: -e 's/'$(echo -ne "\x5a4")'/'$(echo -ne "\xe2\x80\x9d")'/g' \

// [0xE1, 0x9A, 0x80] in UTF-8 is U+1680.
// This character is invalid for swift source.
// It becomes garbage trivia.
// Marker(5) is replaced to this sequence.

// RUN: -e 's/'$(echo -ne "\x5a5")'/'$(echo -ne "\xe1\x9a\x80")'/g' \

// RUN: > %t

// RUN: %swift-syntax-test -input-source-filename %t -dump-full-tokens 2>&1 | %FileCheck %t
// RUN: %round-trip-syntax-test --swift-syntax-test %swift-syntax-test --file %t

aaa
Z1 bbb Z1

ccc Z2

ddd Z2Z2Z2Z2

eee Z3Z3

fff Z3hello worldZ4

ggg Z4

hhh
Z5 iii Z5
jjj

// Diagnostics
// CHECK: 52:1: error: invalid UTF-8 found in source file
// CHECK: 52:7: error: invalid UTF-8 found in source file
// CHECK: 54:5: error: an identifier cannot begin with this character
// CHECK: 56:5: error: an identifier cannot begin with this character
// CHECK: 58:5: error: unicode curly quote found
// CHECK: 58:8: error: unicode curly quote found
// CHECK: 60:19: error: unicode curly quote found
// CHECK: 60:5: error: unicode curly quote found
// CHECK: 62:5: error: unicode curly quote found
// CHECK: 65:1: error: invalid character in source file
// CHECK: 65:9: error: invalid character in source file

// Checks around bbb
// CHECK-LABEL: 52:3
// CHECK-NEXT:  (Token identifier
// CHECK-NEXT:   (trivia newline 1)
// CHECK-NEXT:   (trivia garbageText \302)
// CHECK-NEXT:   (trivia space 1)
// CHECK-NEXT:   (text="bbb")
// CHECK-NEXT:   (trivia space 1)
// CHECK-NEXT:   (trivia garbageText \302))

// Checks around ccc
// CHECK-LABEL: 54:5
// CHECK-NEXT:  (Token unknown
// CHECK-NEXT:   (text="\xCC\x82"))

// Checks around ddd
// CHECK-LABEL: 56:5
// CHECK-NEXT:  (Token unknown
// CHECK-NEXT:   (text="\xCC\x82\xCC\x82\xCC\x82\xCC\x82"))

// Checks around eee
// CHECK-LABEL: 58:5
// CHECK-NEXT:  (Token unknown
// CHECK-NEXT:   (text="\xE2\x80\x9C"))
// CHECK-LABEL: 58:8
// CHECK-NEXT:  (Token unknown
// CHECK-NEXT:   (text="\xE2\x80\x9C"))

// Checks around fff
// CHECK-LABEL: 60:5
// CHECK-NEXT:  (Token unknown
// CHECK-NEXT:   (text="\xE2\x80\x9Chello world\xE2\x80\x9D"))

// Checks around ggg
// CHECK-LABEL: 62:5
// CHECK-NEXT:  (Token unknown
// CHECK-NEXT:   (text="\xE2\x80\x9D"))

// Checks around iii
// CHECK-LABEL: 65:5
// CHECK-NEXT:  (Token identifier
// CHECK-NEXT:   (trivia newline 1)
// CHECK-NEXT:   (trivia garbageText \341\232\200)
// CHECK-NEXT:   (trivia space 1)
// CHECK-NEXT:   (text="iii")
// CHECK-NEXT:   (trivia space 1)
// CHECK-NEXT:   (trivia garbageText \341\232\200))
