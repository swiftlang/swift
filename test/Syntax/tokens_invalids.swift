// RUN: cat %s > %t

// 5a is Z. "ZN" style marker is used for marker. N is number.

// C2 is utf8 2 byte character start byte.
// RUN: cat %t | sed 's/'$(echo -ne "\x5a1")'/'$(echo -ne "\xc2")'/g' > %t.sed
// RUN: cp -f %t.sed %t

// CC 82 is U+0302, invalid for identifier start, valid for identifier body.
// RUN: cat %t | sed 's/'$(echo -ne "\x5a2")'/'$(echo -ne "\xcc\x82")'/g' > %t.sed
// RUN: cp -f %t.sed %t

// E2 80 9D is U+201D, right quote.
// RUN: cat %t | sed 's/'$(echo -ne "\x5a3")'/'$(echo -ne "\xe2\x80\x9d")'/g' > %t.sed
// RUN: cp -f %t.sed %t

// E2 80 9C is U+201C, left quote.
// RUN: cat %t | sed 's/'$(echo -ne "\x5a4")'/'$(echo -ne "\xe2\x80\x9c")'/g' > %t.sed
// RUN: cp -f %t.sed %t

// E1 9A 80 is U+1680, invalid for swift source.
// RUN: cat %t | sed 's/'$(echo -ne "\x5a5")'/'$(echo -ne "\xe1\x9a\x80")'/g' > %t.sed
// RUN: cp -f %t.sed %t

// RUN: %swift-syntax-test -input-source-filename %t -dump-full-tokens 2>&1 | %FileCheck %t

x
Z1 x
Z2
Z3
Z4
Z4 abcdef Z3
Z5 x

// test diagnostics.

// CHECK: 28:1: error: invalid UTF-8 found in source file
// CHECK: 29:1: error: an identifier cannot begin with this character
// CHECK: 30:1: error: unicode curly quote found
// CHECK: 31:1: error: unicode curly quote found
// CHECK: 32:12: error: unicode curly quote found
// CHECK: 32:1: error: unicode curly quote found
// CHECK: 33:1: error: invalid character in source file

// test tokens and trivias.

// CHECK-LABEL: 28:3
// CHECK-NEXT:  (Token identifier
// CHECK-NEXT:   (trivia newline 1)
// CHECK-NEXT:   (trivia garbage_text \302)
// CHECK-NEXT:   (trivia space 1)
// CHECK-NEXT:   (text="x"))

// CHECK-LABEL: 29:1
// CHECK-NEXT:  (Token unknown
// CHECK-NEXT:   (trivia newline 1)
// CHECK-NEXT:   (text="\xCC\x82"))

// CHECK-LABEL: 30:1
// CHECK-NEXT:  (Token unknown
// CHECK-NEXT:   (trivia newline 1)
// CHECK-NEXT:   (text="\xE2\x80\x9D"))

// CHECK-LABEL: 31:1
// CHECK-NEXT:  (Token unknown
// CHECK-NEXT:   (trivia newline 1)
// CHECK-NEXT:   (text="\xE2\x80\x9C"))

// CHECK-LABEL: 32:1
// CHECK-NEXT:  (Token unknown
// CHECK-NEXT:   (trivia newline 1)
// CHECK-NEXT:   (text="\xE2\x80\x9C abcdef \xE2\x80\x9D"))

// CHECK-LABEL: 33:5
// CHECK-NEXT:  (Token identifier
// CHECK-NEXT:   (trivia newline 1)
// CHECK-NEXT:   (trivia garbage_text \341\232\200)
// CHECK-NEXT:   (trivia space 1)
// CHECK-NEXT:   (text="x"))
