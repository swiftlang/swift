// To embed test byte sequence,
// this source replace marker to byte sequence first in runtime.
// Marker(N) have `ZN` style format. Z is Z, N is number.
// Byte sequence is represented in escape sequence.
// To avoid replace marker in sed command by sed itself,
// marker is also represented in escape sequence.

// RUN: cat %s | sed -f %S/Inputs/invalid.sed > %t
// RUN: %{python} -c "import sys; t = open(sys.argv[1], 'rb').read().replace(b'\r\n', b'\n'); open(sys.argv[1], 'wb').write(t)" %t
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
// CHECK: 14:1: error: invalid UTF-8 found in source file
// CHECK: 14:7: error: invalid UTF-8 found in source file
// CHECK: 16:5: error: an identifier cannot begin with this character
// CHECK: 18:5: error: an identifier cannot begin with this character
// CHECK: 20:5: error: unicode curly quote found
// CHECK: 20:8: error: unicode curly quote found
// CHECK: 22:19: error: unicode curly quote found
// CHECK: 22:5: error: unicode curly quote found
// CHECK: 24:5: error: unicode curly quote found
// CHECK: 27:1: error: invalid character in source file
// CHECK: 27:9: error: invalid character in source file

// Checks around bbb
// CHECK-LABEL: 14:3
// CHECK-NEXT:  (Token identifier
// CHECK-NEXT:   (trivia newline 1)
// CHECK-NEXT:   (trivia garbageText \302)
// CHECK-NEXT:   (trivia space 1)
// CHECK-NEXT:   (text="bbb")
// CHECK-NEXT:   (trivia space 1)
// CHECK-NEXT:   (trivia garbageText \302))

// Checks around ccc
// CHECK-LABEL: 16:5
// CHECK-NEXT:  (Token unknown
// CHECK-NEXT:   (text="\xCC\x82"))

// Checks around ddd
// CHECK-LABEL: 18:5
// CHECK-NEXT:  (Token unknown
// CHECK-NEXT:   (text="\xCC\x82\xCC\x82\xCC\x82\xCC\x82"))

// Checks around eee
// CHECK-LABEL: 20:5
// CHECK-NEXT:  (Token unknown
// CHECK-NEXT:   (text="\xE2\x80\x9C"))
// CHECK-LABEL: 20:8
// CHECK-NEXT:  (Token unknown
// CHECK-NEXT:   (text="\xE2\x80\x9C"))

// Checks around fff
// CHECK-LABEL: 22:5
// CHECK-NEXT:  (Token unknown
// CHECK-NEXT:   (text="\xE2\x80\x9Chello world\xE2\x80\x9D"))

// Checks around ggg
// CHECK-LABEL: 24:5
// CHECK-NEXT:  (Token unknown
// CHECK-NEXT:   (text="\xE2\x80\x9D"))

// Checks around iii
// CHECK-LABEL: 27:5
// CHECK-NEXT:  (Token identifier
// CHECK-NEXT:   (trivia newline 1)
// CHECK-NEXT:   (trivia garbageText \341\232\200)
// CHECK-NEXT:   (trivia space 1)
// CHECK-NEXT:   (text="iii")
// CHECK-NEXT:   (trivia space 1)
// CHECK-NEXT:   (trivia garbageText \341\232\200))
