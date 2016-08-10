// RUN: %target-run-simple-swift | %FileCheck %s
// REQUIRES: executable_test

// REQUIRES: OS=linux-gnu

// Validation of hashes produced by ICU-based methods used on linux. Doesn't
// use StdlibUnittest because that doesn't work on linux yet. May go away in
// favour of the more comprehensive tests that already exist once it does.

// Let's not crash on changing case.
let upper = "\u{00df}".uppercased()
let lower = "\u{0130}".lowercased()

// ASCII strings
// CHECK: true
print("abc".hashValue == "\0abc".hashValue)

// Unicode strings
// CHECK-NEXT: true
print("abc\u{0130}".hashValue == "\0abc\u{0130}".hashValue)
