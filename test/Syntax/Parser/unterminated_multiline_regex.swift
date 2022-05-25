// RUN: %swift-syntax-parser-test -dump-diags %s | %FileCheck %s
// CHECK: 7:1 Error: unterminated regex literal
// CHECK: 1 error(s) 0 warnings(s) 0 note(s)

#/
unterminatedLiteral
