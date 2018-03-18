// RUN: cat %s | tr '\132' '\0' > %t.tmp
// RUN: cp -f %t.tmp %t
// RUN: %swift-syntax-test -input-source-filename %t -dump-full-tokens 2>&1 | %FileCheck %t
let a = Z3Z // nul(Z)
func b() {}

// CHECK: 4:9: warning: nul character embedded in middle of file
// CHECK: 4:11: warning: nul character embedded in middle of file
// CHECK: 4:20: warning: nul character embedded in middle of file

// CHECK-LABEL: 4:7
// CHECK-NEXT:(Token equal
// CHECK-NEXT: (text="=")
// CHECK-NEXT: (trivia space 1)
// CHECK-NEXT: (trivia garbageText \000))

// CHECK-LABEL: 4:10
// CHECK-NEXT:(Token integer_literal
// CHECK-NEXT: (text="3")
// CHECK-NEXT: (trivia garbageText \000)
// CHECK-NEXT: (trivia space 1))

// CHECK-LABEL: 5:1
// CHECK-NEXT:(Token kw_func
// CHECK-NEXT: (trivia lineComment // nul(\000))
// CHECK-NEXT: (trivia newline 1)
// CHECK-NEXT: (text="func")
// CHECK-NEXT: (trivia space 1))
