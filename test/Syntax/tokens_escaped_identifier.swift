// RUN: %swift-syntax-test -input-source-filename %s -dump-full-tokens | %FileCheck %s
let /*leading trivia*/ `if` = 3
print(/*leading trivia*/ `if` )

// CHECK-LABEL: 2:25
// CHECK-NEXT:(Token identifier
// CHECK-NEXT: (trivia blockComment /*leading trivia*/)
// CHECK-NEXT: (trivia space 1)
// CHECK-NEXT: (trivia backtick 1)
// CHECK-NEXT: (text="if")
// CHECK-NEXT: (trivia backtick 1)
// CHECK-NEXT: (trivia space 1))

// CHECK-LABEL: 3:27
// CHECK-NEXT:(Token identifier
// CHECK-NEXT: (trivia blockComment /*leading trivia*/)
// CHECK-NEXT: (trivia space 1)
// CHECK-NEXT: (trivia backtick 1)
// CHECK-NEXT: (text="if")
// CHECK-NEXT: (trivia backtick 1)
// CHECK-NEXT: (trivia space 1))
