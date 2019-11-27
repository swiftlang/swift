// RUN: not %target-swift-frontend -enable-descriptive-diagnostics -diagnostic-documentation-path %S/test-docs/ -typecheck %s 2>&1 | %FileCheck %s

// A diagnostic with no educational notes
let x = 1 +
// CHECK: error: expected expression after operator
// CHECK-NOT: {{-+$}}

// A diagnostic with an educational note
extension (Int, Int) {}
// CHECK: error: non-nominal type '(Int, Int)' cannot be extended
// CHECK-NEXT: extension (Int, Int) {}
// CHECK-NEXT: ^         ~~~~~~~~~~
// CHECK-NEXT: Nominal Types
// CHECK-NEXT: -------------
// CHECK-NEXT: Nominal types documentation content
