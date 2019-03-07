var bar: Int {
    return 0
}
// CHECK-NO-ENV-LABEL: Action begins
// CHECK-NO-ENV-NEXT: Action ends

// CHECK-ENV-LABEL: Action begins
// CHECK-ENV-NEXT: Replace Function Bodies With 'fatalError()'
// CHECK-ENV-NEXT: Action ends

// replace.bodies.with.fatalError/-replace-bodies-with-fatalError should only be
// found if the appropriate environment variable is set.

// RUN: %refactor -actions -source-filename %s -pos=1:1 -end-pos=3:2 | %FileCheck %s -check-prefix CHECK-NO-ENV

// RUN: env SWIFT_ENABLE_INTERNAL_REFACTORING_ACTIONS=1 %refactor -actions -source-filename %s -pos=1:1 -end-pos=3:2 | %FileCheck %s -check-prefix CHECK-ENV
