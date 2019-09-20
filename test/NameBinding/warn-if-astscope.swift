// Verify the action of -warn-if-astscope-lookup
//
// RUN: not %target-swift-frontend -typecheck %s -enable-astscope-lookup 2>&1 |  %FileCheck %s --check-prefix=CHECK-NO-WARN
// RUN: not %target-swift-frontend -typecheck %s -disable-astscope-lookup 2>&1 | %FileCheck %s --check-prefix=CHECK-NO-WARN
// RUN: not %target-swift-frontend -typecheck %s -enable-astscope-lookup -warn-if-astscope-lookup 2>&1 | %FileCheck %s --check-prefix=CHECK-WARN
// RUN: not %target-swift-frontend -typecheck %s -disable-astscope-lookup -warn-if-astscope-lookup 2>&1 | %FileCheck %s --check-prefix=CHECK-NO-WARN

func foo() -> Int {
  return bar() // create an error so the input to fileCheck isn't empty
}

// CHECK-NO-WARN-NOT: WARNING: TRYING Scope exclusively
// CHECK-WARN: WARNING: TRYING Scope exclusively
