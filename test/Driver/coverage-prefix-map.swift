// RUN: not %target-swiftc_driver -coverage-prefix-map old %s 2>&1 | %FileCheck %s -check-prefix CHECK-INVALID
// RUN: %target-swiftc_driver -### -coverage-prefix-map old=new %s 2>&1 | %FileCheck %s -check-prefix CHECK-SIMPLE
// RUN: %target-swiftc_driver -### -coverage-prefix-map old=n=ew %s 2>&1 | %FileCheck %s -check-prefix CHECK-COMPLEX
// RUN: %target-swiftc_driver -### -coverage-prefix-map old= %s 2>&1 | %FileCheck %s -check-prefix CHECK-EMPTY

// CHECK-INVALID: error: invalid argument 'old' to -coverage-prefix-map
// CHECK-SIMPLE: coverage-prefix-map old=new
// CHECK-COMPLEX: coverage-prefix-map old=n=ew
// CHECK-EMPTY: coverage-prefix-map old=
