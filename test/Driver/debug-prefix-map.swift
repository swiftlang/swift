// RUN: not %target-swiftc_driver -debug-prefix-map old %s 2>&1 | %FileCheck %s -check-prefix CHECK-INVALID
// RUN: %target-swiftc_driver -### -debug-prefix-map old=new %s 2>&1 | %FileCheck %s -check-prefix CHECK-SIMPLE
// RUN: %target-swiftc_driver -### -debug-prefix-map old=n=ew %s 2>&1 | %FileCheck %s -check-prefix CHECK-COMPLEX
// RUN: %target-swiftc_driver -### -debug-prefix-map old= %s 2>&1 | %FileCheck %s -check-prefix CHECK-EMPTY

// CHECK-INVALID: error: values for '-debug-prefix-map' must be in the format 'original=remapped', but 'old' was provided
// CHECK-SIMPLE: debug-prefix-map old=new
// CHECK-COMPLEX: debug-prefix-map old=n=ew
// CHECK-EMPTY: debug-prefix-map old=
