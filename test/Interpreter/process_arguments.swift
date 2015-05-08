// RUN: %swift -interpret %s | FileCheck %s -check-prefix=CHECK-NONE
// RUN: %swift -interpret %s -Onone -g | FileCheck %s -check-prefix=CHECK-NONE
// RUN: %swift -interpret %s -Onone -g -- | FileCheck %s -check-prefix=CHECK-NONE
// RUN: %swift -interpret %s -Onone -g -- a b c | FileCheck %s -check-prefix=CHECK-THREE

// REQUIRES: swift_interpreter

print("Begin")
for arg in Process.arguments { print(arg) }
print("End")

// CHECK-NONE: Begin
// CHECK-NONE-NEXT: {{.*}}process_arguments.swift
// CHECK-NONE-NEXT: End

// CHECK-THREE: Begin
// CHECK-THREE-NEXT: {{.*}}process_arguments.swift
// CHECK-THREE-NEXT: a
// CHECK-THREE-NEXT: b
// CHECK-THREE-NEXT: c
// CHECK-THREE-NEXT: End
