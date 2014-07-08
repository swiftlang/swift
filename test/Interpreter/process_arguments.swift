// RUN: %swift -i %s | FileCheck %s -check-prefix=CHECK-NONE
// RUN: %swift -i %s -O0 -g | FileCheck %s -check-prefix=CHECK-NONE
// RUN: %swift -i %s -O0 -g -- | FileCheck %s -check-prefix=CHECK-NONE
// RUN: %swift -i %s -O0 -g -- a b c | FileCheck %s -check-prefix=CHECK-THREE

println("Begin")
for arg in Process.arguments { println(arg) }
println("End")

// CHECK-NONE: Begin
// CHECK-NONE-NEXT: {{.*}}process_arguments.swift
// CHECK-NONE-NEXT: End

// CHECK-THREE: Begin
// CHECK-THREE-NEXT: {{.*}}process_arguments.swift
// CHECK-THREE-NEXT: a
// CHECK-THREE-NEXT: b
// CHECK-THREE-NEXT: c
// CHECK-THREE-NEXT: End
