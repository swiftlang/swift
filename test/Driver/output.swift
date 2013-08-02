// RUN: rm -rf %t
// RUN: mkdir -p %t
// RUN: mkdir %t/sub
// RUN: cp %s %t/input.swift

// RUN: %swift -emit-bc %t/input.swift
// RUN: %swift -emit-bc %t/input.swift -module-name output
// RUN: %swift -emit-bc %t/input.swift -o %t/sub/
// RUN: %swift -emit-bc %t/input.swift -o %t/sub/ -module-name output
// RUN: %swift -emit-bc %t/input.swift -o %t/explicit.llvmbc
// RUN: %swift -emit-bc %t/input.swift -o %t/explicit2.llvmbc -module-name output

// RUN: ls -1 %t | FileCheck %s
// RUN: ls -1 %t/sub/ | FileCheck %s -check-prefix=SUB

// CHECK: explicit.llvmbc
// CHECK-NEXT: explicit2.llvmbc
// CHECK-NEXT: input.bc
// CHECK-NEXT: input.swift
// CHECK-NEXT: output.bc
// CHECK-NEXT: sub

// SUB: input.bc
// SUB-NEXT: output.bc
