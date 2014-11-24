// RUN: rm -rf %t
// RUN: mkdir -p %t
// RUN: mkdir %t/sub
// RUN: cd %t

// RUN: %swift -emit-bc %s
// RUN: %swift -emit-bc %s -module-name explicit
// RUN: %swift -emit-bc %s -o %t/sub/
// RUN: %swift -emit-bc %s -o %t/sub/ -module-name explicit
// RUN: %swift -emit-bc %s -o %t/explicit.llvmbc
// RUN: %swift -emit-bc %s -o %t/explicit2.llvmbc -module-name explicit2
// RUN: echo | %swift -emit-bc - -o %t/sub -module-name stdin

// RUN: ls -1 %t | FileCheck %s
// RUN: ls -1 %t/sub/ | FileCheck %s -check-prefix=SUB

// CHECK: explicit.bc
// CHECK-NEXT: explicit.llvmbc
// CHECK-NEXT: explicit2.llvmbc
// CHECK-NEXT: output.bc
// CHECK-NEXT: sub

// SUB: explicit.bc
// SUB-NEXT: output.bc
// SUB-NEXT: stdin.bc
