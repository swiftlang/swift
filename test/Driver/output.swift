// RUN: rm -rf %t
// RUN: mkdir -p %t
// RUN: mkdir %t/sub
// RUN: cd %t

// RUN: %target-swift-frontend -emit-bc %s
// RUN: %target-swift-frontend -emit-bc %s -module-name explicit
// RUN: %target-swift-frontend -emit-bc %s -o %t/sub/
// RUN: %target-swift-frontend -emit-bc %s -o %t/sub/ -module-name explicit
// RUN: %target-swift-frontend -emit-bc %s -o %t/explicit.llvmbc
// RUN: %target-swift-frontend -emit-bc %s -o %t/explicit2.llvmbc -module-name explicit2
// RUN: echo | %target-swift-frontend -emit-bc - -o %t/sub -module-name stdin

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
