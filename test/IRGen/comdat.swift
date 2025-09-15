// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-ir %S/Inputs/comdat1.swift %S/Inputs/comdat2.swift -O -num-threads 1 -module-name comdat -o %t/comdat1.ll -o %t/comdat2.ll
// RUN: %FileCheck -check-prefix CHECK-1 %s < %t/comdat1.ll
// RUN: %FileCheck -check-prefix CHECK-2 %s < %t/comdat2.ll

// REQUIRES: OS=windows-msvc

// Ensure that the definition is marked as COMDAT
// CHECK-1: "$s6comdat1C33_{{.*}}LLCMa" = comdat any
// CHECK-1: "$s6comdat1C33_{{.*}}LLCMn" = comdat any

// Ensure that no forward declaration is emitted
// CHECK-2-NOT: "$s6comdat1C33_{{.*}}LLCMa" = comdat any
// CHECK-2-NOT: "$s6comdat1C33_{{.*}}LLCMn" = comdat any
