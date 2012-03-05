// RUN: %swift -triple x86_64-apple-darwin10 %S/swift.swift -emit-llvm | FileCheck %s

// This test double-checks that swift.swift can be compiled to IR.

// CHECK: define i1 @_TSsop2aaFT3lhsNSs4bool3rhsFT_S__S_(i1, i8*, i8*)
