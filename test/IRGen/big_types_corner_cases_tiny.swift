
// RUN: %target-swift-frontend -assume-parsing-unqualified-ownership-sil -enable-large-loadable-types -primary-file %s %S/big_types_corner_cases.swift -emit-ir | %FileCheck %s --check-prefix=CHECK
// REQUIRES: optimized_stdlib

// DO NOT ADD ANY MORE CODE TO THIS FILE!

// CHECK-LABEL: define internal void @globalinit
// CHECK: [[ALLOC:%.*]] = alloca %T27big_types_corner_cases_tiny30LoadableStructWithBiggerStringV
// CHECK: call swiftcc void {{.*}}(%T27big_types_corner_cases_tiny30LoadableStructWithBiggerStringV* noalias nocapture sret [[ALLOC]]
let model = ClassWithLoadableStructWithBiggerString().f()

