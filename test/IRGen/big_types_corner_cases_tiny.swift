
// RUN: %target-swift-frontend %use_no_opaque_pointers -primary-file %s %S/big_types_corner_cases.swift -emit-ir | %FileCheck %s --check-prefix=CHECK
// RUN: %target-swift-frontend -primary-file %s %S/big_types_corner_cases.swift -emit-ir
// REQUIRES: optimized_stdlib

// DO NOT ADD ANY MORE CODE TO THIS FILE!

// CHECK-LABEL: define internal void @{{.*}}WZ
// CHECK: [[ALLOC:%.*]] = alloca %T27big_types_corner_cases_tiny30LoadableStructWithBiggerStringV
// CHECK: call swiftcc void {{.*}}(%T27big_types_corner_cases_tiny30LoadableStructWithBiggerStringV* noalias nocapture sret({{.*}}) [[ALLOC]]
let model = ClassWithLoadableStructWithBiggerString().f()

