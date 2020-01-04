// RUN: %target-swift-frontend -emit-ir -parse-stdlib -module-name Swift %s | %FileCheck %s

public protocol Equatable {}

// CHECK-LABEL: @"$sxd_tSQsMb" ={{ dllexport | protected | }}constant { i32, i32, i32, i32, i16, i16, i32, i32 } {
// -- protocol descriptor
// CHECK-SAME:           @"$sSQMp"
// -- metadata kind
// CHECK-SAME:           i32 769
// -- witness table
// CHECK-SAME:           i32 0
// -- flags
// CHECK-SAME:           i32 131104
// -- witness table size
// CHECK-SAME:           i16 2
// -- private table size & requires init
// CHECK-SAME:           i16 1
// -- generic witness table instantiator
// CHECK-SAME:           i32 0
// -- generic witness table private data
// CHECK-SAME:           [16 x i8*]* @{{.*}}
