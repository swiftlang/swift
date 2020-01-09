// RUN: %target-swift-frontend -emit-ir -parse-stdlib -module-name Swift %s | %FileCheck %s

public protocol Equatable {}

// CHECK-LABEL: @"$sxd_tSQsMb" ={{ dllexport | protected | }}constant %swift.protocol_conformance_descriptor {
// -- protocol descriptor
// CHECK-SAME:           @"$sSQMp"
// -- metadata kind
// CHECK-SAME:           i32 769
// -- witness table
// CHECK-SAME:           i32 0
// -- flags
// CHECK-SAME:           i32 32
