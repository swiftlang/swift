// RUN: %target-swift-frontend -emit-ir -parse-stdlib -module-name Swift %s | %FileCheck %s

public protocol Equatable {}

// CHECK-LABEL: @"$sytN" = external global %swift.type
// CHECK-LABEL: @"got.$sytN" = private unnamed_addr constant %swift.type* @"$sytN"
// CHECK-LABEL: @"$sytSQsWB" = external global i8*
// CHECK-LABEL: @"got.$sytSQsWB" = private unnamed_addr constant i8** @"$sytSQsWB"
// CHECK-LABEL: @"$sytSQsMb" ={{ dllexport | protected | }}constant %swift.protocol_conformance_descriptor {
// -- protocol descriptor
// CHECK-SAME:           @"$sSQMp"
// -- type metadata
// CHECK-SAME:           @"got.$sytN"
// -- witness table
// CHECK-SAME:           @"got.$sytSQsWB"
// -- flags
// CHECK-SAME:           i32 32 },
