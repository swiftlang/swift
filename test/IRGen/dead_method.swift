// RUN: %target-swift-frontend %s -emit-ir -O | %FileCheck %s --check-prefix=CHECK --check-prefix=CHECK-%target-ptrsize -DINT=i%target-ptrsize

// Test to make sure that methods removed by dead code elimination still appear
// in the vtable in both the nominal type descriptor and class metadata.
//
// We should be dropping the implementation, not the actual entry -- it is still
// there but filled in with a placeholder.

public class Class {
  public init() {}
  public func live() {}
  private func dead() {}
}

// CHECK-LABEL: @"$s11dead_method5ClassCMn" ={{( protected)?}} constant <{{.*}}> <{

// -- metadata accessor
// CHECK-SAME: "$s11dead_method5ClassCMa"

// -- vtable
// CHECK-SAME: %swift.method_descriptor {
// CHECK-SAME: i32 1,
// CHECK-SAME: @"$s11dead_method5ClassCACycfC"
// CHECK-SAME: }

// CHECK-SAME: %swift.method_descriptor {
// CHECK-SAME: i32 16,
// CHECK-SAME: @"$s11dead_method5ClassC4liveyyF"
// CHECK-SAME: }

// CHECK-SAME: %swift.method_descriptor { i32 16, i32 0 }
// CHECK-SAME: }>

// CHECK-LABEL: @"$s11dead_method5ClassCMf" = internal global <{{.*}}> <{

// -- destructor
// CHECK-SAME:   void (%T11dead_method5ClassC*)* @"$s11dead_method5ClassCfD",

// -- value witness table
// CHECK-SAME:   i8** @"$sBoWV",

// -- nominal type descriptor
// CHECK-SAME:   @"$s11dead_method5ClassCMn",

// -- ivar destroyer
// CHECK-SAME:   i8* null,

// -- vtable
// CHECK-SAME:   %T11dead_method5ClassC* (%swift.type*)* @"$s11dead_method5ClassCACycfC",
// CHECK-SAME:   void (%T11dead_method5ClassC*)* @"$s11dead_method5ClassC4liveyyF",
// CHECK-SAME:   i8* bitcast (void ()* @swift_deletedMethodError to i8*)

// CHECK-SAME: }>
