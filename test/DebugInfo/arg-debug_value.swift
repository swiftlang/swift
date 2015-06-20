// RUN: %target-swift-frontend -primary-file %s -emit-ir -g -o - | FileCheck %s

// Verify that arguments described by debug_value intrinsics are only
// emitted once.
var g = 1

class Foo {
	var x: Int
  // CHECK: define {{.*}}_TFC4main3FoocfMS0_FT_S0_
  // CHECK: entry:
  // CHECK-NEXT: %[[SELF:.*]] = alloca
  // CHECK-NEXT: store %C4main3Foo* %0, %C4main3Foo** %[[SELF]]
  // CHECK-NEXT: call void @llvm.dbg.declare({{.*}}%[[SELF]]
	init () { x = g++ }
}
