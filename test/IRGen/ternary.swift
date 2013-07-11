// RUN: %swift -triple x86_64-apple-darwin10 %s -emit-llvm | FileCheck %s

// <rdar://problem/14410061> Check that we can emit address SILBasicBlock
// arguments into phi nodes.
// CHECK: define void @_T7ternary3fooU__FT1xSb1yQ__Q_
func foo<T>(x:Bool, y:T) -> T {
  // CHECK: br i1 {{%.*}}, label [[THEN:%.*]], label [[ELSE:%.*]]
  return x ? y : y
  // CHECK: [[THEN_ADDR:%.*]] = call %swift.opaque* 
  // CHECK: br
  // CHECK: [[ELSE_ADDR:%.*]] = call %swift.opaque* 
  // CHECK: br
  // CHECK: phi %swift.opaque* [ [[THEN_ADDR]], [[THEN]] ], [ [[ELSE_ADDR]], [[ELSE]] ]
}
