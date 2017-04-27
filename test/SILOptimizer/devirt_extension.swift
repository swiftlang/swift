// RUN: %target-swift-frontend -O -emit-sil %s | %FileCheck %s

protocol DrawingElementDispatch {}

extension DrawingElementDispatch {
  var boundingBox: Int32 {
    return 0
  }
}

protocol DrawingElement : DrawingElementDispatch {
  var boundingBox: Int32 {get}
}

struct D : DrawingElement {
  var boundingBox: Int32 = 42
}

// Check that boundingBox is devirtualized and inlined.
// CHECK: sil @{{.*}}test1111
// bb0:
// CHECK-NOT: class_method
// CHECK-NOT: witness_method
// CHECK: integer_literal $Builtin.Int32, 42
// CHECK-NOT: class_method
// CHECK-NOT: witness_method
// CHECK-NOT: bb1:
// return
public func test1111() -> Int32 {
  return (D() as DrawingElement).boundingBox
}
