// RUN: %target-swift-frontend -O -emit-sil %s | FileCheck %s

protocol DrawingElementDispatchType {}

extension DrawingElementDispatchType {
  final var boundingBox: Int32 {
    return 0
  }
}

protocol DrawingElementType : DrawingElementDispatchType {
  var boundingBox: Int32 {get}
}

struct D : DrawingElementType {
  var boundingBox: Int32 = 42
}

// Check that that boundingBox is devirtualized and inlined.
// CHECK: sil @{{.*}}test1111
// bb0:
// CHECK-NOT: class_method
// CHECK-NOT: witness_method
// CHECK: integer_literal $Builtin.Int32, 42
// CHECK-NOT: class_method
// CHECK-NOT: witness_method
// CHECK-NOT: bb1
// return
public func test1111() -> Int32 {
  return (D() as DrawingElementType).boundingBox
}
