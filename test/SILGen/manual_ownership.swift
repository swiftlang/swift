// RUN: %target-swift-frontend %s -emit-silgen -verify \
// RUN:   -enable-experimental-feature ManualOwnership -o %t.silgen

// RUN: %FileCheck %s --input-file %t.silgen

// REQUIRES: swift_feature_ManualOwnership

class ShapeClass {}
class TriangleClass {
  var shape = ShapeClass()
}

// CHECK-LABEL: sil {{.*}} @basic_access_of_loadable
// CHECK:       bb0(%0 : @guaranteed $TriangleClass):
// CHECK-NEXT:    debug_value %0
// CHECK-NEXT:    [[M:%.*]] = class_method %0, #TriangleClass.shape!getter
// CHECK-NEXT:    [[SHAPE:%.*]] = apply [[M]](%0)
// CHECK-NEXT:    [[B:%.*]] = begin_borrow [[SHAPE]]
// CHECK-NEXT:    [[COPY:%.*]] = explicit_copy_value [[B]]
// CHECK-NEXT:    end_borrow [[B]]
// CHECK-NEXT:    destroy_value [[SHAPE]]
// CHECK-NEXT:    return [[COPY]]
// CHECK-NEXT:  } // end sil function 'basic_access_of_loadable'
@_manualOwnership
@_silgen_name("basic_access_of_loadable")
func basic_access_of_loadable(_ t: TriangleClass) -> ShapeClass {
  return copy t.shape
}
