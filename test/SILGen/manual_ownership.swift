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

// CHECK-LABEL: sil {{.*}} [manual_ownership] [ossa] @return_borrowed
// CHECK:       bb0(%0 : @guaranteed $TriangleClass):
// CHECK-NEXT:     debug_value %0
// CHECK-NEXT:     [[IMPL_COPY:%.*]] = copy_value %0
// CHECK-NEXT:     return [[IMPL_COPY]]
// CHECK-NEXT:  } // end sil function 'return_borrowed'
@_manualOwnership
@_silgen_name("return_borrowed")
func return_borrowed(_ t: borrowing TriangleClass) -> TriangleClass {
  return t
}

// CHECK-LABEL: sil {{.*}} [manual_ownership] [ossa] @return_consumingParam
// CHECK:       bb0(%0 : @_eagerMove @owned $TriangleClass):
// CHECK-NEXT:     alloc_box ${ var TriangleClass }, var, name "t"
// CHECK-NEXT:     begin_borrow [var_decl]
// CHECK-NEXT:     [[ADDR:%.*]] = project_box {{.*}}, 0
// CHECK-NEXT:     store %0 to [init] [[ADDR]]
// CHECK-NEXT:     [[ACCESS:%.*]] = begin_access [read] [unknown] [[ADDR]]
// CHECK-NEXT:     [[IMPL_COPY:%.*]] = load [copy] [[ACCESS]]
// CHECK-NEXT:     end_access [[ACCESS]]
// CHECK-NEXT:     end_borrow
// CHECK-NEXT:     destroy_value
// CHECK-NEXT:     return [[IMPL_COPY]]
// CHECK-NEXT:  } // end sil function 'return_consumingParam'
@_manualOwnership
@_silgen_name("return_consumingParam")
func return_consumingParam(_ t: consuming TriangleClass) -> TriangleClass {
  return t
}

// CHECK-LABEL: sil {{.*}} [manual_ownership] [ossa] @return_owned
// CHECK:       bb0(%0 : @owned $TriangleClass):
// CHECK-NEXT:     debug_value %0
// CHECK-NEXT:     [[BORROW:%.*]] = begin_borrow %0
// CHECK-NEXT:     [[IMPL_COPY:%.*]] = copy_value [[BORROW]]
// CHECK-NEXT:     end_borrow [[BORROW]]
// CHECK-NEXT:     destroy_value %0
// CHECK-NEXT:     return [[IMPL_COPY]]
// CHECK-NEXT:  } // end sil function 'return_owned'
@_manualOwnership
@_silgen_name("return_owned")
func return_owned(_ t: __owned TriangleClass) -> TriangleClass {
  return t
}
