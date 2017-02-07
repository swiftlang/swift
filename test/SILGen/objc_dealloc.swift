// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -sdk %S/Inputs -I %S/Inputs -enable-source-import %s -emit-silgen | %FileCheck %s

// REQUIRES: objc_interop

import gizmo

class X { }
func onDestruct() { }

@requires_stored_property_inits
class SwiftGizmo : Gizmo {
  var x = X()

  // CHECK-LABEL: sil hidden [transparent] @_T012objc_dealloc10SwiftGizmoC1xAA1XCvfi : $@convention(thin) () -> @owned X
  // CHECK:      [[FN:%.*]] = function_ref @_T012objc_dealloc1XCACycfC : $@convention(method) (@thick X.Type) -> @owned X
  // CHECK-NEXT: [[METATYPE:%.*]] = metatype $@thick X.Type
  // CHECK-NEXT: [[RESULT:%.*]] = apply [[FN]]([[METATYPE]]) : $@convention(method) (@thick X.Type) -> @owned X
  // CHECK-NEXT: return [[RESULT]] : $X

  // CHECK-LABEL: sil hidden @_T012objc_dealloc10SwiftGizmoC{{[_0-9a-zA-Z]*}}fc
  // CHECK: bb0([[SELF_PARAM:%[0-9]+]] : $SwiftGizmo):
  override init() {
    // CHECK:   [[SELF_UNINIT:%[0-9]+]] = mark_uninitialized [derivedselfonly]
    // CHECK-NOT: ref_element_addr
    // CHECK:   upcast
    // CHECK-NEXT:   super_method
    // CHECK:   return
    super.init()
  }

  // CHECK-LABEL: sil hidden @_T012objc_dealloc10SwiftGizmoCfD : $@convention(method) (@owned SwiftGizmo) -> ()
  deinit {
    // CHECK: bb0([[SELF:%[0-9]+]] : $SwiftGizmo):
    // Call onDestruct()
    // CHECK:   [[ONDESTRUCT_REF:%[0-9]+]] = function_ref @_T012objc_dealloc10onDestructyyF : $@convention(thin) () -> ()
    // CHECK:   [[ONDESTRUCT_RESULT:%[0-9]+]] = apply [[ONDESTRUCT_REF]]() : $@convention(thin) () -> ()
    onDestruct()

    // Note: don't destroy instance variables
    // CHECK-NOT: ref_element_addr

    // Call super -dealloc.
    // CHECK:   [[SUPER_DEALLOC:%[0-9]+]] = super_method [[SELF]] : $SwiftGizmo, #Gizmo.deinit!deallocator.foreign : (Gizmo) -> () -> (), $@convention(objc_method) (Gizmo) -> ()
    // CHECK:   [[SUPER:%[0-9]+]] = upcast [[SELF]] : $SwiftGizmo to $Gizmo
    // CHECK:   [[SUPER_DEALLOC_RESULT:%[0-9]+]] = apply [[SUPER_DEALLOC]]([[SUPER]]) : $@convention(objc_method) (Gizmo) -> ()
    // CHECK:   [[RESULT:%[0-9]+]] = tuple ()
    // CHECK:   return [[RESULT]] : $()
  }

  // Objective-C deallocation deinit thunk (i.e., -dealloc).
  // CHECK-LABEL: sil hidden [thunk] @_T012objc_dealloc10SwiftGizmoCfDTo : $@convention(objc_method) (SwiftGizmo) -> ()
  // CHECK: bb0([[SELF:%[0-9]+]] : $SwiftGizmo):
  // CHECK:   [[SELF_COPY:%.*]] = copy_value [[SELF]]

  // CHECK:   [[GIZMO_DTOR:%[0-9]+]] = function_ref @_T012objc_dealloc10SwiftGizmoCfD : $@convention(method) (@owned SwiftGizmo) -> ()
  // CHECK:   [[RESULT:%[0-9]+]] = apply [[GIZMO_DTOR]]([[SELF_COPY]]) : $@convention(method) (@owned SwiftGizmo) -> ()
  // CHECK:   return [[RESULT]] : $()

  // Objective-C IVar initializer (i.e., -.cxx_construct)
  // CHECK-LABEL: sil hidden @_T012objc_dealloc10SwiftGizmoCfeTo : $@convention(objc_method) (@owned SwiftGizmo) -> @owned SwiftGizmo
  // CHECK: bb0([[SELF_PARAM:%[0-9]+]] : $SwiftGizmo):
  // CHECK-NEXT:   debug_value [[SELF_PARAM]] : $SwiftGizmo, let, name "self"
  // CHECK-NEXT:   [[SELF:%[0-9]+]] = mark_uninitialized [rootself] [[SELF_PARAM]] : $SwiftGizmo
  // CHECK:        [[XINIT:%[0-9]+]] = function_ref @_T012objc_dealloc10SwiftGizmoC1xAA1XCvfi
  // CHECK-NEXT:   [[XOBJ:%[0-9]+]] = apply [[XINIT]]() : $@convention(thin) () -> @owned X
  // CHECK-NEXT:   [[BORROWED_SELF:%.*]] = begin_borrow [[SELF]]
  // CHECK-NEXT:   [[X:%[0-9]+]] = ref_element_addr [[BORROWED_SELF]] : $SwiftGizmo, #SwiftGizmo.x
  // CHECK-NEXT:   assign [[XOBJ]] to [[X]] : $*X
  // CHECK-NEXT:   end_borrow [[BORROWED_SELF]] from [[SELF]]
  // CHECK-NEXT:   return [[SELF]] : $SwiftGizmo

  // Objective-C IVar destroyer (i.e., -.cxx_destruct)
  // CHECK-LABEL: sil hidden @_T012objc_dealloc10SwiftGizmoCfETo : $@convention(objc_method) (SwiftGizmo) -> ()
  // CHECK:      bb0([[SELF:%[0-9]+]] : $SwiftGizmo):
  // CHECK-NEXT:  debug_value [[SELF]] : $SwiftGizmo, let, name "self"
  // CHECK-NEXT: [[X:%[0-9]+]] = ref_element_addr [[SELF]] : $SwiftGizmo, #SwiftGizmo.x
  // CHECK-NEXT: destroy_addr [[X]] : $*X
  // CHECK-NEXT: [[RESULT:%[0-9]+]] = tuple ()
  // CHECK-NEXT: return [[RESULT]] : $()
}

// CHECK-NOT: sil hidden [thunk] @_T0So11SwiftGizmo2CfETo : $@convention(objc_method) (SwiftGizmo2) -> ()
class SwiftGizmo2 : Gizmo {
}
