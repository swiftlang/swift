// RUN: %target-swift-emit-silgen -sdk %S/Inputs -I %S/Inputs -enable-source-import %s -enable-sil-ownership -enable-objc-interop | %FileCheck %s

import gizmo

class X { }
func onDestruct() { }

@requires_stored_property_inits
class SwiftGizmo : Gizmo {
  var x = X()

  // CHECK-LABEL: sil hidden [transparent] @$S12objc_dealloc10SwiftGizmoC1xAA1XCvpfi : $@convention(thin) () -> @owned X
  // CHECK:      [[METATYPE:%.*]] = metatype $@thick X.Type
  // CHECK:      [[FN:%.*]] = function_ref @$S12objc_dealloc1XCACycfC : $@convention(method) (@thick X.Type) -> @owned X
  // CHECK-NEXT: [[RESULT:%.*]] = apply [[FN]]([[METATYPE]]) : $@convention(method) (@thick X.Type) -> @owned X
  // CHECK-NEXT: return [[RESULT]] : $X

  // CHECK-LABEL: sil hidden @$S12objc_dealloc10SwiftGizmoC{{[_0-9a-zA-Z]*}}fc
  // CHECK: bb0([[SELF_PARAM:%[0-9]+]] : @owned $SwiftGizmo):
  override init() {
    // CHECK:   [[SELF_BOX:%.*]] = alloc_box ${ var SwiftGizmo }, let, name "self"
    // CHECK:   [[SELF_UNINIT:%.*]] = mark_uninitialized [derivedselfonly] [[SELF_BOX]] : ${ var SwiftGizmo }
    // CHECK:   [[SELF_ADDR:%.*]] = project_box [[SELF_UNINIT]]
    // CHECK-NOT: ref_element_addr
    // CHECK:   [[SELF:%.*]] = load [take] [[SELF_ADDR]]
    // CHECK-NEXT:   [[UPCAST_SELF:%.*]] = upcast [[SELF]] : $SwiftGizmo to $Gizmo
    // CHECK-NEXT:   [[BORROWED_UPCAST_SELF:%.*]] = begin_borrow [[UPCAST_SELF]]
    // CHECK-NEXT:   [[DOWNCAST_BORROWED_UPCAST_SELF:%.*]] = unchecked_ref_cast [[BORROWED_UPCAST_SELF]] : $Gizmo to $SwiftGizmo
    // CHECK-NEXT:   objc_super_method [[DOWNCAST_BORROWED_UPCAST_SELF]] : $SwiftGizmo
    // CHECK-NEXT:   end_borrow [[BORROWED_UPCAST_SELF]] from [[UPCAST_SELF]]
    // CHECK:   return
    super.init()
  }

  // CHECK-LABEL: sil hidden @$S12objc_dealloc10SwiftGizmoCfD : $@convention(method) (@owned SwiftGizmo) -> ()
  deinit {
    // CHECK: bb0([[SELF:%[0-9]+]] : @owned $SwiftGizmo):
    // Call onDestruct()
    // CHECK:   [[ONDESTRUCT_REF:%[0-9]+]] = function_ref @$S12objc_dealloc10onDestructyyF : $@convention(thin) () -> ()
    // CHECK:   [[ONDESTRUCT_RESULT:%[0-9]+]] = apply [[ONDESTRUCT_REF]]() : $@convention(thin) () -> ()
    onDestruct()

    // Note: don't destroy instance variables
    // CHECK-NOT: ref_element_addr

    // Call super -dealloc.
    // CHECK:   [[SUPER_DEALLOC:%[0-9]+]] = objc_super_method [[SELF]] : $SwiftGizmo, #Gizmo.deinit!deallocator.1.foreign : (Gizmo) -> () -> (), $@convention(objc_method) (Gizmo) -> ()
    // CHECK:   [[SUPER:%[0-9]+]] = upcast [[SELF]] : $SwiftGizmo to $Gizmo
    // CHECK:   [[SUPER_DEALLOC_RESULT:%[0-9]+]] = apply [[SUPER_DEALLOC]]([[SUPER]]) : $@convention(objc_method) (Gizmo) -> ()
    // CHECK:   end_lifetime [[SUPER]]
    // CHECK:   [[RESULT:%[0-9]+]] = tuple ()
    // CHECK:   return [[RESULT]] : $()
  }

  // Objective-C deallocation deinit thunk (i.e., -dealloc).
  // CHECK-LABEL: sil hidden [thunk] @$S12objc_dealloc10SwiftGizmoCfDTo : $@convention(objc_method) (SwiftGizmo) -> ()
  // CHECK: bb0([[SELF:%[0-9]+]] : @unowned $SwiftGizmo):
  // CHECK:   [[SELF_COPY:%.*]] = copy_value [[SELF]]

  // CHECK:   [[GIZMO_DTOR:%[0-9]+]] = function_ref @$S12objc_dealloc10SwiftGizmoCfD : $@convention(method) (@owned SwiftGizmo) -> ()
  // CHECK:   [[RESULT:%[0-9]+]] = apply [[GIZMO_DTOR]]([[SELF_COPY]]) : $@convention(method) (@owned SwiftGizmo) -> ()
  // CHECK:   return [[RESULT]] : $()

  // Objective-C IVar initializer (i.e., -.cxx_construct)
  // CHECK-LABEL: sil hidden @$S12objc_dealloc10SwiftGizmoCfeTo : $@convention(objc_method) (@owned SwiftGizmo) -> @owned SwiftGizmo
  // CHECK: bb0([[SELF_PARAM:%[0-9]+]] : @owned $SwiftGizmo):
  // CHECK-NEXT:   debug_value [[SELF_PARAM]] : $SwiftGizmo, let, name "self"
  // CHECK-NEXT:   [[SELF:%[0-9]+]] = mark_uninitialized [rootself] [[SELF_PARAM]] : $SwiftGizmo
  // CHECK:        [[XINIT:%[0-9]+]] = function_ref @$S12objc_dealloc10SwiftGizmoC1xAA1XCvpfi
  // CHECK-NEXT:   [[XOBJ:%[0-9]+]] = apply [[XINIT]]() : $@convention(thin) () -> @owned X
  // CHECK-NEXT:   [[BORROWED_SELF:%.*]] = begin_borrow [[SELF]]
  // CHECK-NEXT:   [[X:%[0-9]+]] = ref_element_addr [[BORROWED_SELF]] : $SwiftGizmo, #SwiftGizmo.x
  // CHECK-NEXT:   [[WRITE:%.*]] = begin_access [modify] [dynamic] [[X]] : $*X
  // CHECK-NEXT:   assign [[XOBJ]] to [[WRITE]] : $*X
  // CHECK-NEXT:   end_access [[WRITE]] : $*X
  // CHECK-NEXT:   end_borrow [[BORROWED_SELF]] from [[SELF]]
  // CHECK-NEXT:   return [[SELF]] : $SwiftGizmo

  // Objective-C IVar destroyer (i.e., -.cxx_destruct)
  // CHECK-LABEL: sil hidden @$S12objc_dealloc10SwiftGizmoCfETo : $@convention(objc_method) (SwiftGizmo) -> ()
  // CHECK:      bb0([[SELF:%[0-9]+]] : @unowned $SwiftGizmo):
  // CHECK-NEXT: debug_value [[SELF]] : $SwiftGizmo, let, name "self"
  // CHECK-NEXT: [[SELF_BORROW:%.*]] = begin_borrow [[SELF]]
  // CHECK-NEXT: [[X:%[0-9]+]] = ref_element_addr [[SELF_BORROW]] : $SwiftGizmo, #SwiftGizmo.x
  // CHECK-NEXT: destroy_addr [[X]] : $*X
  // CHECK-NEXT: end_borrow [[SELF_BORROW]] from [[SELF]]
  // CHECK-NEXT: [[RESULT:%[0-9]+]] = tuple ()
  // CHECK-NEXT: return [[RESULT]] : $()
}

// CHECK-NOT: sil hidden [thunk] @$SSo11SwiftGizmo2CfETo : $@convention(objc_method) (SwiftGizmo2) -> ()
class SwiftGizmo2 : Gizmo {
}
