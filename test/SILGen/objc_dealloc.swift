// RUN: rm -rf %t/clang-module-cache
// RUN: %swift -module-cache-path=%t/clang-module-cache -triple x86_64-apple-darwin13 -sdk=%S/Inputs %s -emit-silgen | FileCheck %s

import gizmo

class X { }

func onDestruct() { }

class SwiftGizmo : Gizmo {
  var x = X()

  // CHECK-LABEL: sil @_TToFCSo10SwiftGizmod : $@cc(objc_method) @thin (SwiftGizmo) -> ()
  destructor() {
    // CHECK: bb0([[SELF:%[0-9]+]] : $SwiftGizmo):
    // Call onDestruct()
    // CHECK:   [[ONDESTRUCT_REF:%[0-9]+]] = function_ref @_TF12objc_dealloc10onDestructFT_T_ : $@thin () -> ()
    // CHECK:   [[ONDESTRUCT_RESULT:%[0-9]+]] = apply [[ONDESTRUCT_REF]]() : $@thin () -> ()
    onDestruct()

    // Destroy instance variables
    // FIXME: This should move to .cxx_destruct
    // CHECK:   [[X:%[0-9]+]] = ref_element_addr [[SELF]] : $SwiftGizmo, #x
    // CHECK:   destroy_addr [[X]] : $*X

    // Call super -dealloc.
    // CHECK:   [[SUPER_DEALLOC:%[0-9]+]] = super_method [[SELF]] : $SwiftGizmo, #Gizmo!destroyer.foreign : $@cc(objc_method) @thin (Gizmo) -> ()
    // CHECK:   [[SUPER:%[0-9]+]] = upcast [[SELF]] : $SwiftGizmo to $Gizmo
    // CHECK:   [[SUPER_DEALLOC_RESULT:%[0-9]+]] = apply [[SUPER_DEALLOC]]([[SUPER]]) : $@cc(objc_method) @thin (Gizmo) -> ()
    // CHECK:   [[RESULT:%[0-9]+]] = tuple ()
    // CHECK:   return [[RESULT]] : $()
  }
}
