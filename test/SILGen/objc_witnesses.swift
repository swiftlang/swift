// RUN: %target-swift-frontend -emit-silgen -sdk %S/Inputs -I %S/Inputs -enable-source-import %s | FileCheck %s

// REQUIRES: objc_interop

import Foundation
import gizmo

protocol Fooable {
  func foo() -> String!
}

// Witnesses Fooable.foo with the original ObjC-imported -foo method .
extension Foo: Fooable {}

class Phoûx : NSObject, Fooable {
  @objc func foo() -> String! {
    return "phoûx!"
  }
}

// witness for Foo.foo uses the foreign-to-native thunk:
// CHECK-LABEL: sil hidden [transparent] [thunk] @_TTWCSo3Foo14objc_witnesses7FooableS0_FS1_3fooUS1___fQPS1_FT_GSQSS_
// CHECK:         function_ref @_TTOFCSo3Foo3foofS_FT_GSQSS_

// witness for Phoûx.foo uses the Swift vtable
// CHECK-LABEL: _TTWC14objc_witnessesX8Phox_xraS_7FooableS_FS1_3fooUS1___fQPS1_FT_GSQSS_
// CHECK:         class_method %1 : $Phoûx, #Phoûx.foo!1

protocol Bells {
  init(bellsOn: Int)
}

extension Gizmo : Bells {
}

// CHECK: sil hidden [transparent] [thunk] @_TTWCSo5Gizmo14objc_witnesses5BellsS0_FS1_CUS1___fMQPS1_FT7bellsOnSi_S2_
// CHECK: bb0([[SELF:%[0-9]+]] : $*Gizmo, [[I:%[0-9]+]] : $Int, [[META:%[0-9]+]] : $@thick Gizmo.Type):

// CHECK:   [[INIT:%[0-9]+]] = function_ref @_TFCSo5GizmoCfMS_FT7bellsOnSi_GSQS__ : $@thin (Int, @thick Gizmo.Type) -> @owned ImplicitlyUnwrappedOptional<Gizmo>
// CHECK:   [[IUO_RESULT:%[0-9]+]] = apply [[INIT]]([[I]], [[META]]) : $@thin (Int, @thick Gizmo.Type) -> @owned ImplicitlyUnwrappedOptional<Gizmo>
// CHECK:   [[IUO_RESULT_TEMP:%[0-9]+]] = alloc_stack $ImplicitlyUnwrappedOptional<Gizmo>
// CHECK:   store [[IUO_RESULT]] to [[IUO_RESULT_TEMP]]#1 : $*ImplicitlyUnwrappedOptional<Gizmo>

// CHECK:   [[UNWRAP_FUNC:%[0-9]+]] = function_ref @_TFSs36_getImplicitlyUnwrappedOptionalValueU__FGSQQ__Q_ : $@thin <τ_0_0> (@out τ_0_0, @in ImplicitlyUnwrappedOptional<τ_0_0>) -> ()
// CHECK:   [[UNWRAPPED_RESULT_TEMP:%[0-9]+]] = alloc_stack $Gizmo
// CHECK:   apply [[UNWRAP_FUNC]]<Gizmo>([[UNWRAPPED_RESULT_TEMP]]#1, [[IUO_RESULT_TEMP]]#1) : $@thin <τ_0_0> (@out τ_0_0, @in ImplicitlyUnwrappedOptional<τ_0_0>) -> ()
// CHECK:   [[UNWRAPPED_RESULT:%[0-9]+]] = load [[UNWRAPPED_RESULT_TEMP]]#1 : $*Gizmo
// CHECK:   store [[UNWRAPPED_RESULT]] to [[SELF]] : $*Gizmo
// CHECK:   dealloc_stack [[UNWRAPPED_RESULT_TEMP]]#0 : $*@local_storage Gizmo
// CHECK:   dealloc_stack [[IUO_RESULT_TEMP]]#0 : $*@local_storage ImplicitlyUnwrappedOptional<Gizmo>


