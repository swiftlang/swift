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
// CHECK-LABEL: sil hidden [transparent] [thunk] @_TTWCSo3Foo14objc_witnesses7FooableS0_FS1_3foouRq_S1__fq_FT_GSQSS_
// CHECK:         function_ref @_TTOFCSo3Foo3foofS_FT_GSQSS_

// *NOTE* We have an extra copy here for the time being right
// now. This will change once we teach SILGen how to not emit the
// extra copy.
//
// witness for Phoûx.foo uses the Swift vtable
// CHECK-LABEL: _TFC14objc_witnessesX8Phox_xra3foofS0_FT_GSQSS_
// CHECK:      bb0([[IN_ADDR:%.*]] : 
// CHECK:         [[STACK_SLOT:%.*]] = alloc_stack $Phoûx
// CHECK:         copy_addr [[IN_ADDR]] to [initialization] [[STACK_SLOT]]#1
// CHECK:         [[VALUE:%.*]] = load [[STACK_SLOT]]#1
// CHECK:         class_method [[VALUE]] : $Phoûx, #Phoûx.foo!1

protocol Bells {
  init(bellsOn: Int)
}

extension Gizmo : Bells {
}

// CHECK: sil hidden [transparent] [thunk] @_TTWCSo5Gizmo14objc_witnesses5BellsS0_FS1_CuRq_S1__fMq_FT7bellsOnSi_q_
// CHECK: bb0([[SELF:%[0-9]+]] : $*Gizmo, [[I:%[0-9]+]] : $Int, [[META:%[0-9]+]] : $@thick Gizmo.Type):

// CHECK:   [[INIT:%[0-9]+]] = function_ref @_TFCSo5GizmoCfMS_FT7bellsOnSi_GSQS__ : $@convention(thin) (Int, @thick Gizmo.Type) -> @owned ImplicitlyUnwrappedOptional<Gizmo>
// CHECK:   [[IUO_RESULT:%[0-9]+]] = apply [[INIT]]([[I]], [[META]]) : $@convention(thin) (Int, @thick Gizmo.Type) -> @owned ImplicitlyUnwrappedOptional<Gizmo>
// CHECK:   [[IUO_RESULT_TEMP:%[0-9]+]] = alloc_stack $ImplicitlyUnwrappedOptional<Gizmo>
// CHECK:   store [[IUO_RESULT]] to [[IUO_RESULT_TEMP]]#1 : $*ImplicitlyUnwrappedOptional<Gizmo>

// CHECK:   [[UNWRAP_FUNC:%[0-9]+]] = function_ref @_TFSs36_getImplicitlyUnwrappedOptionalValueurFGSQq__q_ : $@convention(thin) <τ_0_0> (@out τ_0_0, @in ImplicitlyUnwrappedOptional<τ_0_0>) -> ()
// CHECK:   [[UNWRAPPED_RESULT_TEMP:%[0-9]+]] = alloc_stack $Gizmo
// CHECK:   apply [[UNWRAP_FUNC]]<Gizmo>([[UNWRAPPED_RESULT_TEMP]]#1, [[IUO_RESULT_TEMP]]#1) : $@convention(thin) <τ_0_0> (@out τ_0_0, @in ImplicitlyUnwrappedOptional<τ_0_0>) -> ()
// CHECK:   [[UNWRAPPED_RESULT:%[0-9]+]] = load [[UNWRAPPED_RESULT_TEMP]]#1 : $*Gizmo
// CHECK:   store [[UNWRAPPED_RESULT]] to [[SELF]] : $*Gizmo
// CHECK:   dealloc_stack [[UNWRAPPED_RESULT_TEMP]]#0 : $*@local_storage Gizmo
// CHECK:   dealloc_stack [[IUO_RESULT_TEMP]]#0 : $*@local_storage ImplicitlyUnwrappedOptional<Gizmo>

// Test extension of a native @objc class to conform to a protocol with a
// subscript requirement. rdar://problem/20371661

protocol Subscriptable {
  subscript(x: Int) -> AnyObject { get }
}

// CHECK-LABEL: sil hidden [transparent] [thunk] @_TTWCSo7NSArray14objc_witnesses13SubscriptableS0_FS1_g9subscriptFSiPSs9AnyObject_ : $@convention(witness_method) (Int, @in_guaranteed NSArray) -> @owned AnyObject {
// CHECK:         function_ref @_TTOFCSo7NSArrayg9subscriptFSiPSs9AnyObject_ : $@convention(method) (Int, @guaranteed NSArray) -> @owned AnyObject
// CHECK-LABEL: sil shared @_TTOFCSo7NSArrayg9subscriptFSiPSs9AnyObject_ : $@convention(method) (Int, @guaranteed NSArray) -> @owned AnyObject {
// CHECK:         class_method [volatile] %1 : $NSArray, #NSArray.subscript!getter.1.foreign
extension NSArray: Subscriptable {}
