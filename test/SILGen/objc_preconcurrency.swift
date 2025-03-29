// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -module-name objc_preconcurrency -sdk %S/Inputs -I %S/Inputs -enable-source-import -import-objc-header %S/Inputs/objc_preconcurrency.h %s -disable-objc-attr-requires-foundation-module | %FileCheck %s

// REQUIRES: objc_interop

@objc protocol P {
  @preconcurrency @objc optional func f(_ completionHandler: @Sendable @escaping () -> Void)
  @preconcurrency var sendyHandler: @Sendable () -> Void { get set }
}

@preconcurrency class OldWorld {
  @preconcurrency var handler: (@Sendable () -> Void)?
  @preconcurrency var mainHandler: (@MainActor () -> Void)?
  @preconcurrency var nonOptionalHandler: @Sendable () -> Void = {}
  @preconcurrency var nonOptionalMainHandler: @MainActor () -> Void = {}
}

// CHECK-LABEL: sil hidden [ossa] @$s19objc_preconcurrency19testDynamicDispatch1p17completionHandleryAA1P_p_yyctF
// CHECK: dynamic_method_br
// CHECK: bb{{[0-9]+}}(%{{[0-9]+}} : $@convention(objc_method) (@convention(block) @Sendable () -> (), @opened
func testDynamicDispatch(p: P, completionHandler: @escaping () -> Void) {
  p.f?(completionHandler)

  // CHECK: dynamic_method_br
  // CHECK: bb{{[0-9]+}}(%{{[0-9]+}} : $@convention(objc_method) (@convention(block) @Sendable () -> (), @opened
  let _ = p.f
}

// CHECK-LABEL: sil hidden [ossa] @$s19objc_preconcurrency21testOptionalVarAccessyySo12NSTouchGrassCF
// CHECK:         unchecked_bitwise_cast {{.*}} : $Optional<@Sendable @callee_guaranteed () -> ()> to $Optional<@callee_guaranteed () -> ()>
// CHECK:       } // end sil function '$s19objc_preconcurrency21testOptionalVarAccessyySo12NSTouchGrassCF'
func testOptionalVarAccess(_ grass: NSTouchGrass) {
  grass.cancellationHandler?()
}

// CHECK-LABEL: sil hidden [ossa] @$s19objc_preconcurrency33testOptionalVarAccessPartialApplyyyycSgSo12NSTouchGrassCF
// CHECK:         unchecked_bitwise_cast {{.*}} : $Optional<@Sendable @callee_guaranteed () -> ()> to $Optional<@callee_guaranteed () -> ()>
// CHECK: } // end sil function '$s19objc_preconcurrency33testOptionalVarAccessPartialApplyyyycSgSo12NSTouchGrassCF'
func testOptionalVarAccessPartialApply(_ grass: NSTouchGrass) -> (() -> Void)? {
  let handler = grass.cancellationHandler
  if let unwrapped = handler {
    unwrapped()
  }
  return handler
}

// CHECK-LABEL: sil hidden [ossa] @$s19objc_preconcurrency16testObjCVarWriteyySo12NSTouchGrassCF
// CHECK:         unchecked_bitwise_cast {{.*}} : $Optional<@callee_guaranteed () -> ()> to $Optional<@Sendable @callee_guaranteed () -> ()>
// CHECK:         objc_method {{.*}} : $NSTouchGrass, #NSTouchGrass.cancellationHandler!setter.foreign : (NSTouchGrass) -> ((@Sendable () -> ())?) -> (), $@convention(objc_method) (Optional<@convention(block) @Sendable () -> ()>, NSTouchGrass) -> ()
// CHECK:       } // end sil function '$s19objc_preconcurrency16testObjCVarWriteyySo12NSTouchGrassCF'
func testObjCVarWrite(_ grass: NSTouchGrass) {
  grass.cancellationHandler = {}
}

// the below looks kinda long and wonky, but is expected. for a summary, the steps are:
// 1. objc to native
// 2. Sendable to non-Sendable (major part of this test)
// 3. non-optional to optional
// 4. from non-Sendable to Sendable (major part of this test)
// 5. from native to objc (which involves unwrapping and rewrapping that optional; kinda silly but optimization will clean it up)
//
// CHECK-LABEL: sil hidden [ossa] @$s19objc_preconcurrency22testObjCVarWriteAcrossyySo12NSTouchGrassCF
// CHECK:         [[GET_EXCEPTION:%[0-9]+]] = objc_method {{.*}} : $NSTouchGrass, #NSTouchGrass.exceptionHandler!getter.foreign
// CHECK:         [[SENDABLE_BLOCK:%[0-9]+]] = apply [[GET_EXCEPTION]]({{.*}}) : $@convention(objc_method) (NSTouchGrass) -> @autoreleased @convention(block) @Sendable () -> ()
//                           << step 1 >>
// CHECK:         [[NATIVE_THUNK:%[0-9]+]] = function_ref @$sIeyBh_Iegh_TR : $@convention(thin) @Sendable (@guaranteed @convention(block) @Sendable () -> ()) -> ()
// CHECK:         [[NATIVE_SENDABLE_EXCEPTION:%[0-9]+]] = partial_apply [callee_guaranteed] [[NATIVE_THUNK]]([[SENDABLE_BLOCK]])
//                           << step 2 >>
// CHECK:         [[NATIVE_EXCEPTION:%[0-9]+]] = convert_function [[NATIVE_SENDABLE_EXCEPTION]] : $@Sendable @callee_guaranteed () -> () to $@callee_guaranteed () -> ()
//                           << step 3 >>
// CHECK:         [[OPTIONAL_NATIVE_EXCEPTION:%[0-9]+]] = enum $Optional<@callee_guaranteed () -> ()>, #Optional.some!enumelt, [[NATIVE_EXCEPTION]] : $@callee_guaranteed () -> ()
//                           << step 4 >>
// CHECK:         = unchecked_bitwise_cast [[OPTIONAL_NATIVE_EXCEPTION]] : $Optional<@callee_guaranteed () -> ()> to $Optional<@Sendable @callee_guaranteed () -> ()>
//                           << step 5 >>
// CHECK:         switch_enum {{.*}} : $Optional<@Sendable @callee_guaranteed () -> ()>
//
// CHECK:         bb1({{.*}} : @owned $@Sendable @callee_guaranteed () -> ()):
// CHECK:           init_block_storage_header {{.*}} : $*@block_storage @Sendable @callee_guaranteed () -> ()
// CHECK:       } // end sil function '$s19objc_preconcurrency22testObjCVarWriteAcrossyySo12NSTouchGrassCF'
func testObjCVarWriteAcross(_ grass: NSTouchGrass) {
  grass.cancellationHandler = grass.exceptionHandler // *slaps roof of assignment* this bad boy can fit so much conversion in it!
}

// CHECK-LABEL: sil hidden [ossa] @$s19objc_preconcurrency25testOptionalAssignSetter1yyAA8OldWorldCF
// CHECK:  unchecked_bitwise_cast {{.*}} : $Optional<@callee_guaranteed () -> ()> to $Optional<@Sendable @callee_guaranteed () -> ()>
// CHECK:  #OldWorld.handler!setter : (OldWorld) -> ((@Sendable () -> ())?) -> ()
// CHECK: } // end sil function '$s19objc_preconcurrency25testOptionalAssignSetter1yyAA8OldWorldCF'
func testOptionalAssignSetter1(_ oldWorld: OldWorld) {
  oldWorld.handler = {}
}

// CHECK-LABEL: sil hidden [ossa] @$s19objc_preconcurrency25testOptionalAssignSetter2yyAA8OldWorldCF
// CHECK:  convert_function {{.*}} : $@callee_guaranteed () -> () to $@Sendable @callee_guaranteed () -> ()
// CHECK:  $OldWorld, #OldWorld.nonOptionalHandler!setter : (OldWorld) -> (@escaping @Sendable () -> ()) -> ()
// CHECK: } // end sil function '$s19objc_preconcurrency25testOptionalAssignSetter2yyAA8OldWorldCF'
func testOptionalAssignSetter2(_ oldWorld: OldWorld) {
  oldWorld.nonOptionalHandler = {}
}

// CHECK-LABEL: sil hidden [ossa] @$s19objc_preconcurrency21testMainHandlerWritesyyAA8OldWorldCF
// CHECK:         = unchecked_bitwise_cast {{.*}} : $Optional<@callee_guaranteed () -> ()> to $Optional<@Sendable @callee_guaranteed () -> ()>
// CHECK:         = unchecked_bitwise_cast {{.*}} : $Optional<@Sendable @callee_guaranteed () -> ()> to $Optional<@callee_guaranteed () -> ()>
// CHECK:       } // end sil function '$s19objc_preconcurrency21testMainHandlerWritesyyAA8OldWorldCF'
func testMainHandlerWrites(_ oldWorld: OldWorld) {
  oldWorld.handler = oldWorld.mainHandler
  oldWorld.mainHandler = oldWorld.handler
}

// CHECK-LABEL: sil hidden [ossa] @$s19objc_preconcurrency32testMainHandlerNonOptionalWritesyyAA8OldWorldCF
// CHECK:         = convert_function {{.*}} : $@callee_guaranteed () -> () to $@Sendable @callee_guaranteed () -> ()
// CHECK:         = convert_function {{.*}} : $@Sendable @callee_guaranteed () -> () to $@callee_guaranteed () -> ()
// CHECK:       } // end sil function '$s19objc_preconcurrency32testMainHandlerNonOptionalWritesyyAA8OldWorldCF'
func testMainHandlerNonOptionalWrites(_ oldWorld: OldWorld) {
  oldWorld.nonOptionalHandler = oldWorld.nonOptionalMainHandler
  oldWorld.nonOptionalMainHandler = oldWorld.nonOptionalHandler
}

// CHECK-LABEL: sil hidden [ossa] @$s19objc_preconcurrency15testMixedWritesyyAA8OldWorldCF
//
//                << sendable conversions should be here >>
// CHECK:         = unchecked_bitwise_cast {{.*}} : $Optional<@Sendable @callee_guaranteed () -> ()> to $Optional<@callee_guaranteed () -> ()>
// CHECK:         = convert_function {{.*}} : $@callee_guaranteed () -> () to $@Sendable @callee_guaranteed () -> ()
//
//                << but main actor type mismatches are accepted by SIL >>
// CHECK:         [[NO_MAIN_ACTOR:%[0-9]+]] = partial_apply {{.*}} : $@convention(thin) (@guaranteed @callee_guaranteed () -> @out ()) -> ()
// CHECK:         [[SETTER:%[0-9]+]] = class_method {{.*}} : $OldWorld, #OldWorld.nonOptionalMainHandler!setter : (OldWorld) -> (@escaping @MainActor () -> ()) -> (), $@convention(method) (@owned @callee_guaranteed () -> (), @guaranteed OldWorld) -> ()
// CHECK:         apply [[SETTER]]([[NO_MAIN_ACTOR]]
// CHECK:       } // end sil function '$s19objc_preconcurrency15testMixedWritesyyAA8OldWorldCF'
func testMixedWrites(_ oldWorld: OldWorld) {
  oldWorld.nonOptionalHandler = oldWorld.handler ?? {}
  oldWorld.nonOptionalMainHandler = oldWorld.mainHandler ?? {}
}

func modify(_ v: inout () -> Void) {
  v = {}
}

// CHECK-LABEL: sil hidden [ossa] @$s19objc_preconcurrency15testInoutAccessyySo12NSTouchGrassCF
// CHECK:         [[BEFORE_MODIFY:%[0-9]+]] = convert_function {{.*}} : $@Sendable @callee_guaranteed () -> () to $@callee_guaranteed () -> ()
// CHECK:         store [[BEFORE_MODIFY]] to [init] [[INOUT_ALLOC:%[0-9]+]] : $*@callee_guaranteed () -> ()
// CHECK:         [[MODIFY_FN:%[0-9]+]] = function_ref @$s19objc_preconcurrency6modifyyyyyczF : $@convention(thin) (@inout @callee_guaranteed () -> ()) -> ()
// CHECK:         = apply [[MODIFY_FN]]([[INOUT_ALLOC]])
// CHECK:         [[AFTER_MODIFY:%[0-9]+]] = load [take] [[INOUT_ALLOC]] : $*@callee_guaranteed () -> ()
// CHECK:         convert_function [[AFTER_MODIFY]] : $@callee_guaranteed () -> () to $@Sendable @callee_guaranteed () -> ()
// CHECK:       } // end sil function '$s19objc_preconcurrency15testInoutAccessyySo12NSTouchGrassCF'
func testInoutAccess(_ grass: NSTouchGrass) {
  modify(&grass.exceptionHandler)
}


// CHECK-LABEL: sil hidden [ossa] @$s19objc_preconcurrency21testProtocolVarAccess1pyAA1P_p_tF
// CHECK:         [[BEFORE_MODIFY:%[0-9]+]] = convert_function {{.*}} : $@Sendable @callee_guaranteed () -> () to $@callee_guaranteed () -> ()
// CHECK:         store [[BEFORE_MODIFY]] to [init] [[INOUT_ALLOC:%[0-9]+]] : $*@callee_guaranteed () -> ()
// CHECK:         [[MODIFY_FN:%[0-9]+]] = function_ref @$s19objc_preconcurrency6modifyyyyyczF : $@convention(thin) (@inout @callee_guaranteed () -> ()) -> ()
// CHECK:         = apply [[MODIFY_FN]]([[INOUT_ALLOC]])
// CHECK:         [[AFTER_MODIFY:%[0-9]+]] = load [take] [[INOUT_ALLOC]] : $*@callee_guaranteed () -> ()
// CHECK:         convert_function [[AFTER_MODIFY]] : $@callee_guaranteed () -> () to $@Sendable @callee_guaranteed () -> ()
// CHECK:       } // end sil function '$s19objc_preconcurrency21testProtocolVarAccess1pyAA1P_p_tF'
func testProtocolVarAccess(p: P) {
  modify(&p.sendyHandler)
}
