// RUN: %target-swift-emit-silgen -module-name objc_preconcurrency -sdk %S/Inputs -I %S/Inputs -enable-source-import -import-objc-header %S/Inputs/objc_preconcurrency.h %s -disable-objc-attr-requires-foundation-module | %FileCheck %s

// REQUIRES: objc_interop

@objc protocol P {
  @preconcurrency @objc optional func f(_ completionHandler: @Sendable @escaping () -> Void)
  @preconcurrency var sendyHandler: @Sendable () -> Void { get set }
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
// CHECK:         unchecked_addr_cast {{.*}} : $*Optional<@Sendable @callee_guaranteed () -> ()> to $*Optional<@callee_guaranteed () -> ()>
// CHECK:       } // end sil function '$s19objc_preconcurrency21testOptionalVarAccessyySo12NSTouchGrassCF'
func testOptionalVarAccess(_ grass: NSTouchGrass) {
  grass.cancellationHandler?()
}

func modify(_ v: inout () -> Void) {
  v = {}
}

// CHECK-LABEL: sil hidden [ossa] @$s19objc_preconcurrency15testInoutAccessyySo12NSTouchGrassCF
// CHECK:         unchecked_addr_cast {{.*}} : $*@Sendable @callee_guaranteed () -> () to $*@callee_guaranteed () -> ()
// CHECK:       } // end sil function '$s19objc_preconcurrency15testInoutAccessyySo12NSTouchGrassCF'
func testInoutAccess(_ grass: NSTouchGrass) {
  modify(&grass.exceptionHandler)
}


// CHECK-LABEL: sil hidden [ossa] @$s19objc_preconcurrency21testProtocolVarAccess1pyAA1P_p_tF
// CHECK:         unchecked_addr_cast {{.*}} : $*@Sendable @callee_guaranteed () -> () to $*@callee_guaranteed () -> ()
// CHECK:       } // end sil function '$s19objc_preconcurrency21testProtocolVarAccess1pyAA1P_p_tF'
func testProtocolVarAccess(p: P) {
  modify(&p.sendyHandler)
}
