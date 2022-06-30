// RUN: %target-swift-emit-silgen -module-name objc_preconcurrency -sdk %S/Inputs -I %S/Inputs -enable-source-import %s -disable-objc-attr-requires-foundation-module | %FileCheck %s

// REQUIRES: objc_interop

@objc protocol P {
  @preconcurrency @objc optional func f(_ completionHandler: @Sendable @escaping () -> Void)
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
