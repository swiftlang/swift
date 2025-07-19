// RUN: %swift_frontend_plain -emit-silgen %s -I %S/Inputs -o - -parse-as-library -verify -enable-experimental-feature ImportMacroAliases | %FileCheck %s

// REQUIRES: swift_feature_ImportMacroAliases

import Variadic

public func f() {
  withVaList([0.0]) {
    u_vformatMessage(0, $0)
  }
}

            // closure #1 in f()
// CHECK: bb0(%0 : $*(), %1 : $CVaListPointer):
            // function_ref u_vformatMessage.getter
// CHECK: [[PROJECTION:%.*]] = function_ref @$sSC16u_vformatMessageyys5Int32V_s14CVaListPointerVSgtcvg : $@convention(thin) () -> @owned @callee_guaranteed (Int32, Optional<CVaListPointer>) -> ()
// CHECK: [[FUNCTION:%.*]] = apply [[PROJECTION]]() : $@convention(thin) () -> @owned @callee_guaranteed (Int32, Optional<CVaListPointer>) -> ()
// CHECK: [[VALIST:%.*]] = enum $Optional<CVaListPointer>, #Optional.some!enumelt, %1
// CHECK: [[BORROW:%.*]] = begin_borrow [[FUNCTION]]
// CHECK: {{.*}} = apply [[BORROW]]({{.*}}, [[VALIST]]) : $@callee_guaranteed (Int32, Optional<CVaListPointer>) -> ()
