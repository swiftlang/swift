// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %S/Inputs/adcs_imported_tape_library.swift -O -emit-module -o %t/ADCSLibrary.swiftmodule -module-name ADCSLibrary
// RUN: %target-swift-frontend %s -I %t -O -sil-verify-all -emit-ir -sil-output-path %t/client.sil -o %t/client.ll -module-name ADCSClient
// RUN: %FileCheck %s < %t/client.sil
// RUN: %FileCheck %s --check-prefix=IR < %t/client.ll
// RUN: %target-swift-frontend %S/Inputs/adcs_imported_tape_library.swift -O -enable-library-evolution -emit-module -o %t/ADCSLibrary.swiftmodule -module-name ADCSLibrary
// RUN: %target-swift-frontend %s -I %t -O -enable-library-evolution -sil-verify-all -emit-ir -sil-output-path %t/resilient-client.sil -o %t/resilient-client.ll -module-name ADCSClient
// RUN: %FileCheck %s < %t/resilient-client.sil
// RUN: %FileCheck %s --check-prefix=IR < %t/resilient-client.ll
// The empty nonprimary file appears first to exercise primary-file selection.
// RUN: touch %t/nonprimary.swift
// RUN: %target-swift-frontend %t/nonprimary.swift -primary-file %s -I %t -O -enable-library-evolution -sil-verify-all -emit-ir -o %t/primary-client.ll -module-name ADCSClient
// RUN: %FileCheck %s --check-prefix=IR < %t/primary-client.ll

// The client specializes the outer tapes and preserves the imported
// predecessor types with their original generic arguments.
// CHECK:       enum {{_AD__[^ ]+_bb3__Pred__src_0_wrt_0_l_spec[^ ]*}} {
// CHECK-DAG:   case bb1((predecessor: {{_AD__[^ ]+}}_bb1__Pred__src_0_wrt_0_l<Int>, (Float, Float)))
// CHECK-DAG:   case bb1((predecessor: {{_AD__[^ ]+}}_bb1__Pred__src_0_wrt_0_l<Double>, (Float, Float)))
// CHECK-LABEL: {{^}}// specialized pullback of clientInt(_:_:)
// CHECK-NOT:     apply {{.*}} : $@callee_guaranteed
// CHECK:         switch_enum %[[#]], case #[[INT_TAPE:_AD__[^. ]+_spec[^. ]+]].bb2!enumelt: bb[[#]], case #[[INT_TAPE]].bb1!enumelt:
// CHECK-NOT:     apply {{.*}} : $@callee_guaranteed
// CHECK:       } // end sil function
// CHECK-LABEL: {{^}}// specialized pullback of clientDouble(_:_:)
// CHECK-NOT:     apply {{.*}} : $@callee_guaranteed
// CHECK:         switch_enum %[[#]], case #[[DOUBLE_TAPE:_AD__[^. ]+_spec[^. ]+]].bb2!enumelt: bb[[#]], case #[[DOUBLE_TAPE]].bb1!enumelt:
// CHECK-NOT:     apply {{.*}} : $@callee_guaranteed
// CHECK:       } // end sil function

// The nominal descriptor belongs to the client even though its synthesized
// name embeds the imported function's name.
// IR-NOT: @"$s11ADCSLibrary{{[0-9]+}}_AD__{{[^"]+}}_spec{{[^"]+}}OMn"
// IR:     @"$s10ADCSClient{{[0-9]+}}_AD__{{[^"]+}}_spec{{[^"]+}}OMn" =
// IR-NOT: @"$s11ADCSLibrary{{[0-9]+}}_AD__{{[^"]+}}_spec{{[^"]+}}OMn"

import _Differentiation
import ADCSLibrary

@differentiable(reverse, wrt: x)
public func clientInt(_ x: Float, _ flag: Bool) -> Float {
  importedBranch(x, Int(1), flag)
}

@differentiable(reverse, wrt: x)
public func clientDouble(_ x: Float, _ flag: Bool) -> Float {
  importedBranch(x, Double(1), flag)
}
