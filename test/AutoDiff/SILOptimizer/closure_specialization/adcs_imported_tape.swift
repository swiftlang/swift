// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %S/../../validation-test/closure_specialization/adcs_imported_tape.swift -D LIBRARY -O -emit-module -o %t/ADCSLibrary.swiftmodule -module-name ADCSLibrary
// RUN: %target-swift-frontend %S/../../validation-test/closure_specialization/adcs_imported_tape.swift -D DEFINITIONS_ONLY -I %t -O -sil-verify-all -emit-sil -o %t/client.sil -module-name ADCSClient
// RUN: %FileCheck %s < %t/client.sil
// RUN: %target-sil-opt -I %t -module-name ADCSClient -enable-sil-verify-all %t/client.sil -o /dev/null
// RUN: %target-swift-frontend %S/../../validation-test/closure_specialization/adcs_imported_tape.swift -D DEFINITIONS_ONLY -I %t -O -sil-verify-all -emit-ir -o %t/client.ll -module-name ADCSClient
// RUN: %FileCheck %s --check-prefix=IR < %t/client.ll
// RUN: %target-swift-frontend %S/../../validation-test/closure_specialization/adcs_imported_tape.swift -D LIBRARY -O -enable-library-evolution -emit-module -o %t/ADCSLibrary.swiftmodule -module-name ADCSLibrary
// RUN: %target-swift-frontend %S/../../validation-test/closure_specialization/adcs_imported_tape.swift -D DEFINITIONS_ONLY -I %t -O -enable-library-evolution -sil-verify-all -emit-sil -o %t/resilient-client.sil -module-name ADCSClient
// RUN: %FileCheck %s < %t/resilient-client.sil
// RUN: %target-swift-frontend %S/../../validation-test/closure_specialization/adcs_imported_tape.swift -D DEFINITIONS_ONLY -I %t -O -enable-library-evolution -sil-verify-all -emit-ir -o %t/resilient-client.ll -module-name ADCSClient
// RUN: %FileCheck %s --check-prefix=IR < %t/resilient-client.ll
// The empty nonprimary file appears first to exercise primary-file selection.
// RUN: touch %t/nonprimary.swift
// RUN: %target-swift-frontend %t/nonprimary.swift -primary-file %S/../../validation-test/closure_specialization/adcs_imported_tape.swift -D DEFINITIONS_ONLY -I %t -O -enable-library-evolution -sil-verify-all -emit-ir -o %t/primary-client.ll -module-name ADCSClient
// RUN: %FileCheck %s --check-prefix=IR < %t/primary-client.ll

// CHECK: @frozen enum {{_AD__[^ ]+_spec[^ ]*}} {
// CHECK: case bb0(())
// CHECK: @frozen enum {{_AD__[^ ]+_spec[^ ]*}} {
// CHECK: case bb0(())
// CHECK: @frozen enum {{_AD__[^ ]+_bb3__Pred__src_0_wrt_0_l_spec[^ ]*}} {
// CHECK: (Float, Float)
// CHECK-LABEL: // specialized pullback of clientInt(_:_:)
// CHECK-NOT: apply {{.*}} : $@callee_guaranteed
// CHECK: } // end sil function
// CHECK-LABEL: // specialized pullback of clientDouble(_:_:)
// CHECK-NOT: apply {{.*}} : $@callee_guaranteed
// CHECK: } // end sil function

// The nominal descriptor belongs to the client even though its synthesized
// name embeds the imported function's name.
// IR-NOT: @"$s11ADCSLibrary{{[0-9]+}}_AD__{{[^"]+}}_spec{{[^"]+}}OMn"
// IR: @"$s10ADCSClient{{[0-9]+}}_AD__{{[^"]+}}_spec{{[^"]+}}OMn" =
// IR-NOT: @"$s11ADCSLibrary{{[0-9]+}}_AD__{{[^"]+}}_spec{{[^"]+}}OMn"
