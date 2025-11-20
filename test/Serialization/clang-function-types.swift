// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-module -o %t %S/Inputs/def_clang_function_types.swift -use-clang-function-types
// RUN: llvm-bcanalyzer %t/def_clang_function_types.swiftmodule | %FileCheck -check-prefix=CHECK-BCANALYZER %s
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -use-clang-function-types -experimental-print-full-convention -emit-sil -sil-debug-serialization -I %t %s -O | %FileCheck %s

import def_clang_function_types

// CHECK-BCANALYZER-LABEL: (INDEX_BLOCK):
// CHECK-BCANALYZER: CLANG_TYPE_OFFSETS

// CHECK-LABEL: sil hidden @$s4main5test1yyF
func test1() {
  // CHECK: global_addr @$s24def_clang_function_types11has_fp_types13OpaquePointerVSgyXzC32_ZTSPFP21ForwardInTypedefForFPvESgvp : $*Optional<@convention(c, cType: "struct ForwardInTypedefForFP *(*)(void)") () -> Optional<OpaquePointer>>
  let fp = has_fp_type
  _ = fp?()
}
// CHECK-LABEL: } // end sil function '$s4main5test1yyF'

// CHECK-LABEL: sil hidden @$s4main5test2yyF
func test2() {
  use_fp_internally()
}
// CHECK-LABEL: } // end sil function '$s4main5test2yyF'

// CHECK-LABEL: sil public_external [canonical] @$s24def_clang_function_types17use_fp_internallyyyF
// CHECK:         enum $Optional<@convention(c, cType: "struct ForwardInTypedefForFP2 *(*)(void)") () -> Optional<OpaquePointer>>, #Optional.none!enumelt
// CHECK:         [[FN:%.*]] = function_ref @$s24def_clang_function_types9use_inout3argyxz_tlF : $@convention(thin) <τ_0_0> (@inout τ_0_0) -> ()
// CHECK:         apply [[FN]]<Optional<@convention(c, cType: "struct ForwardInTypedefForFP2 *(*)(void)") () -> Optional<OpaquePointer>>>
// CHECK-LABEL: } // end sil function '$s24def_clang_function_types17use_fp_internallyyyF'
