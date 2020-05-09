// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t %S/Inputs/def_noinline.swift
// RUN: llvm-bcanalyzer %t/def_noinline.swiftmodule | %FileCheck %s
// RUN: %target-swift-frontend -emit-sib -I %t %s -o %t/noinline.sib
// RUN: %target-sil-opt -performance-linker %t/noinline.sib -I %t -emit-sorted-sil | %FileCheck %s -check-prefix=SIL

// CHECK-NOT: UnknownCode

import def_noinline

// SIL-LABEL: sil public_external [serialized] [noinline] [canonical] [ossa] @$s12def_noinline12testNoinline1xS2b_tF : $@convention(thin) (Bool) -> Bool {

// SIL-LABEL: sil public_external [serialized] [noinline] [canonical] [ossa] @$s12def_noinline18NoInlineInitStructV1xACSb_tcfC : $@convention(method) (Bool, @thin NoInlineInitStruct.Type) -> NoInlineInitStruct {

// SIL-LABEL: sil @main
// SIL: [[RAW:%.+]] = global_addr @$s8noinline3rawSbvp : $*Bool
// SIL: [[FUNC:%.+]] = function_ref @$s12def_noinline12testNoinline1xS2b_tF : $@convention(thin) (Bool) -> Bool
// SIL: [[RESULT:%.+]] = apply [[FUNC]]({{%.+}}) : $@convention(thin) (Bool) -> Bool
// SIL: store [[RESULT]] to [[RAW]] : $*Bool
var raw = testNoinline(x: false)

// SIL: [[FUNC2:%.+]] = function_ref @$s12def_noinline18NoInlineInitStructV1xACSb_tcfC : $@convention(method) (Bool, @thin NoInlineInitStruct.Type) -> NoInlineInitStruct
// SIL: apply [[FUNC2]]({{%.+}}, {{%.+}}) : $@convention(method) (Bool, @thin NoInlineInitStruct.Type) -> NoInlineInitStruct

var a = NoInlineInitStruct(x: false)
