// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t %S/Inputs/def_noinline.swift
// RUN: llvm-bcanalyzer %t/def_noinline.swiftmodule | %FileCheck %s
// RUN: %target-swift-frontend -emit-silgen -sil-link-all -I %t %s | %FileCheck %s -check-prefix=SIL

// CHECK-NOT: UnknownCode

import def_noinline

// SIL-LABEL: sil @main
// SIL: [[RAW:%.+]] = global_addr @$S8noinline3rawSbvp : $*Bool
// SIL: [[FUNC:%.+]] = function_ref @$S12def_noinline12testNoinline1xS2b_tF : $@convention(thin) (Bool) -> Bool
// SIL: [[RESULT:%.+]] = apply [[FUNC]]({{%.+}}) : $@convention(thin) (Bool) -> Bool
// SIL: store [[RESULT]] to [trivial] [[RAW]] : $*Bool
var raw = testNoinline(x: false)

// SIL: [[FUNC2:%.+]] = function_ref @$S12def_noinline18NoInlineInitStructV1xACSb_tcfC : $@convention(method) (Bool, @thin NoInlineInitStruct.Type) -> NoInlineInitStruct
// SIL: apply [[FUNC2]]({{%.+}}, {{%.+}}) : $@convention(method) (Bool, @thin NoInlineInitStruct.Type) -> NoInlineInitStruct

var a = NoInlineInitStruct(x: false)

// SIL-LABEL: [serialized] [noinline] [canonical] @$S12def_noinline12testNoinline1xS2b_tF : $@convention(thin) (Bool) -> Bool

// SIL-LABEL: sil public_external [serialized] [noinline] [canonical] @$S12def_noinline18NoInlineInitStructV1xACSb_tcfC : $@convention(method) (Bool, @thin NoInlineInitStruct.Type) -> NoInlineInitStruct {
