// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t %S/Inputs/def_always_inline.swift
// RUN: llvm-bcanalyzer %t/def_always_inline.swiftmodule | %FileCheck %s
// RUN: %target-swift-frontend -emit-silgen -sil-link-all -I %t %s | %FileCheck %s -check-prefix=SIL

// CHECK-NOT: UnknownCode

import def_always_inline

// SIL-LABEL: sil @main
// SIL: [[RAW:%.+]] = global_addr @$S13always_inline3rawSbvp : $*Bool
// SIL: [[FUNC:%.+]] = function_ref @$S17def_always_inline16testAlwaysInline1xS2b_tF : $@convention(thin) (Bool) -> Bool
// SIL: [[RESULT:%.+]] = apply [[FUNC]]({{%.+}}) : $@convention(thin) (Bool) -> Bool
// SIL: store [[RESULT]] to [trivial] [[RAW]] : $*Bool
var raw = testAlwaysInline(x: false)

// SIL: [[FUNC2:%.+]] = function_ref @$S17def_always_inline22AlwaysInlineInitStructV1xACSb_tcfC : $@convention(method) (Bool, @thin AlwaysInlineInitStruct.Type) -> AlwaysInlineInitStruct
// SIL: apply [[FUNC2]]({{%.+}}, {{%.+}}) : $@convention(method) (Bool, @thin AlwaysInlineInitStruct.Type) -> AlwaysInlineInitStruct

var a = AlwaysInlineInitStruct(x: false)

// SIL-LABEL: [always_inline] [canonical] @$S17def_always_inline16testAlwaysInline1xS2b_tF : $@convention(thin) (Bool) -> Bool

// SIL-LABEL: sil public_external [serialized] [always_inline] [canonical] @$S17def_always_inline22AlwaysInlineInitStructV1xACSb_tcfC : $@convention(method) (Bool, @thin AlwaysInlineInitStruct.Type) -> AlwaysInlineInitStruct {

