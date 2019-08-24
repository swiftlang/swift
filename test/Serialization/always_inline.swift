// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t %S/Inputs/def_always_inline.swift
// RUN: llvm-bcanalyzer %t/def_always_inline.swiftmodule | %FileCheck %s
// RUN: %target-swift-frontend -emit-sib -I %t %s -o %t/always_inline.sib
// RUN: %target-sil-opt -performance-linker %t/always_inline.sib -I %t | %FileCheck %s -check-prefix=SIL

// CHECK-NOT: UnknownCode

import def_always_inline

// SIL-LABEL: sil public_external [serialized] [always_inline] [canonical] @$s17def_always_inline22AlwaysInlineInitStructV1xACSb_tcfC : $@convention(method) (Bool, @thin AlwaysInlineInitStruct.Type) -> AlwaysInlineInitStruct {

// SIL-LABEL: sil public_external [serialized] [always_inline] [canonical] @$s17def_always_inline16testAlwaysInline1xS2b_tF : $@convention(thin) (Bool) -> Bool {

// SIL-LABEL: sil @main
// SIL: [[RAW:%.+]] = global_addr @$s13always_inline3rawSbvp : $*Bool
// SIL: [[FUNC:%.+]] = function_ref @$s17def_always_inline16testAlwaysInline1xS2b_tF : $@convention(thin) (Bool) -> Bool
// SIL: [[RESULT:%.+]] = apply [[FUNC]]({{%.+}}) : $@convention(thin) (Bool) -> Bool
// SIL: store [[RESULT]] to [[RAW]] : $*Bool
var raw = testAlwaysInline(x: false)

// SIL: [[FUNC2:%.+]] = function_ref @$s17def_always_inline22AlwaysInlineInitStructV1xACSb_tcfC : $@convention(method) (Bool, @thin AlwaysInlineInitStruct.Type) -> AlwaysInlineInitStruct
// SIL: apply [[FUNC2]]({{%.+}}, {{%.+}}) : $@convention(method) (Bool, @thin AlwaysInlineInitStruct.Type) -> AlwaysInlineInitStruct
var a = AlwaysInlineInitStruct(x: false)

