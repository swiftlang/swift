// RUN: rm -rf %t
// RUN: mkdir %t
// RUN: %swift -emit-module -sil-serialize-all -o %t %S/Inputs/def_always_inline.swift
// RUN: llvm-bcanalyzer %t/def_always_inline.swiftmodule | FileCheck %s
// RUN: %swift -emit-silgen -sil-link-all -I=%t %s | FileCheck %s -check-prefix=SIL

// CHECK-NOT: UnknownCode

import def_always_inline

// SIL-LABEL: sil private @top_level_code : $@thin () -> () {
// SIL: [[RAW:%.+]] = sil_global_addr @_Tv13always_inline3rawSb : $*Bool
// SIL: [[FUNC:%.+]] = function_ref @_TF17def_always_inline16testAlwaysInlineFT1xSb_Sb : $@thin (Bool) -> Bool
// SIL: [[RESULT:%.+]] = apply [[FUNC]]({{%.+}}) : $@thin (Bool) -> Bool
// SIL: store [[RESULT]] to [[RAW]] : $*Bool
var raw = testAlwaysInline(x: false)

// SIL: [[FUNC2:%.+]] = function_ref @_TFV17def_always_inline22AlwaysInlineInitStructCfMS0_FT1xSb_S0_ : $@thin (Bool, @thin AlwaysInlineInitStruct.Type) -> AlwaysInlineInitStruct
// SIL: apply [[FUNC2]]({{%.+}}, {{%.+}}) : $@thin (Bool, @thin AlwaysInlineInitStruct.Type) -> AlwaysInlineInitStruct

var a = AlwaysInlineInitStruct(x: false)

// SIL-LABEL: [always_inline] @_TF17def_always_inline16testAlwaysInlineFT1xSb_Sb : $@thin (Bool) -> Bool

// SIL-LABEL: sil public_external [always_inline] @_TFV17def_always_inline22AlwaysInlineInitStructCfMS0_FT1xSb_S0_ : $@thin (Bool, @thin AlwaysInlineInitStruct.Type) -> AlwaysInlineInitStruct {

