// RUN: rm -rf %t
// RUN: mkdir %t
// RUN: %swift -emit-module -sil-serialize-all -o %t %S/Inputs/def_noinline.swift
// RUN: llvm-bcanalyzer %t/def_noinline.swiftmodule | FileCheck %s
// RUN: %swift -emit-silgen -sil-link-all -I=%t %s | FileCheck %s -check-prefix=SIL

// CHECK-NOT: UnknownCode

import def_noinline

// SIL-LABEL: sil private @top_level_code : $@thin () -> () {
// SIL: [[RAW:%.+]] = global_addr #raw : $*Bool
// SIL: [[FUNC:%.+]] = function_ref @_TF12def_noinline12testNoinlineFT1xSb_Sb : $@thin (Bool) -> Bool
// SIL: [[RESULT:%.+]] = apply [[FUNC]]({{%.+}}) : $@thin (Bool) -> Bool
// SIL: store [[RESULT]] to [[RAW]] : $*Bool
var raw = testNoinline(x: false)

// SIL: [[FUNC2:%.+]] = function_ref @_TFV12def_noinline18NoInlineInitStructCfMS0_FT1xSb_S0_ : $@thin (Bool, @thin NoInlineInitStruct.Type) -> NoInlineInitStruct
// SIL: apply [[FUNC2]]({{%.+}}, {{%.+}}) : $@thin (Bool, @thin NoInlineInitStruct.Type) -> NoInlineInitStruct

var a = NoInlineInitStruct(x: false)

// SIL: [[FUNC3:%.+]] = function_ref @_TF12def_noinline16testNoinlineLateFT1xSb_Sb : $@thin (Bool) -> Bool
// SIL: [[RESULT:%.+]] = apply [[FUNC3]]({{%.+}}) : $@thin (Bool) -> Bool
var raw2 = testNoinlineLate(x: true)

// SIL: [[FUNC4:%.+]] = function_ref @_TFV12def_noinline20LateInlineInitStructCfMS0_FT1xSb_S0_ : $@thin (Bool, @thin LateInlineInitStruct.Type) -> LateInlineInitStruct
// SIL: apply [[FUNC4]]({{%.+}}, {{%.+}}) : $@thin (Bool, @thin LateInlineInitStruct.Type) -> LateInlineInitStruct
var b = LateInlineInitStruct(x: true)

// SIL-LABEL: [noinline] @_TF12def_noinline12testNoinlineFT1xSb_Sb : $@thin (Bool) -> Bool

// SIL-LABEL: sil public_external [noinline] @_TFV12def_noinline18NoInlineInitStructCfMS0_FT1xSb_S0_ : $@thin (Bool, @thin NoInlineInitStruct.Type) -> NoInlineInitStruct {

// SIL-LABEL: [noinlinesil] @_TF12def_noinline16testNoinlineLateFT1xSb_Sb : $@thin (Bool) -> Bool

// SIL-LABEL: sil public_external [noinlinesil] @_TFV12def_noinline20LateInlineInitStructCfMS0_FT1xSb_S0_ : $@thin (Bool, @thin LateInlineInitStruct.Type) -> LateInlineInitStruct {
