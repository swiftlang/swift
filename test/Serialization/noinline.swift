// RUN: rm -rf %t
// RUN: mkdir %t
// RUN: %target-swift-frontend -emit-module -sil-serialize-all -o %t %S/Inputs/def_noinline.swift
// RUN: llvm-bcanalyzer %t/def_noinline.swiftmodule | FileCheck %s
// RUN: %target-swift-frontend -emit-silgen -sil-link-all -I %t %s | FileCheck %s -check-prefix=SIL

// CHECK-NOT: UnknownCode

import def_noinline

// SIL-LABEL: sil @main
// SIL: [[RAW:%.+]] = global_addr @_Tv8noinline3rawSb : $*Bool
// SIL: [[FUNC:%.+]] = function_ref @_TF12def_noinline12testNoinlineFT1xSb_Sb : $@thin (Bool) -> Bool
// SIL: [[RESULT:%.+]] = apply [[FUNC]]({{%.+}}) : $@thin (Bool) -> Bool
// SIL: store [[RESULT]] to [[RAW]] : $*Bool
var raw = testNoinline(x: false)

// SIL: [[FUNC2:%.+]] = function_ref @_TFV12def_noinline18NoInlineInitStructCfMS0_FT1xSb_S0_ : $@thin (Bool, @thin NoInlineInitStruct.Type) -> NoInlineInitStruct
// SIL: apply [[FUNC2]]({{%.+}}, {{%.+}}) : $@thin (Bool, @thin NoInlineInitStruct.Type) -> NoInlineInitStruct

var a = NoInlineInitStruct(x: false)

// SIL-LABEL: [fragile] [noinline] @_TF12def_noinline12testNoinlineFT1xSb_Sb : $@thin (Bool) -> Bool

// SIL-LABEL: sil public_external [fragile] [noinline] @_TFV12def_noinline18NoInlineInitStructCfMS0_FT1xSb_S0_ : $@thin (Bool, @thin NoInlineInitStruct.Type) -> NoInlineInitStruct {
