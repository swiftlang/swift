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

// SIL-LABEL: [noinline] @_TF12def_noinline12testNoinlineFT1xSb_Sb : $@thin (Bool) -> Bool
