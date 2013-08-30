// RUN: rm -rf %t
// RUN: mkdir %t
// RUN: %swift -emit-module -o %t %S/Inputs/def_transparent.swift
// RUN: llvm-bcanalyzer %t/def_transparent.swiftmodule | FileCheck %s
// RUN: %swift -emit-silgen -I=%t %s | FileCheck %s -check-prefix=SIL

// CHECK-NOT: UnknownCode

import def_transparent

// SIL: sil internal @top_level_code : $() -> () {
// SIL: [[RAW:%.+]] = global_addr #raw : $*Bool
// SIL: [[FUNC:%.+]] = function_ref @_T15def_transparent15testTransparentFT1xSb_Sb : $[thin] (x : Bool) -> Bool
// SIL: [[RESULT:%.+]] = apply [transparent] [[FUNC]]({{%.+}}) : $[thin] (x : Bool) -> Bool
// SIL: store [[RESULT]] to [[RAW]] : $*Bool
var raw = testTransparent(false)
