// RUN: rm -rf %t
// RUN: mkdir -p %t
// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -emit-module -sil-serialize-all -o %t %S/Inputs/def_noinline.swift
// RUN: llvm-bcanalyzer %t/def_noinline.swiftmodule | %FileCheck %s
// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -emit-silgen -sil-link-all -I %t %s | %FileCheck %s -check-prefix=SIL

// CHECK-NOT: UnknownCode

import def_noinline

// SIL-LABEL: sil @main
// SIL: [[RAW:%.+]] = global_addr @_T08noinline3rawSbv : $*Bool
// SIL: [[FUNC:%.+]] = function_ref @_T012def_noinline12testNoinlineSbSb1x_tF : $@convention(thin) (Bool) -> Bool
// SIL: [[RESULT:%.+]] = apply [[FUNC]]({{%.+}}) : $@convention(thin) (Bool) -> Bool
// SIL: store [[RESULT]] to [trivial] [[RAW]] : $*Bool
var raw = testNoinline(x: false)

// SIL: [[FUNC2:%.+]] = function_ref @_T012def_noinline18NoInlineInitStructVACSb1x_tcfC : $@convention(method) (Bool, @thin NoInlineInitStruct.Type) -> NoInlineInitStruct
// SIL: apply [[FUNC2]]({{%.+}}, {{%.+}}) : $@convention(method) (Bool, @thin NoInlineInitStruct.Type) -> NoInlineInitStruct

var a = NoInlineInitStruct(x: false)

// SIL-LABEL: [fragile] [noinline] @_T012def_noinline12testNoinlineSbSb1x_tF : $@convention(thin) (Bool) -> Bool

// SIL-LABEL: sil public_external [fragile] [noinline] @_T012def_noinline18NoInlineInitStructVACSb1x_tcfC : $@convention(method) (Bool, @thin NoInlineInitStruct.Type) -> NoInlineInitStruct {
