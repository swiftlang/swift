// RUN: %target-swift-frontend -emit-silgen -enable-bare-slash-regex -disable-availability-checking %s | %FileCheck %s
// REQUIRES: swift_swift_parser

var s = #/abc/#
// CHECK: [[REGEX_STR_LITERAL:%[0-9]+]] = string_literal utf8 "#/abc/#"
// CHECK: [[STRING_INIT:%[0-9]+]] = function_ref @$sSS21_builtinStringLiteral17utf8CodeUnitCount7isASCIISSBp_BwBi1_tcfC : $@convention(method) (Builtin.RawPointer, Builtin.Word, Builtin.Int1, @thin String.Type) -> @owned String
// CHECK: [[REGEX_STR:%[0-9]+]] = apply [[STRING_INIT]]([[REGEX_STR_LITERAL]]

// CHECK: [[VERSION_LITERAL:%[0-9]+]] = integer_literal $Builtin.IntLiteral
// CHECK: [[INT_INIT:%[0-9]+]] = function_ref @$sSi22_builtinIntegerLiteralSiBI_tcfC : $@convention(method) (Builtin.IntLiteral, @thin Int.Type) -> Int
// CHECK: [[VERSION_INT:%[0-9]+]] = apply [[INT_INIT]]([[VERSION_LITERAL]]

// CHECK: [[REGEX_INIT:%[0-9]+]] = function_ref @$s17_StringProcessing5RegexV06_regexA07versionACyxGSS_SitcfC : $@convention(method) <τ_0_0> (@owned String, Int, @thin Regex<τ_0_0>.Type) -> @out Regex<τ_0_0>
// CHECK: apply [[REGEX_INIT]]<{{.+}}>({{%.+}}, [[REGEX_STR]], [[VERSION_INT]], {{%.+}}) : $@convention(method) <τ_0_0> (@owned String, Int, @thin Regex<τ_0_0>.Type) -> @out Regex<τ_0_0>
