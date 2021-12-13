// RUN: %target-swift-frontend -emit-silgen -enable-experimental-string-processing %s | %FileCheck %s
// REQUIRES: libswift

var s = 'abc'
// CHECK: [[REGEX_STR_LITERAL:%[0-9]+]] = string_literal utf8 "abc"
// CHECK: [[STRING_INIT:%[0-9]+]] = function_ref @$sSS21_builtinStringLiteral17utf8CodeUnitCount7isASCIISSBp_BwBi1_tcfC : $@convention(method) (Builtin.RawPointer, Builtin.Word, Builtin.Int1, @thin String.Type) -> @owned String
// CHECK: [[REGEX_STR:%[0-9]+]] = apply [[STRING_INIT]]([[REGEX_STR_LITERAL]]
// CHECK: [[REGEX_INIT:%[0-9]+]] = function_ref @$s17_StringProcessing5RegexV06_regexA0ACyxGSS_tcfC : $@convention(method) <τ_0_0> (@owned String, @thin Regex<τ_0_0>.Type) -> @out Regex<τ_0_0>
// CHECK: apply [[REGEX_INIT]]<DynamicCaptures>({{%.+}}, [[REGEX_STR]], {{%.+}}) : $@convention(method) <τ_0_0> (@owned String, @thin Regex<τ_0_0>.Type) -> @out Regex<τ_0_0>
