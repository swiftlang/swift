// RUN: %target-swift-frontend -emit-silgen -enable-experimental-string-processing %s | %FileCheck %s
// REQUIRES: libswift

struct Regex {
  init(_regexString: String) {}
}

var s = 'abc'
// CHECK: [[REGEX_STR_LITERAL:%[0-9]+]] = string_literal utf8 "abc"
// CHECK: [[STRING_INIT:%[0-9]+]] = function_ref @$sSS21_builtinStringLiteral17utf8CodeUnitCount7isASCIISSBp_BwBi1_tcfC : $@convention(method) (Builtin.RawPointer, Builtin.Word, Builtin.Int1, @thin String.Type) -> @owned String
// CHECK: [[REGEX_STR:%[0-9]+]] = apply [[STRING_INIT]]([[REGEX_STR_LITERAL]]
// CHECK: [[REGEX_INIT:%[0-9]+]] = function_ref @$s20regex_literal_silgen5RegexV01_A6StringACSS_tcfC : $@convention(method) (@owned String, @thin Regex.Type) -> Regex
// CHECK: apply [[REGEX_INIT]]([[REGEX_STR]]
