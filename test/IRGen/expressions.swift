// RUN: %target-swift-frontend -primary-file %s -emit-ir -parse-stdlib -disable-access-control -enable-character-literals | FileCheck %s

// REQUIRES: CPU=x86_64

import Swift

typealias CharacterLiteralType = SillyCharacter

struct SillyCharacter :
    _BuiltinCharacterLiteralConvertible, CharacterLiteralConvertible {

  init(_builtinCharacterLiteral value: Builtin.Int32) { }

  init(characterLiteral value: SillyCharacter) { }
}

// CHECK: private unnamed_addr constant [22 x i8] c"this is just a\0A\0A test\00"

// CHECK: define hidden [[stringLayout:[^@]*]] @_TF11expressions17TestStringLiteralFT_SS() {
// CHECK: call [[stringLayout]] @{{.*}}_builtinStringLiteral{{.*}}(i8* getelementptr inbounds ([22 x i8]* @0, i64 0, i64 0), i64 21, i1 true)

func TestStringLiteral() -> String {
  return "this is just a\n\u{0a} test"
}

// CHECK: define hidden [[stringLayout]] @_TF11expressions18TestStringLiteral2FT_SS() {
// CHECK: call [[stringLayout]] @{{.*}}_builtinUTF16StringLiteral{{.*}}(i8* bitcast ([19 x i16]* @1 to i8*), i64 18)
func TestStringLiteral2() -> String {
  return "non-ASCII string \u{00B5}"
}

// CHECK: @_TF11expressions15TestCharLiteral
func TestCharLiteral(inout a: SillyCharacter) {
  a = '0'  // CHECK: _TFV11expressions14SillyCharacterCfMS0_FT24_builtinCharacterLiteralBi32__S0_{{.*}}(i32 48)
  a = 'a'  // CHECK: _TFV11expressions14SillyCharacterCfMS0_FT24_builtinCharacterLiteralBi32__S0_(i32 97)
  a = '\u{2603}' // CHECK: _TFV11expressions14SillyCharacterCfMS0_FT24_builtinCharacterLiteralBi32__S0_(i32 9731)
  a = '☃' // CHECK: _TFV11expressions14SillyCharacterCfMS0_FT24_builtinCharacterLiteralBi32__S0_(i32 9731)
}
