// RUN: %swift -target x86_64-apple-macosx10.9 %s -emit-ir -parse-stdlib -enable-character-literals | FileCheck %s

import Swift

typealias CharacterLiteralType = SillyCharacter

struct SillyCharacter :
    _BuiltinCharacterLiteralConvertible, CharacterLiteralConvertible {

  static func _convertFromBuiltinCharacterLiteral(
    value: Builtin.Int32
  ) -> SillyCharacter {
    return SillyCharacter()
  }

  static func convertFromCharacterLiteral(value: SillyCharacter) -> SillyCharacter {
    return value
  }
}

// CHECK: private unnamed_addr constant [22 x i8] c"this is just a\0A\0A test\00"

// CHECK: define [[stringLayout:[^@]*]] @_TF11expressions17TestStringLiteralFT_SS() {
// CHECK: call [[stringLayout]] @_TFSS32_convertFromBuiltinStringLiteral{{.*}}(i8* getelementptr inbounds ([22 x i8]* @0, i64 0, i64 0), i64 21, i1 true)

func TestStringLiteral() -> String {
  return "this is just a\n\u{0a} test"
}

// CHECK: define [[stringLayout]] @_TF11expressions18TestStringLiteral2FT_SS() {
// CHECK: call [[stringLayout]] @_TFSS37_convertFromBuiltinUTF16StringLiteral{{.*}}(i8* bitcast ([19 x i16]* @1 to i8*), i64 18)
func TestStringLiteral2() -> String {
  return "non-ASCII string \u{00B5}"
}

// CHECK: @_TF11expressions15TestCharLiteral
func TestCharLiteral(inout a: SillyCharacter) {
  a = '0'  // CHECK: _TFV11expressions14SillyCharacter35_convertFromBuiltinCharacterLiteral{{.*}}(i32 48)
  a = 'a'  // CHECK: _TFV11expressions14SillyCharacter35_convertFromBuiltinCharacterLiteral{{.*}}(i32 97)
  a = '\u{2603}' // CHECK: _TFV11expressions14SillyCharacter35_convertFromBuiltinCharacterLiteral{{.*}}(i32 9731)
  a = '☃' // CHECK: _TFV11expressions14SillyCharacter35_convertFromBuiltinCharacterLiteral{{.*}}(i32 9731)
}
