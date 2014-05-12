// RUN: %swift -target x86_64-apple-darwin10 %s -emit-ir -parse-stdlib | FileCheck %s

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

// CHECK: private unnamed_addr constant [22 x i16] [i16 116, i16 104, i16 105, i16 115, i16 32, i16 105, i16 115, i16 32, i16 106, i16 117, i16 115, i16 116, i16 32, i16 97, i16 10, i16 10, i16 32, i16 116, i16 101, i16 115, i16 116, i16 0]

// CHECK: define [[stringLayout:[^@]*]] @_TF11expressions17TestStringLiteralFT_SS() {
// CHECK: call [[stringLayout]] @_TFSS37_convertFromBuiltinUTF16StringLiteral{{.*}}(i8* bitcast ([22 x i16]* @0 to i8*), i64 21)

func TestStringLiteral() -> String {
  return "this is just a\n\x0a test"
}

// CHECK: define [[stringLayout]] @_TF11expressions18TestStringLiteral2FT_SS() {
// CHECK: call [[stringLayout]] @_TFSS37_convertFromBuiltinUTF16StringLiteral{{.*}}(i8* bitcast ([19 x i16]* @1 to i8*), i64 18)
func TestStringLiteral2() -> String {
  return "non-ASCII string \u00B5"
}

// CHECK: @_TF11expressions15TestCharLiteral
func TestCharLiteral(inout a: SillyCharacter) {
  a = '0'  // CHECK: _TFV11expressions14SillyCharacter35_convertFromBuiltinCharacterLiteral{{.*}}(i32 48)
  a = 'a'  // CHECK: _TFV11expressions14SillyCharacter35_convertFromBuiltinCharacterLiteral{{.*}}(i32 97)
  a = '\u2603' // CHECK: _TFV11expressions14SillyCharacter35_convertFromBuiltinCharacterLiteral{{.*}}(i32 9731)
  a = '☃' // CHECK: _TFV11expressions14SillyCharacter35_convertFromBuiltinCharacterLiteral{{.*}}(i32 9731)
}
