// RUN: %target-swift-ide-test -print-indexed-symbols -source-filename %s | %FileCheck %s

struct CustomInteger: ExpressibleByIntegerLiteral {
  init(integerLiteral: Int) {}
}
struct CustomFloat: ExpressibleByFloatLiteral {
  init(floatLiteral: Double) {}
}
struct CustomBool: ExpressibleByBooleanLiteral {
  init(booleanLiteral: Bool) {}
}
struct CustomNil: ExpressibleByNilLiteral {
  init(nilLiteral: ()) {}
}
struct CustomString: ExpressibleByStringLiteral {
  init(stringLiteral: StaticString) {}
}
struct CustomScalar: ExpressibleByUnicodeScalarLiteral {
  init(unicodeScalarLiteral: Unicode.Scalar) {}
}
struct CustomCharacter: ExpressibleByExtendedGraphemeClusterLiteral {
  init(extendedGraphemeClusterLiteral: Character) {}
}
struct CustomArray: ExpressibleByArrayLiteral {
  init(arrayLiteral: Int...) {}
}
struct CustomDictionary: ExpressibleByDictionaryLiteral {
  init(dictionaryLiteral: (Int, Int)...) {}
}
struct CustomInterpolation: ExpressibleByStringInterpolation {
  init(stringInterpolation: StringInterpolation) {}
  init(stringLiteral: String) {}
}

func customInteger() {
  // CHECK: [[@LINE+2]]:26 | constructor/Swift | init(integerLiteral:) | s:14swift_ide_test13CustomIntegerV14integerLiteralACSi_tcfc | Ref,Call,Impl,RelCall,RelCont |
  // CHECK-NEXT: RelCall,RelCont | function/Swift | customInteger() | s:14swift_ide_test13customIntegeryyF
  let _: CustomInteger = 100
  // CHECK: [[@LINE+2]]:7 | constructor/Swift | init(integerLiteral:) | s:14swift_ide_test13CustomIntegerV14integerLiteralACSi_tcfc | Ref,Call,Impl,RelCall,RelCont |
  // CHECK-NEXT: RelCall,RelCont | function/Swift | customInteger() | s:14swift_ide_test13customIntegeryyF
  _ = 100 as CustomInteger
  // CHECK: [[@LINE+2]]:21 | constructor/Swift | init(integerLiteral:) | s:14swift_ide_test13CustomIntegerV14integerLiteralACSi_tcfc | Ref,Call,Impl,RelCall,RelCont |
  // CHECK-NEXT: RelCall,RelCont | function/Swift | customInteger() | s:14swift_ide_test13customIntegeryyF
  _ = CustomInteger(100)
}
func customFloat() {
  // CHECK: [[@LINE+2]]:24 | constructor/Swift | init(floatLiteral:) | s:14swift_ide_test11CustomFloatV12floatLiteralACSd_tcfc | Ref,Call,Impl,RelCall,RelCont |
  // CHECK-NEXT: RelCall,RelCont | function/Swift | customFloat() | s:14swift_ide_test11customFloatyyF
  let _: CustomFloat = -1.23
  // CHECK: [[@LINE+2]]:7 | constructor/Swift | init(floatLiteral:) | s:14swift_ide_test11CustomFloatV12floatLiteralACSd_tcfc | Ref,Call,Impl,RelCall,RelCont |
  // CHECK-NEXT: RelCall,RelCont | function/Swift | customFloat() | s:14swift_ide_test11customFloatyyF
  _ = -1.23 as CustomFloat
  // CHECK: [[@LINE+2]]:19 | constructor/Swift | init(floatLiteral:) | s:14swift_ide_test11CustomFloatV12floatLiteralACSd_tcfc | Ref,Call,Impl,RelCall,RelCont |
  // CHECK-NEXT: RelCall,RelCont | function/Swift | customFloat() | s:14swift_ide_test11customFloatyyF
  _ = CustomFloat(-1.23)
}
func customBool() {
  // CHECK: [[@LINE+2]]:23 | constructor/Swift | init(booleanLiteral:) | s:14swift_ide_test10CustomBoolV14booleanLiteralACSb_tcfc | Ref,Call,Impl,RelCall,RelCont |
  // CHECK-NEXT: RelCall,RelCont | function/Swift | customBool() | s:14swift_ide_test10customBoolyyF
  let _: CustomBool = true
  // CHECK: [[@LINE+2]]:7 | constructor/Swift | init(booleanLiteral:) | s:14swift_ide_test10CustomBoolV14booleanLiteralACSb_tcfc | Ref,Call,Impl,RelCall,RelCont |
  // CHECK-NEXT: RelCall,RelCont | function/Swift | customBool() | s:14swift_ide_test10customBoolyyF
  _ = false as CustomBool
  // CHECK: [[@LINE+2]]:18 | constructor/Swift | init(booleanLiteral:) | s:14swift_ide_test10CustomBoolV14booleanLiteralACSb_tcfc | Ref,Call,Impl,RelCall,RelCont |
  // CHECK-NEXT: RelCall,RelCont | function/Swift | customBool() | s:14swift_ide_test10customBoolyyF
  _ = CustomBool(true)
}
func customNil() {
  // CHECK: [[@LINE+2]]:22 | constructor/Swift | init(nilLiteral:) | s:14swift_ide_test9CustomNilV10nilLiteralACyt_tcfc | Ref,Call,Impl,RelCall,RelCont |
  // CHECK-NEXT: RelCall,RelCont | function/Swift | customNil() | s:14swift_ide_test9customNilyyF
  let _: CustomNil = nil
  // CHECK: [[@LINE+2]]:7 | constructor/Swift | init(nilLiteral:) | s:14swift_ide_test9CustomNilV10nilLiteralACyt_tcfc | Ref,Call,Impl,RelCall,RelCont |
  // CHECK-NEXT: RelCall,RelCont | function/Swift | customNil() | s:14swift_ide_test9customNilyyF
  _ = nil as CustomNil
}
func customString() {
  // CHECK: [[@LINE+2]]:25 | constructor/Swift | init(stringLiteral:) | s:14swift_ide_test12CustomStringV13stringLiteralACs06StaticE0V_tcfc | Ref,Call,Impl,RelCall,RelCont |
  // CHECK-NEXT: RelCall,RelCont | function/Swift | customString() | s:14swift_ide_test12customStringyyF
  let _: CustomString = "abc"
  // CHECK: [[@LINE+2]]:7 | constructor/Swift | init(stringLiteral:) | s:14swift_ide_test12CustomStringV13stringLiteralACs06StaticE0V_tcfc | Ref,Call,Impl,RelCall,RelCont |
  // CHECK-NEXT: RelCall,RelCont | function/Swift | customString() | s:14swift_ide_test12customStringyyF
  _ = "abc" as CustomString
  // CHECK: [[@LINE+2]]:20 | constructor/Swift | init(stringLiteral:) | s:14swift_ide_test12CustomStringV13stringLiteralACs06StaticE0V_tcfc | Ref,Call,Impl,RelCall,RelCont |
  // CHECK-NEXT: RelCall,RelCont | function/Swift | customString() | s:14swift_ide_test12customStringyyF
  _ = CustomString("abc")
}
func customScalar() {
  // CHECK: [[@LINE+2]]:25 | constructor/Swift | init(unicodeScalarLiteral:) | s:14swift_ide_test12CustomScalarV07unicodeE7LiteralACs7UnicodeO0E0V_tcfc | Ref,Call,Impl,RelCall,RelCont |
  // CHECK-NEXT: RelCall,RelCont | function/Swift | customScalar() | s:14swift_ide_test12customScalaryyF
  let _: CustomScalar = "a"
  // CHECK: [[@LINE+2]]:7 | constructor/Swift | init(unicodeScalarLiteral:) | s:14swift_ide_test12CustomScalarV07unicodeE7LiteralACs7UnicodeO0E0V_tcfc | Ref,Call,Impl,RelCall,RelCont |
  // CHECK-NEXT: RelCall,RelCont | function/Swift | customScalar() | s:14swift_ide_test12customScalaryyF
  _ = "a" as CustomScalar
  // CHECK: [[@LINE+2]]:20 | constructor/Swift | init(unicodeScalarLiteral:) | s:14swift_ide_test12CustomScalarV07unicodeE7LiteralACs7UnicodeO0E0V_tcfc | Ref,Call,Impl,RelCall,RelCont |
  // CHECK-NEXT: RelCall,RelCont | function/Swift | customScalar() | s:14swift_ide_test12customScalaryyF
  _ = CustomScalar("a")
}
func customCharacter() {
  // CHECK: [[@LINE+2]]:28 | constructor/Swift | init(extendedGraphemeClusterLiteral:) | s:14swift_ide_test15CustomCharacterV30extendedGraphemeClusterLiteralACSJ_tcfc | Ref,Call,Impl,RelCall,RelCont |
  // CHECK-NEXT: RelCall,RelCont | function/Swift | customCharacter() | s:14swift_ide_test15customCharacteryyF
  let _: CustomCharacter = "a"
  // CHECK: [[@LINE+2]]:7 | constructor/Swift | init(extendedGraphemeClusterLiteral:) | s:14swift_ide_test15CustomCharacterV30extendedGraphemeClusterLiteralACSJ_tcfc | Ref,Call,Impl,RelCall,RelCont |
  // CHECK-NEXT: RelCall,RelCont | function/Swift | customCharacter() | s:14swift_ide_test15customCharacteryyF
  _ = "a" as CustomCharacter
  // CHECK: [[@LINE+2]]:23 | constructor/Swift | init(extendedGraphemeClusterLiteral:) | s:14swift_ide_test15CustomCharacterV30extendedGraphemeClusterLiteralACSJ_tcfc | Ref,Call,Impl,RelCall,RelCont |
  // CHECK-NEXT: RelCall,RelCont | function/Swift | customCharacter() | s:14swift_ide_test15customCharacteryyF
  _ = CustomCharacter("a")
}
func customArray() {
  // CHECK: [[@LINE+2]]:24 | constructor/Swift | init(arrayLiteral:) | s:14swift_ide_test11CustomArrayV12arrayLiteralACSid_tcfc | Ref,Call,Impl,RelCall,RelCont |
  // CHECK-NEXT: RelCall,RelCont | function/Swift | customArray() | s:14swift_ide_test11customArrayyyF
  let _: CustomArray = [1, 2, 3]
  // CHECK: [[@LINE+2]]:7 | constructor/Swift | init(arrayLiteral:) | s:14swift_ide_test11CustomArrayV12arrayLiteralACSid_tcfc | Ref,Call,Impl,RelCall,RelCont |
  // CHECK-NEXT: RelCall,RelCont | function/Swift | customArray() | s:14swift_ide_test11customArrayyyF
  _ = [1, 2, 3] as CustomArray
}
func customDictionary() {
  // CHECK: [[@LINE+2]]:29 | constructor/Swift | init(dictionaryLiteral:) | s:14swift_ide_test16CustomDictionaryV17dictionaryLiteralACSi_Sitd_tcfc | Ref,Call,Impl,RelCall,RelCont |
  // CHECK-NEXT: RelCall,RelCont | function/Swift | customDictionary() | s:14swift_ide_test16customDictionaryyyF
  let _: CustomDictionary = [1: 1, 2: 2, 3: 3]
  // CHECK: [[@LINE+2]]:7 | constructor/Swift | init(dictionaryLiteral:) | s:14swift_ide_test16CustomDictionaryV17dictionaryLiteralACSi_Sitd_tcfc | Ref,Call,Impl,RelCall,RelCont |
  // CHECK-NEXT: RelCall,RelCont | function/Swift | customDictionary() | s:14swift_ide_test16customDictionaryyyF
  _ = [1: 1, 2: 2, 3: 3] as CustomDictionary
}
func customInterpolation() {
  // CHECK: [[@LINE+2]]:32 | constructor/Swift | init(stringInterpolation:) | s:14swift_ide_test19CustomInterpolationV06stringE0ACs013DefaultStringE0V_tcfc | Ref,Call,Impl,RelCall,RelCont |
  // CHECK-NEXT: RelCall,RelCont | function/Swift | customInterpolation() | s:14swift_ide_test19customInterpolationyyF
  let _: CustomInterpolation = "a\(0)b"
  // CHECK: [[@LINE+2]]:7 | constructor/Swift | init(stringInterpolation:) | s:14swift_ide_test19CustomInterpolationV06stringE0ACs013DefaultStringE0V_tcfc | Ref,Call,Impl,RelCall,RelCont |
  // CHECK-NEXT: RelCall,RelCont | function/Swift | customInterpolation() | s:14swift_ide_test19customInterpolationyyF
  _ = "a\(0)b" as CustomInterpolation
  // CHECK: [[@LINE+2]]:27 | constructor/Swift | init(stringInterpolation:) | s:14swift_ide_test19CustomInterpolationV06stringE0ACs013DefaultStringE0V_tcfc | Ref,Call,Impl,RelCall,RelCont |
  // CHECK-NEXT: RelCall,RelCont | function/Swift | customInterpolation() | s:14swift_ide_test19customInterpolationyyF
  _ = CustomInterpolation("a\(0)b")
}
