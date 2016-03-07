// RUN: %target-swift-ide-test(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t -I %S/Inputs/custom-modules) -print-module -source-filename %s -module-to-print=InferImportAsMember -enable-omit-needless-words -always-argument-labels -enable-infer-import-as-member > %t.printed.A.txt
// RUN: FileCheck %s -check-prefix=PRINT -strict-whitespace < %t.printed.A.txt

import InferImportAsMember

// TODO: more cases, eventually exhaustive, as we start inferring the result we
// want

// PRINT-LABEL: extension IAMStruct1 {

// FIXME: global var

// FIXME: move "Copy" onto argument label
// PRINT-NEXT:   init(in in: IAMStruct1)

// FIXME: move "Simple" onto argument label
// PRINT-NEXT:   init(value value: Double)

// FIXME: lower-camel-case these
// PRINT-NEXT:   func Invert() -> IAMStruct1
// PRINT-NEXT:   mutating func InvertInPlace()
// PRINT-NEXT:   func Rotate(radians radians: Double) -> IAMStruct1
// PRINT-NEXT:   var Radius: Double { get nonmutating set }
// PRINT-NEXT:   var Altitude: Double
// PRINT-NEXT:   func GetMagnitude() -> Double
// PRINT-NEXT:   func SelfComesLast(x x: Double)
// PRINT-NEXT:   func SelfComesThird(a a: Int32, b b: Float, x x: Double)
// PRINT-NEXT: }

// FIXME: get the class working...