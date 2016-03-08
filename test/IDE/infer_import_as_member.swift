// RUN: %target-swift-ide-test(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t -I %S/Inputs/custom-modules) -print-module -source-filename %s -module-to-print=InferImportAsMember -enable-omit-needless-words -always-argument-labels -enable-infer-import-as-member > %t.printed.A.txt
// RUN: FileCheck %s -check-prefix=PRINT -strict-whitespace < %t.printed.A.txt

import InferImportAsMember

// TODO: more cases, eventually exhaustive, as we start inferring the result we
// want


// PRINT-LABEL: struct IAMStruct1 {
// PRINT-NEXT:   var x: Double
// PRINT-NEXT:   var y: Double
// PRINT-NEXT:   var z: Double
// PRINT-NEXT:   init()
// PRINT-NEXT:   init(x x: Double, y y: Double, z z: Double)
// PRINT-NEXT: }

// PRINT-LABEL: extension IAMStruct1 {
// PRINT-NEXT:   static var globalVar: Double

// FIXME: get/setNonProperty as member method

// FIXME: move extra word to argument label
// PRINT-NEXT:   init(in in: IAMStruct1)
// PRINT-NEXT:   init(value value: Double)

// PRINT-NEXT:   func invert() -> IAMStruct1
// PRINT-NEXT:   mutating func invertInPlace()
// PRINT-NEXT:   func rotate(radians radians: Double) -> IAMStruct1
// PRINT-NEXT:   var radius: Double { get nonmutating set }
// PRINT-NEXT:   var altitude: Double
// PRINT-NEXT:   var magnitude: Double

// PRINT-NEXT:   func selfComesLast(x x: Double)
// PRINT-NEXT:   func selfComesThird(a a: Int32, b b: Float, x x: Double)
// PRINT-NEXT:   static func staticMethod() -> Int32
// PRINT-NEXT:   static var staticProperty: Int32
// PRINT-NEXT:   static var staticOnlyProperty: Int32 { get }
// PRINT-NEXT: } 

// FIXME: get the class working

