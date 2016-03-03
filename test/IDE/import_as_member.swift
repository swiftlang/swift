// RUN: %target-swift-ide-test(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t -I %S/Inputs/custom-modules) -print-module -source-filename %s -module-to-print=ImportAsMember -enable-omit-needless-words -always-argument-labels > %t.printed.txt
// RUN: FileCheck %s -check-prefix=PRINT -strict-whitespace < %t.printed.txt

import ImportAsMember

let iamStruct = IAMStruct1CreateSimple(1)
let i = Struct1.globalVar
let i = IAMStruct1GlobalVar

// PRINT: struct Struct1 {
// PRINT-NEXT:   var x: Double
// PRINT-NEXT:   var y: Double
// PRINT-NEXT:   var z: Double
// PRINT-NEXT:   init()
// PRINT-NEXT:   init(x x: Double, y y: Double, z z: Double)
// PRINT-NEXT: }
// PRINT-NEXT: extension Struct1 {
// PRINT-NEXT:   static var globalVar: Int32
// PRINT-NEXT: }

