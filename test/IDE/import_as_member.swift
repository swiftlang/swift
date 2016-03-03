// RUN: %target-swift-ide-test(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t -I %S/Inputs/custom-modules) -print-module -source-filename %s -module-to-print=ImportAsMember -enable-omit-needless-words > %t.printed.txt
// RUN: FileCheck %s -check-prefix=PRINT -strict-whitespace < %t.printed.txt

import ImportAsMember

let iamStruct = IAMStruct1CreateSimple(1)
let i = Struct1.globalVar
let i = IAMStruct1GlobalVar

// PRINT: struct Struct1 {
// PRINT:   var x: Double
// PRINT:   var y: Double
// PRINT:   var z: Double
// PRINT:   init()
// PRINT:   init(x: Double, y: Double, z: Double)
// PRINT:   var globalVar: Int32
// PRINT: }