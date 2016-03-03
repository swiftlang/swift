// RUN: %target-swift-ide-test(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t -I %S/Inputs/custom-modules) -print-module -source-filename %s -module-to-print=ImportAsMember -enable-omit-needless-words -always-argument-labels > %t.printed.txt
// RUN: FileCheck %s -check-prefix=PRINT -strict-whitespace < %t.printed.txt

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

// RUN: %target-swift-frontend %s -parse -I %S/Inputs/custom-modules -verify

import ImportAsMember

let iamStructFail = IAMStruct1CreateSimple()
  // expected-error@-1{{use of unresolved identifier 'IAMStruct1CreateSimple'}}
let iamStruct = Struct1(x: 1.0, y: 1.0, z: 1.0)

let gVarFail = IAMStruct1GlobalVar
  // expected-error@-1{{use of unresolved identifier 'IAMStruct1GlobalVar'}}
let gVar = Struct1.globalVar

print("\(gVar)")
