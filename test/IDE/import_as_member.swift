// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -I %t -I %S/Inputs/custom-modules -print-module -source-filename %s -module-to-print=ImportAsMember.A -always-argument-labels > %t.printed.A.txt
// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -I %t -I %S/Inputs/custom-modules -print-module -source-filename %s -module-to-print=ImportAsMember.B -always-argument-labels > %t.printed.B.txt

// RUN: %FileCheck %s -check-prefix=PRINT -strict-whitespace < %t.printed.A.txt
// RUN: %FileCheck %s -check-prefix=PRINTB -strict-whitespace < %t.printed.B.txt

// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -I %t -I %S/Inputs/custom-modules -print-module -source-filename %s -module-to-print=ImportAsMember.APINotes -swift-version 3 -always-argument-labels | %FileCheck %s -check-prefix=PRINT-APINOTES-3 -implicit-check-not "not inherited" -strict-whitespace
// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -I %t -I %S/Inputs/custom-modules -print-module -source-filename %s -module-to-print=ImportAsMember.APINotes -swift-version 4 -always-argument-labels | %FileCheck %s -check-prefix=PRINT-APINOTES-4 -implicit-check-not "not inherited" -strict-whitespace

// RUN: %target-typecheck-verify-swift -I %S/Inputs/custom-modules

// PRINT: struct Struct1 {
// PRINT-NEXT:   var x: Double
// PRINT-NEXT:   var y: Double
// PRINT-NEXT:   var z: Double
// PRINT-NEXT:   init()
// PRINT-NEXT:   init(x x: Double, y y: Double, z z: Double)
// PRINT-NEXT: }

// Make sure the other extension isn't here.
// PRINT-NOT: static var static1: Double

// PRINT:      extension Struct1 {
// PRINT-NEXT:   static var globalVar: Double
// PRINT-NEXT:   init(value value: Double)
// PRINT-NEXT:   init(specialLabel specialLabel: ())
// PRINT-NEXT:   func inverted() -> Struct1
// PRINT-NEXT:   mutating func invert()
// PRINT-NEXT:   func translate(radians radians: Double) -> Struct1
// PRINT-NEXT:   func scale(_ radians: Double) -> Struct1
// PRINT-NEXT:   var radius: Double { get nonmutating set }
// PRINT-NEXT:   var altitude: Double{{$}}
// PRINT-NEXT:   var magnitude: Double { get }
// PRINT-NEXT:   static func staticMethod() -> Int32
// PRINT-NEXT:   static var property: Int32
// PRINT-NEXT:   static var getOnlyProperty: Int32 { get }
// PRINT-NEXT:   func selfComesLast(x x: Double)
// PRINT-NEXT:   func selfComesThird(a a: Int32, b b: Float, x x: Double)
// PRINT-NEXT: }
// PRINT-NOT: static var static1: Double


// Make sure the other extension isn't here.
// PRINTB-NOT: static var globalVar: Double

// PRINTB:      extension Struct1 {
// PRINTB:        static var static1: Double
// PRINTB-NEXT:   static var static2: Float
// PRINTB-NEXT:   init(float value: Float)
// PRINTB-NEXT:   static var zero: Struct1 { get }
// PRINTB-NEXT: }

// PRINTB: var currentStruct1: Struct1

// PRINTB-NOT: static var globalVar: Double

// PRINT-APINOTES-3:      @available(swift, obsoleted: 3, renamed: "Struct1.oldApiNoteVar")
// PRINT-APINOTES-3-NEXT: var IAMStruct1APINoteVar: Double
// PRINT-APINOTES-3:      extension Struct1 {
// PRINT-APINOTES-3-NEXT:   var oldApiNoteVar: Double
// PRINT-APINOTES-3-NEXT:   @available(swift, introduced: 4, renamed: "Struct1.oldApiNoteVar")
// PRINT-APINOTES-3-NEXT:   var newApiNoteVar: Double
// PRINT-APINOTES-3-NEXT:   @available(swift, introduced: 4, renamed: "IAMStruct1APINoteVarInSwift4")
// PRINT-APINOTES-3-NEXT:   var apiNoteVarInSwift4: Double
// PRINT-APINOTES-3-NEXT:   static func oldApiNoteMethod()
// PRINT-APINOTES-3-NEXT:   @available(swift, introduced: 4, renamed: "Struct1.oldApiNoteMethod()")
// PRINT-APINOTES-3-NEXT:   static func newApiNoteMethod()
// PRINT-APINOTES-3-NEXT:   init(oldLabel _: Int32)
// PRINT-APINOTES-3-NEXT:   @available(swift, introduced: 4, renamed: "Struct1.init(oldLabel:)")
// PRINT-APINOTES-3-NEXT:   init(newLabel _: Int32)
// PRINT-APINOTES-3-NEXT:   typealias OldApiNoteType = Double
// PRINT-APINOTES-3-NEXT:   @available(swift, introduced: 4, renamed: "Struct1.OldApiNoteType")
// PRINT-APINOTES-3-NEXT:   typealias NewApiNoteType = Struct1.OldApiNoteType
// PRINT-APINOTES-3-NEXT: }
// PRINT-APINOTES-3-NOT: @available
// PRINT-APINOTES-3:     var IAMStruct1APINoteVarInSwift4: Double
// PRINT-APINOTES-3:     @available(swift, obsoleted: 3, renamed: "Struct1.oldApiNoteMethod()")
// PRINT-APINOTES-3-NEXT: func IAMStruct1APINoteFunction()
// PRINT-APINOTES-3:     @available(swift, obsoleted: 3, renamed: "Struct1.init(oldLabel:)")
// PRINT-APINOTES-3-NEXT: func IAMStruct1APINoteCreateFunction(_ _: Int32) -> Struct1
// PRINT-APINOTES-3:      @available(swift, obsoleted: 3, renamed: "Struct1.OldApiNoteType")
// PRINT-APINOTES-3-NEXT: typealias IAMStruct1APINoteType = Struct1.OldApiNoteType

// PRINT-APINOTES-4:      @available(swift, obsoleted: 3, renamed: "Struct1.newApiNoteVar")
// PRINT-APINOTES-4-NEXT: var IAMStruct1APINoteVar: Double
// PRINT-APINOTES-4:      extension Struct1 {
// PRINT-APINOTES-4-NEXT:   var newApiNoteVar: Double
// PRINT-APINOTES-4-NEXT:   @available(swift, obsoleted: 4, renamed: "Struct1.newApiNoteVar")
// PRINT-APINOTES-4-NEXT:   var oldApiNoteVar: Double
// PRINT-APINOTES-4-NEXT:   var apiNoteVarInSwift4: Double
// PRINT-APINOTES-4-NEXT:   static func newApiNoteMethod()
// PRINT-APINOTES-4-NEXT:   @available(swift, obsoleted: 4, renamed: "Struct1.newApiNoteMethod()")
// PRINT-APINOTES-4-NEXT:   static func oldApiNoteMethod()
// PRINT-APINOTES-4-NEXT:   init(newLabel _: Int32)
// PRINT-APINOTES-4-NEXT:   @available(swift, obsoleted: 4, renamed: "Struct1.init(newLabel:)")
// PRINT-APINOTES-4-NEXT:   init(oldLabel _: Int32)
// PRINT-APINOTES-4-NEXT:   typealias NewApiNoteType = Double
// PRINT-APINOTES-4-NEXT:   @available(swift, obsoleted: 4, renamed: "Struct1.NewApiNoteType")
// PRINT-APINOTES-4-NEXT:   typealias OldApiNoteType = Struct1.NewApiNoteType
// PRINT-APINOTES-4-NEXT: }
// PRINT-APINOTES-4:      @available(swift, obsoleted: 4, renamed: "Struct1.apiNoteVarInSwift4")
// PRINT-APINOTES-4-NEXT: var IAMStruct1APINoteVarInSwift4: Double
// PRINT-APINOTES-4:     @available(swift, obsoleted: 3, renamed: "Struct1.newApiNoteMethod()")
// PRINT-APINOTES-4-NEXT: func IAMStruct1APINoteFunction()
// PRINT-APINOTES-4:     @available(swift, obsoleted: 3, renamed: "Struct1.init(newLabel:)")
// PRINT-APINOTES-4-NEXT: func IAMStruct1APINoteCreateFunction(_ _: Int32) -> Struct1
// PRINT-APINOTES-4:      @available(swift, obsoleted: 3, renamed: "Struct1.NewApiNoteType")
// PRINT-APINOTES-4-NEXT: typealias IAMStruct1APINoteType = Struct1.NewApiNoteType

import ImportAsMember.A
import ImportAsMember.B
import ImportAsMember.APINotes

let iamStructFail = IAMStruct1CreateSimple()
  // expected-error@-1{{missing argument for parameter #1 in call}}
var iamStruct = Struct1(x: 1.0, y: 1.0, z: 1.0)

let gVarFail = IAMStruct1GlobalVar
  // expected-error@-1{{IAMStruct1GlobalVar' has been renamed to 'Struct1.globalVar'}}
let gVar = Struct1.globalVar
print("\(gVar)")

let iamStructInitFail = IAMStruct1CreateSimple(42)
  // expected-error@-1{{'IAMStruct1CreateSimple' has been replaced by 'Struct1.init(value:)'}}
let iamStructInitFail2 = Struct1(value: 42)

let gVar2 = Struct1.static2

// Instance properties
iamStruct.radius += 1.5
_ = iamStruct.magnitude

// Static properties
iamStruct = Struct1.zero

// Global properties
currentStruct1.x += 1.5
