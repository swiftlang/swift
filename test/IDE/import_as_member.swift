// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -I %t -I %S/Inputs/custom-modules -print-module -source-filename %s -module-to-print=ImportAsMember.A -always-argument-labels > %t.printed.A.txt
// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -I %t -I %S/Inputs/custom-modules -print-module -source-filename %s -module-to-print=ImportAsMember.B -always-argument-labels > %t.printed.B.txt

// RUN: %FileCheck %s -check-prefix=PRINT -strict-whitespace < %t.printed.A.txt
// RUN: %FileCheck %s -check-prefix=PRINTB -strict-whitespace < %t.printed.B.txt

// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -I %t -I %S/Inputs/custom-modules -print-module -source-filename %s -module-to-print=ImportAsMember.APINotes -swift-version 4 -always-argument-labels | %FileCheck %s -check-prefix=PRINT-APINOTES-3 -strict-whitespace
// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -I %t -I %S/Inputs/custom-modules -print-module -source-filename %s -module-to-print=ImportAsMember.APINotes -swift-version 5 -always-argument-labels | %FileCheck %s -check-prefix=PRINT-APINOTES-4 -strict-whitespace

// RUN: %target-typecheck-verify-swift -I %S/Inputs/custom-modules -verify-ignore-unrelated

// PRINT: struct Struct1 {
// PRINT-NEXT:   init()
// PRINT-NEXT:   init(x x: CDouble, y y: CDouble, z z: CDouble)
// PRINT-NEXT:   var x: CDouble
// PRINT-NEXT:   var y: CDouble
// PRINT-NEXT:   var z: CDouble
// PRINT-NEXT: }

// Make sure the other extension isn't here.
// PRINT-NOT: static var static1: {{C?}}Double

// PRINT:      extension Struct1 {
// PRINT-NEXT:   static var globalVar: {{C?}}Double
// PRINT-NEXT:   init(value value: {{C?}}Double)
// PRINT-NEXT:   init(specialLabel specialLabel: ())
// PRINT-NEXT:   func inverted() -> Struct1
// PRINT-NEXT:   mutating func invert()
// PRINT-NEXT:   func translate(radians radians: {{C?}}Double) -> Struct1
// PRINT-NEXT:   func scale(_ radians: {{C?}}Double) -> Struct1
// PRINT-NEXT:   var radius: {{C?}}Double { get nonmutating set }
// PRINT-NEXT:   var altitude: {{C?}}Double{{$}}
// PRINT-NEXT:   var magnitude: {{C?}}Double { get }
// PRINT-NEXT:   static func staticMethod() -> {{(CInt|Int32)}}
// PRINT-NEXT:   static var property: {{(CInt|Int32)}}
// PRINT-NEXT:   static var getOnlyProperty: {{(CInt|Int32)}} { get }
// PRINT-NEXT:   func selfComesLast(x x: {{C?}}Double)
// PRINT-NEXT:   func selfComesThird(a a: {{(CInt|Int32)}}, b b: {{C?}}Float, x x: {{C?}}Double)
// PRINT-NEXT: }
// PRINT-NOT: static var static1: {{C?}}Double


// Make sure the other extension isn't here.
// PRINTB-NOT: static var globalVar: {{C?}}Double

// PRINTB:      extension Struct1 {
// PRINTB:        static var static1: {{C?}}Double
// PRINTB-NEXT:   static var static2: {{C?}}Float
// PRINTB-NEXT:   init(float value: {{C?}}Float)
// PRINTB-NEXT:   static var zero: Struct1 { get }
// PRINTB-NEXT: }

// PRINTB: var currentStruct1: Struct1

// PRINTB-NOT: static var globalVar: {{C?}}Double

// PRINT-APINOTES-3:      @available(swift, obsoleted: 3, renamed: "Struct1.oldApiNoteVar")
// PRINT-APINOTES-3-NEXT: var IAMStruct1APINoteVar: {{C?}}Double
// PRINT-APINOTES-3:      extension Struct1 {
// PRINT-APINOTES-3-NEXT:   var oldApiNoteVar: {{C?}}Double
// PRINT-APINOTES-3-NEXT:   @available(swift, introduced: 4.2, renamed: "Struct1.oldApiNoteVar")
// PRINT-APINOTES-3-NEXT:   var newApiNoteVar: {{C?}}Double
// PRINT-APINOTES-3-NEXT:   @available(swift, introduced: 4.2, renamed: "IAMStruct1APINoteVarInSwift4")
// PRINT-APINOTES-3-NEXT:   var apiNoteVarInSwift4: {{C?}}Double
// PRINT-APINOTES-3-NEXT:   static func oldApiNoteMethod()
// PRINT-APINOTES-3-NEXT:   @available(swift, introduced: 4.2, renamed: "Struct1.oldApiNoteMethod()")
// PRINT-APINOTES-3-NEXT:   static func newApiNoteMethod()
// PRINT-APINOTES-3-NEXT:   init(oldLabel _: {{(CInt|Int32)}})
// PRINT-APINOTES-3-NEXT:   @available(swift, introduced: 4.2, renamed: "Struct1.init(oldLabel:)")
// PRINT-APINOTES-3-NEXT:   init(newLabel _: {{(CInt|Int32)}})
// PRINT-APINOTES-3-NEXT:   typealias OldApiNoteType = Struct1.NewApiNoteType
// PRINT-APINOTES-3-NEXT:   typealias NewApiNoteType = {{C?}}Double
// PRINT-APINOTES-3-NEXT: }
// PRINT-APINOTES-3-NOT: @available
// PRINT-APINOTES-3:     var IAMStruct1APINoteVarInSwift4: {{C?}}Double
// PRINT-APINOTES-3:     @available(swift, obsoleted: 3, renamed: "Struct1.oldApiNoteMethod()")
// PRINT-APINOTES-3-NEXT: func IAMStruct1APINoteFunction()
// PRINT-APINOTES-3:     @available(swift, obsoleted: 3, renamed: "Struct1.init(oldLabel:)")
// PRINT-APINOTES-3-NEXT: func IAMStruct1APINoteCreateFunction(_ _: {{(CInt|Int32)}}) -> Struct1
// PRINT-APINOTES-3:      @available(swift, obsoleted: 3, renamed: "Struct1.OldApiNoteType")
// PRINT-APINOTES-3-NEXT: typealias IAMStruct1APINoteType = Struct1.OldApiNoteType

// PRINT-APINOTES-4:      @available(swift, obsoleted: 3, renamed: "Struct1.newApiNoteVar")
// PRINT-APINOTES-4-NEXT: var IAMStruct1APINoteVar: {{C?}}Double
// PRINT-APINOTES-4:      extension Struct1 {
// PRINT-APINOTES-4-NEXT:   var newApiNoteVar: {{C?}}Double
// PRINT-APINOTES-4-NEXT:   @available(swift, obsoleted: 4.2, renamed: "Struct1.newApiNoteVar")
// PRINT-APINOTES-4-NEXT:   var oldApiNoteVar: {{C?}}Double
// PRINT-APINOTES-4-NEXT:   var apiNoteVarInSwift4: {{C?}}Double
// PRINT-APINOTES-4-NEXT:   static func newApiNoteMethod()
// PRINT-APINOTES-4-NEXT:   @available(swift, obsoleted: 4.2, renamed: "Struct1.newApiNoteMethod()")
// PRINT-APINOTES-4-NEXT:   static func oldApiNoteMethod()
// PRINT-APINOTES-4-NEXT:   init(newLabel _: {{(CInt|Int32)}})
// PRINT-APINOTES-4-NEXT:   @available(swift, obsoleted: 4.2, renamed: "Struct1.init(newLabel:)")
// PRINT-APINOTES-4-NEXT:   init(oldLabel _: {{(CInt|Int32)}})
// PRINT-APINOTES-4-NEXT:   typealias NewApiNoteType = {{C?}}Double
// PRINT-APINOTES-4-NEXT:   @available(swift, obsoleted: 4.2, renamed: "Struct1.NewApiNoteType")
// PRINT-APINOTES-4-NEXT:   typealias OldApiNoteType = Struct1.NewApiNoteType
// PRINT-APINOTES-4-NEXT: }
// PRINT-APINOTES-4:      @available(swift, obsoleted: 4.2, renamed: "Struct1.apiNoteVarInSwift4")
// PRINT-APINOTES-4-NEXT: var IAMStruct1APINoteVarInSwift4: {{C?}}Double
// PRINT-APINOTES-4:     @available(swift, obsoleted: 3, renamed: "Struct1.newApiNoteMethod()")
// PRINT-APINOTES-4-NEXT: func IAMStruct1APINoteFunction()
// PRINT-APINOTES-4:     @available(swift, obsoleted: 3, renamed: "Struct1.init(newLabel:)")
// PRINT-APINOTES-4-NEXT: func IAMStruct1APINoteCreateFunction(_ _: {{(CInt|Int32)}}) -> Struct1
// PRINT-APINOTES-4:      @available(swift, obsoleted: 3, renamed: "Struct1.NewApiNoteType")
// PRINT-APINOTES-4-NEXT: typealias IAMStruct1APINoteType = Struct1.NewApiNoteType

#if canImport(Foundation)
import Foundation
#endif
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
