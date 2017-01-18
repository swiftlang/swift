// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -I %t -I %S/Inputs/custom-modules -print-module -source-filename %s -module-to-print=ImportAsMember.Class -always-argument-labels > %t.printed.Class.txt

// RUN: %FileCheck %s -check-prefix=PRINT-CLASS -strict-whitespace < %t.printed.Class.txt

// PRINT-CLASS-LABEL: class SomeClass : NSObject {
// PRINT-CLASS-NEXT:   init()
// PRINT-CLASS-NEXT: }
// PRINT-CLASS-NEXT: extension SomeClass {
// PRINT-CLASS-NEXT:   /*not inherited*/ init(value x: Double)
// PRINT-CLASS-NEXT:   func applyOptions(_ options: SomeClass.Options)
// PRINT-CLASS-NEXT:   struct Options : OptionSet {
// PRINT-CLASS-NEXT:     init(rawValue rawValue: Int)
// PRINT-CLASS-NEXT:     let rawValue: Int
// PRINT-CLASS-NEXT:     static var fuzzyDice: SomeClass.Options { get }
// PRINT-CLASS-NEXT:     static var spoiler: SomeClass.Options { get }
// PRINT-CLASS-NEXT:   }
// PRINT-CLASS-NEXT: }

// RUN: %target-typecheck-verify-swift -I %S/Inputs/custom-modules
// REQUIRES: objc_interop

import Foundation
import ImportAsMember.Class
import IAMError

// Errors
ErrorStruct.hasPrototype();

// Import into members of an imported, renamed class.
let someClassOpts: SomeClass.Options = .fuzzyDice
let someClass = SomeClass(value: 3.14159)
someClass.applyOptions(someClassOpts)

class SomeSub : UnavailableDefaultInitSub { }

// Handle default initializers.
let udi1 = UnavailableDefaultInit()
let udis1 = UnavailableDefaultInitSub()
