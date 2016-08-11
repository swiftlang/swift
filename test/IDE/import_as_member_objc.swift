// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -I %t -I %S/Inputs/custom-modules -print-module -source-filename %s -module-to-print=ImportAsMember.Proto -always-argument-labels > %t.printed.Proto.txt

// RUN: %FileCheck %s -check-prefix=PRINT-PROTO -strict-whitespace < %t.printed.Proto.txt

// PRINT-PROTO-LABEL: protocol ImportedProtocolBase : NSObjectProtocol {
// PRINT-PROTO-NEXT:  }
// PRINT-PROTO-NEXT:  typealias ImportedProtocolBase_t = ImportedProtocolBase
// PRINT-PROTO-NEXT:  protocol IAMProto : ImportedProtocolBase {
// PRINT-PROTO-NEXT:  }
// PRINT-PROTO-NEXT:  typealias IAMProto_t = IAMProto
// PRINT-PROTO-NEXT:  extension IAMProto {
// PRINT-PROTO-NEXT:    func mutateSomeState()
// PRINT-PROTO-NEXT:    func mutateSomeState(withParameter _: Int)
// PRINT-PROTO-NEXT:    func mutateSomeState(withFirstParameter _: Int)
// PRINT-PROTO-NEXT:    var someValue: Int32
// PRINT-PROTO-NEXT:  }

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

// RUN: %target-parse-verify-swift -I %S/Inputs/custom-modules
// REQUIRES: objc_interop

import Foundation
import ImportAsMember.Proto
import ImportAsMember.Class
import IAMError

// Errors
ErrorStruct.hasPrototype();

// Protocols
@objc class Foo : NSObject, IAMProto {}

struct Bar : IAMProto {}
  // expected-error@-1{{non-class type 'Bar' cannot conform to class protocol 'IAMProto'}}
  // expected-error@-2{{non-class type 'Bar' cannot conform to class protocol 'ImportedProtocolBase'}}
  // expected-error@-3{{non-class type 'Bar' cannot conform to class protocol 'NSObjectProtocol'}}

@objc class FooErr : NSObject, ErrorProto {}

let foo = Foo()
foo.mutateSomeState()

let fooErr = FooErr()
fooErr.mutateSomeInstanceState()
FooErr.mutateSomeStaticState()
  // expected-error@-1{{type 'FooErr' has no member 'mutateSomeStaticState'}}


// Import into members of an imported, renamed class.
let someClassOpts: SomeClass.Options = .fuzzyDice
let someClass = SomeClass(value: 3.14159)
someClass.applyOptions(someClassOpts)

class SomeSub : UnavailableDefaultInitSub { }

// Handle default initializers.
let udi1 = UnavailableDefaultInit()
let udis1 = UnavailableDefaultInitSub()
