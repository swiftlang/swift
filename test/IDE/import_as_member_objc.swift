// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -I %t -I %S/Inputs/custom-modules -print-module -source-filename %s -module-to-print=ImportAsMember.Proto -always-argument-labels > %t.printed.Proto.txt

// RUN: FileCheck %s -check-prefix=PRINT-PROTO -strict-whitespace < %t.printed.Proto.txt

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

// RUN: %target-parse-verify-swift -I %S/Inputs/custom-modules
// REQUIRES: objc_interop

import Foundation
import ImportAsMember.Proto
import IAMError

// Errors
ErrorStruct.hasPrototype();
ErrorStruct.nonPrototype();
  // expected-error@-1{{type 'ErrorStruct' has no member 'nonPrototype'}}

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


