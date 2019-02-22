// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -parse-as-library -swift-version 4 %s -verify
// RUN: not %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -parse-as-library -swift-version 4 %s -disable-nskeyedarchiver-diagnostics 2>&1 | %FileCheck -check-prefix CHECK-NO-DIAGS %s

// RUN: not %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -parse-as-library -swift-version 4 %s -dump-ast > %t.ast
// RUN: %FileCheck %s < %t.ast

// REQUIRES: objc_interop

// CHECK-NO-DIAGS-NOT: NSCoding
// CHECK-NO-DIAGS-NOT: unstable

import Foundation

// Top-level classes
// CHECK-NOT: class_decl{{.*}}"CodingA"{{.*}}@_staticInitializeObjCMetadata
class CodingA : NSObject, NSCoding {
  required init(coder: NSCoder) { }
  func encode(coder: NSCoder) { }
  
}   // okay

// Nested classes
extension CodingA {
  // CHECK-NOT: class_decl{{.*}}"NestedA"{{.*}}@_staticInitializeObjCMetadata
  class NestedA : NSObject, NSCoding { // expected-error{{nested class 'CodingA.NestedA' has an unstable name when archiving via 'NSCoding'}}
    // expected-note@-1{{for compatibility with existing archives, use '@objc' to record the Swift 3 runtime name}}{{3-3=@objc(_TtCC8nscoding7CodingA7NestedA)}}
    // expected-note@-2{{for new classes, use '@objc' to specify a unique, prefixed Objective-C runtime name}}{{3-3=@objc(<#prefixed Objective-C class name#>)}}
    required init(coder: NSCoder) { }
    func encode(coder: NSCoder) { }
  }

  class NestedB : NSObject {
    // expected-note@-1{{for compatibility with existing archives, use '@objc' to record the Swift 3 runtime name}}{{3-3=@objc(_TtCC8nscoding7CodingA7NestedB)}}
    // expected-note@-2{{for new classes, use '@objc' to specify a unique, prefixed Objective-C runtime name}}{{3-3=@objc(<#prefixed Objective-C class name#>)}}
    required init(coder: NSCoder) { }
    func encode(coder: NSCoder) { }
  }

  // CHECK-NOT: class_decl{{.*}}"NestedC"{{.*}}@_staticInitializeObjCMetadata
  @objc(CodingA_NestedC)
  class NestedC : NSObject, NSCoding {
    required init(coder: NSCoder) { }
    func encode(coder: NSCoder) { }
  }

  // CHECK-NOT: class_decl{{.*}}"NestedD"{{.*}}@_staticInitializeObjCMetadata
  @objc(CodingA_NestedD)
  class NestedD : NSObject {
    required init(coder: NSCoder) { }
    func encode(coder: NSCoder) { }
  }
}

extension CodingA.NestedB: NSCoding { // expected-error{{nested class 'CodingA.NestedB' has an unstable name when archiving via 'NSCoding'}}
}

extension CodingA.NestedD: NSCoding { // okay
}

// Generic classes
// CHECK-NOT: class_decl{{.*}}"CodingB"{{.*}}@_staticInitializeObjCMetadata
class CodingB<T> : NSObject, NSCoding {
  required init(coder: NSCoder) { }
  func encode(coder: NSCoder) { }
}

extension CodingB {
  class NestedA : NSObject, NSCoding {
    required init(coder: NSCoder) { }
    func encode(coder: NSCoder) { }
  }
}

// Fileprivate classes.
// CHECK-NOT: class_decl{{.*}}"CodingC"{{.*}}@_staticInitializeObjCMetadata
fileprivate class CodingC : NSObject, NSCoding { // expected-error{{fileprivate class 'CodingC' has an unstable name when archiving via 'NSCoding'}}
  // expected-note@-1{{for compatibility with existing archives, use '@objc' to record the Swift 3 runtime name}}{{1-1=@objc(_TtC8nscodingP33_0B4E7641C0BD1F170280EEDD0D0C1F6C7CodingC)}}
  // expected-note@-2{{for new classes, use '@objc' to specify a unique, prefixed Objective-C runtime name}}{{1-1=@objc(<#prefixed Objective-C class name#>)}}
  required init(coder: NSCoder) { }
  func encode(coder: NSCoder) { }
}

// Private classes
private class CodingD : NSObject, NSCoding { // expected-error{{private class 'CodingD' has an unstable name when archiving via 'NSCoding'}}
  // expected-note@-1{{for compatibility with existing archives, use '@objc' to record the Swift 3 runtime name}}{{1-1=@objc(_TtC8nscodingP33_0B4E7641C0BD1F170280EEDD0D0C1F6C7CodingD)}}
  // expected-note@-2{{for new classes, use '@objc' to specify a unique, prefixed Objective-C runtime name}}{{1-1=@objc(<#prefixed Objective-C class name#>)}}
  required init(coder: NSCoder) { }
  func encode(coder: NSCoder) { }
}

// Local classes.
func someFunction() {
  class LocalCoding : NSObject, NSCoding { // expected-error{{local class 'LocalCoding' has an unstable name when archiving via 'NSCoding'}}
  // expected-note@-1{{for compatibility with existing archives, use '@objc' to record the Swift 3 runtime name}}{{3-3=@objc(_TtCF8nscoding12someFunctionFT_T_L_11LocalCoding)}}
  // expected-note@-2{{for new classes, use '@objc' to specify a unique, prefixed Objective-C runtime name}}{{3-3=@objc(<#prefixed Objective-C class name#>)}}
  required init(coder: NSCoder) { }
  func encode(coder: NSCoder) { }
}
}

// Inherited conformances.
// CHECK-NOT: class_decl{{.*}}"CodingE"{{.*}}@_staticInitializeObjCMetadata
class CodingE<T> : CodingB<T> {
  required init(coder: NSCoder) { super.init(coder: coder) }
  override func encode(coder: NSCoder) { }
}

// @objc suppressions
@objc(TheCodingF)
fileprivate class CodingF : NSObject, NSCoding {
  required init(coder: NSCoder) { }
  func encode(coder: NSCoder) { }
}

@objc(TheCodingG)
private class CodingG : NSObject, NSCoding {
  required init(coder: NSCoder) { }
  func encode(coder: NSCoder) { }
}

extension CodingB {
  // CHECK-NOT: class_decl{{.*}}"GenericViaScope"{{.*}}@_staticInitializeObjCMetadata
  @objc(GenericViaScope) // expected-error {{generic subclasses of '@objc' classes cannot have an explicit '@objc' because they are not directly visible from Objective-C}}
  class GenericViaScope : NSObject { }
}

// Inference of @_staticInitializeObjCMetadata.
// CHECK-NOT: class_decl{{.*}}"SubclassOfCodingA"{{.*}}@_staticInitializeObjCMetadata
class SubclassOfCodingA : CodingA { }

// CHECK: class_decl{{.*}}"SubclassOfCodingE"{{.*}}@_staticInitializeObjCMetadata
class SubclassOfCodingE : CodingE<Int> { }

// Do not warn when simply inheriting from classes that conform to NSCoding.
// The subclass may never be serialized. But do still infer static
// initialization, just in case.
// CHECK-NOT: class_decl{{.*}}"PrivateSubclassOfCodingA"{{.*}}@_staticInitializeObjCMetadata
private class PrivateSubclassOfCodingA : CodingA { }
// CHECK: class_decl{{.*}}"PrivateSubclassOfCodingE"{{.*}}@_staticInitializeObjCMetadata
private class PrivateSubclassOfCodingE : CodingE<Int> { }

// But do warn when inherited through a protocol.
protocol AlsoNSCoding : NSCoding {}
private class CodingH : NSObject, AlsoNSCoding { // expected-error{{private class 'CodingH' has an unstable name when archiving via 'NSCoding'}}
  // expected-note@-1{{for compatibility with existing archives, use '@objc' to record the Swift 3 runtime name}}{{1-1=@objc(_TtC8nscodingP33_0B4E7641C0BD1F170280EEDD0D0C1F6C7CodingH)}}
  // expected-note@-2{{for new classes, use '@objc' to specify a unique, prefixed Objective-C runtime name}}{{1-1=@objc(<#prefixed Objective-C class name#>)}}
  required init(coder: NSCoder) { }
  func encode(coder: NSCoder) { }
}

@NSKeyedArchiverClassName( "abc" ) // expected-error {{@NSKeyedArchiverClassName has been removed; use @objc instead}} {{2-26=objc}} {{28-29=}} {{32-33=}}
class OldArchiverAttribute: NSObject {}

@NSKeyedArchiverEncodeNonGenericSubclassesOnly // expected-error {{@NSKeyedArchiverEncodeNonGenericSubclassesOnly is no longer necessary}} {{1-48=}}
class OldArchiverAttributeGeneric<T>: NSObject {}

// Don't allow one to write @_staticInitializeObjCMetadata!
@_staticInitializeObjCMetadata // expected-error{{unknown attribute '_staticInitializeObjCMetadata'}}
class DontAllowStaticInits { }
