// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -parse-as-library -swift-version 4 %s -enable-nskeyedarchiver-diagnostics -verify
// RUN: not %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -parse-as-library -swift-version 4 %s 2>&1 | %FileCheck -check-prefix CHECK-NO-DIAGS %s

// RUN: not %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -parse-as-library -swift-version 4 %s -dump-ast 2> %t.ast
// RUN: %FileCheck %s < %t.ast

// REQUIRES: objc_interop

// CHECK-NO-DIAGS-NOT: NSCoding
// CHECK-NO-DIAGS-NOT: unstable

import Foundation

// Top-level classes
class CodingA : NSObject, NSCoding {
  required init(coder: NSCoder) { }
  func encode(coder: NSCoder) { }
  
}   // okay

// Nested classes
extension CodingA {
  class NestedA : NSObject, NSCoding { // expected-error{{nested class 'CodingA.NestedA' has an unstable name when archiving via 'NSCoding'}}
    // expected-note@-1{{for new classes, add '@objc' to specify a unique, prefixed Objective-C runtime name}}{{3-3=@objc(<#Objective-C class name#>)}}
    // expected-note@-2{{for compatibility with existing archives, use '@NSKeyedArchiverClassName' to record the Swift 3 mangled name}}{{3-3=@NSKeyedArchiverClassName("_TtCC8nscoding7CodingA7NestedA")}}
    required init(coder: NSCoder) { }
    func encode(coder: NSCoder) { }
  }

  class NestedB : NSObject {
    // expected-note@-1{{for new classes, add '@objc' to specify a unique, prefixed Objective-C runtime name}}{{3-3=@objc(<#Objective-C class name#>)}}
    // expected-note@-2{{for compatibility with existing archives, use '@NSKeyedArchiverClassName' to record the Swift 3 mangled name}}{{3-3=@NSKeyedArchiverClassName("_TtCC8nscoding7CodingA7NestedB")}}
    required init(coder: NSCoder) { }
    func encode(coder: NSCoder) { }
  }

  @objc(CodingA_NestedC)
  class NestedC : NSObject, NSCoding {
    required init(coder: NSCoder) { }
    func encode(coder: NSCoder) { }
  }

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
class CodingB<T> : NSObject, NSCoding {   // expected-error{{generic class 'CodingB<T>' has an unstable name when archiving via 'NSCoding'}}
  // expected-note@-1{{generic class 'CodingB<T>' should not be archived directly}}{{1-1=@NSKeyedArchiverEncodeNonGenericSubclassesOnly}}
  required init(coder: NSCoder) { }
  func encode(coder: NSCoder) { }
}

extension CodingB {
  class NestedA : NSObject, NSCoding { // expected-error{{generic class 'CodingB<T>.NestedA' has an unstable name when archiving via 'NSCoding'}}
    // expected-note@-1{{generic class 'CodingB<T>.NestedA' should not be archived directly}}{{3-3=@NSKeyedArchiverEncodeNonGenericSubclassesOnly}}
    required init(coder: NSCoder) { }
    func encode(coder: NSCoder) { }
  }
}

// Fileprivate classes.
fileprivate class CodingC : NSObject, NSCoding {    // expected-error{{fileprivate class 'CodingC' has an unstable name when archiving via 'NSCoding'}}
  // expected-note@-1{{for new classes, add '@objc' to specify a unique, prefixed Objective-C runtime name}}{{1-1=@objc(<#Objective-C class name#>)}}
  // expected-note@-2{{for compatibility with existing archives, use '@NSKeyedArchiverClassName' to record the Swift 3 mangled name}}{{1-1=@NSKeyedArchiverClassName("_TtC8nscodingP33_0B4E7641C0BD1F170280EEDD0D0C1F6C7CodingC")}}
  required init(coder: NSCoder) { }
  func encode(coder: NSCoder) { }
}

// Private classes
private class CodingD : NSObject, NSCoding {       // expected-error{{private class 'CodingD' has an unstable name when archiving via 'NSCoding'}}
  // expected-note@-1{{for new classes, add '@objc' to specify a unique, prefixed Objective-C runtime name}}{{1-1=@objc(<#Objective-C class name#>)}}
  // expected-note@-2{{for compatibility with existing archives, use '@NSKeyedArchiverClassName' to record the Swift 3 mangled name}}{{1-1=@NSKeyedArchiverClassName("_TtC8nscodingP33_0B4E7641C0BD1F170280EEDD0D0C1F6C7CodingD")}}
  required init(coder: NSCoder) { }
  func encode(coder: NSCoder) { }
}

// Local classes.
func someFunction() {
  class LocalCoding : NSObject, NSCoding {       // expected-error{{local class 'LocalCoding' has an unstable name when archiving via 'NSCoding'}}
  // expected-note@-1{{for new classes, add '@objc' to specify a unique, prefixed Objective-C runtime name}}{{3-3=@objc(<#Objective-C class name#>)}}
  // expected-note@-2{{for compatibility with existing archives, use '@NSKeyedArchiverClassName' to record the Swift 3 mangled name}}{{3-3=@NSKeyedArchiverClassName("_TtCF8nscoding12someFunctionFT_T_L_11LocalCoding")}}
  required init(coder: NSCoder) { }
  func encode(coder: NSCoder) { }
}
}

// Inherited conformances.
class CodingE<T> : CodingB<T> {   // expected-error{{generic class 'CodingE<T>' has an unstable name when archiving via 'NSCoding'}}
    // expected-note@-1{{generic class 'CodingE<T>' should not be archived directly}}{{1-1=@NSKeyedArchiverEncodeNonGenericSubclassesOnly}}
  required init(coder: NSCoder) { super.init(coder: coder) }
  override func encode(coder: NSCoder) { }
}

// @objc suppressions
extension CodingA {
  @objc(TheNestedE)
  class NestedE : NSObject, NSCoding {
    required init(coder: NSCoder) { }
    func encode(coder: NSCoder) { }
  }
}

@NSKeyedArchiverEncodeNonGenericSubclassesOnly
class CodingGeneric<T> : NSObject, NSCoding {
  required init(coder: NSCoder) { }
  func encode(coder: NSCoder) { }
}

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
  @objc(GenericViaScope) // expected-error {{generic subclasses of '@objc' classes cannot have an explicit '@objc' because they are not directly visible from Objective-C}}
  class GenericViaScope : NSObject { }
}

// Inference of @_staticInitializeObjCMetadata.
class SubclassOfCodingE : CodingE<Int> { }

// But don't allow one to write @_staticInitializeObjCMetadata!
@_staticInitializeObjCMetadata // expected-error{{unknown attribute '_staticInitializeObjCMetadata'}}
class DontAllowStaticInits { }

// CHECK-NOT: class_decl "CodingA"{{.*}}@_staticInitializeObjCMetadata
// CHECK: class_decl "NestedA"{{.*}}@_staticInitializeObjCMetadata
// CHECK: class_decl "NestedC"{{.*}}@_staticInitializeObjCMetadata
// CHECK: class_decl "NestedE"{{.*}}@_staticInitializeObjCMetadata
// CHECK-NOT: class_decl "CodingGeneric"{{.*}}@_staticInitializeObjCMetadata
// CHECK-NOT: class_decl "GenericViaScope"{{.*}}@_staticInitializeObjCMetadata
// CHECK: class_decl "SubclassOfCodingE"{{.*}}@_staticInitializeObjCMetadata
