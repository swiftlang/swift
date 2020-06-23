// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -parse-as-library -swift-version 4 %s -verify
// RUN: not %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -parse-as-library -swift-version 4 %s -disable-nskeyedarchiver-diagnostics 2>&1 | %FileCheck -check-prefix CHECK-NO-DIAGS %s
// RUN: not %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -parse-as-library -swift-version 4 %s -dump-ast -target %target-pre-stable-abi-triple > %t/old.ast

// RUN: %FileCheck --check-prefix=CHECK-OLD %s < %t/old.ast
// RUN: %FileCheck --check-prefix=NEGATIVE %s < %t/old.ast

// REQUIRES: objc_interop
// UNSUPPORTED: OS=iosmac
// UNSUPPORTED: CPU=arm64e

// See also nscoding_stable_abi.swift, for the stable ABI deployment
// target test.

// CHECK-NO-DIAGS-NOT: NSCoding
// CHECK-NO-DIAGS-NOT: unstable

import Foundation

// Top-level classes
// NEGATIVE-NOT: class_decl{{.*}}"CodingA"{{.*}}@_staticInitializeObjCMetadata
class CodingA : NSObject, NSCoding {
  required init(coder: NSCoder) { }
  func encode(coder: NSCoder) { }
  
}   // okay

// Nested classes
extension CodingA {
  // NEGATIVE-NOT: class_decl{{.*}}"NestedA"{{.*}}@_staticInitializeObjCMetadata
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

  // NEGATIVE-NOT: class_decl{{.*}}"NestedC"{{.*}}@_staticInitializeObjCMetadata
  @objc(CodingA_NestedC)
  class NestedC : NSObject, NSCoding {
    required init(coder: NSCoder) { }
    func encode(coder: NSCoder) { }
  }

  // NEGATIVE-NOT: class_decl{{.*}}"NestedD"{{.*}}@_staticInitializeObjCMetadata
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
// NEGATIVE-NOT: class_decl{{.*}}"CodingB"{{.*}}@_staticInitializeObjCMetadata
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
// NEGATIVE-NOT: class_decl{{.*}}"CodingC"{{.*}}@_staticInitializeObjCMetadata
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
// NEGATIVE-NOT: class_decl{{.*}}"CodingE"{{.*}}@_staticInitializeObjCMetadata
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
  // NEGATIVE-NOT: class_decl{{.*}}"GenericViaScope"{{.*}}@_staticInitializeObjCMetadata
  @objc(GenericViaScope) // expected-error {{generic subclasses of '@objc' classes cannot have an explicit '@objc' because they are not directly visible from Objective-C}}
  class GenericViaScope : NSObject { }
}

// Inference of @_staticInitializeObjCMetadata.
// NEGATIVE-NOT: class_decl{{.*}}"SubclassOfCodingA"{{.*}}@_staticInitializeObjCMetadata
class SubclassOfCodingA : CodingA { }

// CHECK-OLD: class_decl{{.*}}"SubclassOfCodingE"{{.*}}@_staticInitializeObjCMetadata
// NEGATIVE-NEW-NOT: class_decl{{.*}}"SubclassOfCodingE"{{.*}}@_staticInitializeObjCMetadata
class SubclassOfCodingE : CodingE<Int> { }

// Do not warn when simply inheriting from classes that conform to NSCoding.
// The subclass may never be serialized. But do still infer static
// initialization, just in case.
// NEGATIVE-NOT: class_decl{{.*}}"PrivateSubclassOfCodingA"{{.*}}@_staticInitializeObjCMetadata
private class PrivateSubclassOfCodingA : CodingA { }
// CHECK-OLD: class_decl{{.*}}"PrivateSubclassOfCodingE"{{.*}}@_staticInitializeObjCMetadata
// NEGATIVE-NEW-NOT: class_decl{{.*}}"PrivateSubclassOfCodingE"{{.*}}@_staticInitializeObjCMetadata
private class PrivateSubclassOfCodingE : CodingE<Int> { }

// But do warn when inherited through a protocol.
protocol AlsoNSCoding : NSCoding {}
private class CodingH : NSObject, AlsoNSCoding { // expected-error{{private class 'CodingH' has an unstable name when archiving via 'NSCoding'}}
  // expected-note@-1{{for compatibility with existing archives, use '@objc' to record the Swift 3 runtime name}}{{1-1=@objc(_TtC8nscodingP33_0B4E7641C0BD1F170280EEDD0D0C1F6C7CodingH)}}
  // expected-note@-2{{for new classes, use '@objc' to specify a unique, prefixed Objective-C runtime name}}{{1-1=@objc(<#prefixed Objective-C class name#>)}}
  required init(coder: NSCoder) { }
  func encode(coder: NSCoder) { }
}

// Don't allow one to write @_staticInitializeObjCMetadata!
@_staticInitializeObjCMetadata // expected-error{{unknown attribute '_staticInitializeObjCMetadata'}}
class DontAllowStaticInits { }
