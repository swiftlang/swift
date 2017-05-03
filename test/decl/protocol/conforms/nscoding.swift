// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -parse-as-library -swift-version 4 %s -verify

// RUN: not %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -parse-as-library -swift-version 4 %s -dump-ast 2> %t.ast
// RUN: %FileCheck %s < %t.ast

// REQUIRES: objc_interop

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
    // expected-note@-2{{for compatibility with existing archives, use '@NSKeyedArchiveLegacy' to record the Swift 3 mangled name}}{{3-3=@NSKeyedArchiveLegacy("<#class archival name#>")}}
    required init(coder: NSCoder) { }
    func encode(coder: NSCoder) { }
  }

  class NestedB : NSObject {
    // expected-note@-1{{for new classes, add '@objc' to specify a unique, prefixed Objective-C runtime name}}{{3-3=@objc(<#Objective-C class name#>)}}
    // expected-note@-2{{for compatibility with existing archives, use '@NSKeyedArchiveLegacy' to record the Swift 3 mangled name}}{{3-3=@NSKeyedArchiveLegacy("<#class archival name#>")}}
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
  // expected-note@-1{{generic classes should not be archived directly}}{{1-1=@NSKeyedArchiveSubclassesOnly}}
  required init(coder: NSCoder) { }
  func encode(coder: NSCoder) { }
}

extension CodingB {
  class NestedA : NSObject, NSCoding { // expected-error{{generic class 'CodingB<T>.NestedA' has an unstable name when archiving via 'NSCoding'}}
    // expected-note@-1{{generic classes should not be archived directly}}{{3-3=@NSKeyedArchiveSubclassesOnly}}
    required init(coder: NSCoder) { }
    func encode(coder: NSCoder) { }
  }
}

// Fileprivate classes.
fileprivate class CodingC : NSObject, NSCoding {    // expected-error{{fileprivate class 'CodingC' has an unstable name when archiving via 'NSCoding'}}
  // expected-note@-1{{for new classes, add '@objc' to specify a unique, prefixed Objective-C runtime name}}{{1-1=@objc(<#Objective-C class name#>)}}
  // expected-note@-2{{for compatibility with existing archives, use '@NSKeyedArchiveLegacy' to record the Swift 3 mangled name}}{{1-1=@NSKeyedArchiveLegacy("<#class archival name#>")}}
  required init(coder: NSCoder) { }
  func encode(coder: NSCoder) { }
}

// Private classes
private class CodingD : NSObject, NSCoding {       // expected-error{{private class 'CodingD' has an unstable name when archiving via 'NSCoding'}}
  // expected-note@-1{{for new classes, add '@objc' to specify a unique, prefixed Objective-C runtime name}}{{1-1=@objc(<#Objective-C class name#>)}}
  // expected-note@-2{{for compatibility with existing archives, use '@NSKeyedArchiveLegacy' to record the Swift 3 mangled name}}{{1-1=@NSKeyedArchiveLegacy("<#class archival name#>")}}
  required init(coder: NSCoder) { }
  func encode(coder: NSCoder) { }
}

// Local classes.
func someFunction() {
  class LocalCoding : NSObject, NSCoding {       // expected-error{{local class 'LocalCoding' has an unstable name when archiving via 'NSCoding'}}
  // expected-note@-1{{for new classes, add '@objc' to specify a unique, prefixed Objective-C runtime name}}{{3-3=@objc(<#Objective-C class name#>)}}
  // expected-note@-2{{for compatibility with existing archives, use '@NSKeyedArchiveLegacy' to record the Swift 3 mangled name}}{{3-3=@NSKeyedArchiveLegacy("<#class archival name#>")}}
  required init(coder: NSCoder) { }
  func encode(coder: NSCoder) { }
}
}

// Inherited conformances.
class CodingE<T> : CodingB<T> {   // expected-error{{generic class 'CodingE<T>' has an unstable name when archiving via 'NSCoding'}}
    // expected-note@-1{{generic classes should not be archived directly}}{{1-1=@NSKeyedArchiveSubclassesOnly}}
  required init(coder: NSCoder) { super.init(coder: coder) }
  override func encode(coder: NSCoder) { }
}

// @NSKeyedArchiveLegacy suppressions
extension CodingA {
  @NSKeyedArchiveLegacy("TheNestedE")
  class NestedE : NSObject, NSCoding {
    required init(coder: NSCoder) { }
    func encode(coder: NSCoder) { }
  }
}

@NSKeyedArchiveSubclassesOnly
class CodingGeneric<T> : NSObject, NSCoding {
  required init(coder: NSCoder) { }
  func encode(coder: NSCoder) { }
}

@NSKeyedArchiveLegacy("TheCodingF")
fileprivate class CodingF : NSObject, NSCoding {
  required init(coder: NSCoder) { }
  func encode(coder: NSCoder) { }
}

@NSKeyedArchiveLegacy("TheCodingG")
private class CodingG : NSObject, NSCoding {
  required init(coder: NSCoder) { }
  func encode(coder: NSCoder) { }
}

// Errors with @NSKeyedArchiveLegacy.
@NSKeyedArchiveLegacy("TheCodingG") // expected-error{{@NSKeyedArchiveLegacy may only be used on 'class' declarations}}
struct Foo { }

@NSKeyedArchiveLegacy("TheCodingG") // expected-error{{'@NSKeyedArchiveLegacy' cannot be applied to generic class 'Bar<T>'}}
class Bar<T> : NSObject { }

extension CodingB {
  @NSKeyedArchiveLegacy("GenericViaParent") // expected-error{{'@NSKeyedArchiveLegacy' cannot be applied to generic class 'CodingB<T>.GenericViaParent'}}
  class GenericViaParent : NSObject { }
}

// Inference of @_staticInitializeObjCMetadata.
class SubclassOfCodingE : CodingE<Int> { }

// CHECK-NOT: class_decl "CodingA"{{.*}}@_staticInitializeObjCMetadata
// CHECK: class_decl "NestedA"{{.*}}@_staticInitializeObjCMetadata
// CHECK: class_decl "NestedC"{{.*}}@_staticInitializeObjCMetadata
// CHECK-NOT: class_decl "NestedE"{{.*}}@_staticInitializeObjCMetadata
// CHECK-NOT: class_decl "CodingGeneric"{{.*}}@_staticInitializeObjCMetadata
// CHECK: class_decl "SubclassOfCodingE"{{.*}}@_staticInitializeObjCMetadata