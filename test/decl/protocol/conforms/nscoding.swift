// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -parse-as-library -swift-version 4 %s -verify

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
    required init(coder: NSCoder) { }
    func encode(coder: NSCoder) { }
  }

  class NestedB : NSObject {
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
  required init(coder: NSCoder) { }
  func encode(coder: NSCoder) { }
}

extension CodingB {
  class NestedA : NSObject, NSCoding { // expected-error{{generic class 'CodingB<T>.NestedA' has an unstable name when archiving via 'NSCoding'}}
    required init(coder: NSCoder) { }
    func encode(coder: NSCoder) { }
  }
}

// Fileprivate classes.
fileprivate class CodingC : NSObject, NSCoding {    // expected-error{{fileprivate class 'CodingC' has an unstable name when archiving via 'NSCoding'}}
  required init(coder: NSCoder) { }
  func encode(coder: NSCoder) { }
}

// Private classes
private class CodingD : NSObject, NSCoding {       // expected-error{{private class 'CodingD' has an unstable name when archiving via 'NSCoding'}}
  required init(coder: NSCoder) { }
  func encode(coder: NSCoder) { }
}

// Inherited conformances.
class CodingE<T> : CodingB<T> {   // expected-error{{generic class 'CodingE<T>' has an unstable name when archiving via 'NSCoding'}}
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
