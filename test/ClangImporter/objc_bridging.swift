// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -parse-as-library -verify %s

// REQUIRES: objc_interop

import Foundation
import objc_structs

extension String {
  func onlyOnString() -> String { return self }
}

extension Bool {
  func onlyOnBool() -> Bool { return self }
}

extension Array {
  func onlyOnArray() { }
}

extension Dictionary {
  func onlyOnDictionary() { }
}

extension Set {
  func onlyOnSet() { }
}

func foo() {
  _  = NSStringToNSString as (String?) -> String? // expected-warning {{redundant cast to '(String?) -> String?' has no effect}} {{27-51=}}
  _ = DummyClass().nsstringProperty.onlyOnString() as String

  _  = BOOLtoBOOL as (Bool) -> Bool // expected-warning {{redundant cast to '(Bool) -> Bool' has no effect}} {{19-37=}}
  _  = DummyClass().boolProperty.onlyOnBool() as Bool

  _  = arrayToArray as (Array<Any>?) -> (Array<Any>?) // expected-warning {{redundant cast to '(Array<Any>?) -> (Array<Any>?)' has no effect}} {{21-55=}}
  DummyClass().arrayProperty.onlyOnArray()

  _ = dictToDict as (Dictionary<AnyHashable, Any>?) -> Dictionary<AnyHashable, Any>? // expected-warning {{redundant cast to '(Dictionary<AnyHashable, Any>?) -> Dictionary<AnyHashable, Any>?' has no effect}} {{18-86=}}

  DummyClass().dictProperty.onlyOnDictionary()

  _ = setToSet as (Set<AnyHashable>?) -> Set<AnyHashable>? // expected-warning {{redundant cast to '(Set<AnyHashable>?) -> Set<AnyHashable>?' has no effect}} {{16-60=}}
  DummyClass().setProperty.onlyOnSet()
}

func allocateMagic(_ zone: NSZone) -> UnsafeMutableRawPointer {
  return allocate(zone)
}

func constPointerToObjC(_ objects: [AnyObject]) -> NSArray {
  return NSArray(objects: objects, count: objects.count)
}

func mutablePointerToObjC(_ path: String) throws -> NSString {
  return try NSString(contentsOfFile: path)
}

func objcStructs(_ s: StructOfNSStrings, sb: StructOfBlocks) {
  // Struct fields must not be bridged.
  _ = s.nsstr! as Bool // expected-error {{cannot convert value of type 'Unmanaged<NSString>' to type 'Bool' in coercion}}

  // FIXME: Blocks should also be Unmanaged.
  _ = sb.block as Bool // expected-error {{cannot convert value of type '@convention(block) () -> Void' to type 'Bool' in coercion}}
  sb.block() // okay
}
