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
  _  = NSStringToNSString as (String!) -> String?
  _ = DummyClass().nsstringProperty.onlyOnString() as String

  _  = BOOLtoBOOL as (Bool) -> Bool
  _  = DummyClass().boolProperty.onlyOnBool() as Bool

  _  = arrayToArray as (Array<Any>!) -> (Array<Any>!)
  DummyClass().arrayProperty.onlyOnArray()

  _ = dictToDict as (Dictionary<AnyHashable, Any>!) -> Dictionary<AnyHashable, Any>!

  DummyClass().dictProperty.onlyOnDictionary()

  _ = setToSet as (Set<AnyHashable>!) -> Set<AnyHashable>!
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
