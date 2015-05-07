// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -parse -parse-as-library -verify %s

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
  var sf : (String!) -> String? = NSStringToNSString
  var s : String = NSArray().nsstringProperty.onlyOnString()

  var bf : (Bool) -> Bool = BOOLtoBOOL
  var b : Bool = NSArray().boolProperty.onlyOnBool()

  var af: (Array<AnyObject>!) -> (Array<AnyObject>!) = arrayToArray
  NSArray().arrayProperty.onlyOnArray()

  var df: (Dictionary<NSObject, AnyObject>!) -> Dictionary<NSObject, AnyObject>!
    = dictToDict
  NSArray().dictProperty.onlyOnDictionary()

  var setf : (Set<NSObject>!) -> Set<NSObject>! = setToSet
  NSArray().setProperty.onlyOnSet()
}

func allocateMagic(zone: NSZone) -> UnsafeMutablePointer<Void> {
  return allocate(zone)
}

func constPointerToObjC(objects: [AnyObject?]) -> NSArray {
  return NSArray(objects: objects, count: objects.count)
}

func mutablePointerToObjC(path: String) throws -> NSString {
  return try NSString(contentsOfFile: path)
}

func objcStructs(s: StructOfNSStrings) {
  // Struct fields must not be bridged.
  let x: NSString = s.nsstr!
}
