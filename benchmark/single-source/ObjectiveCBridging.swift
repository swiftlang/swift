//===--- ObjectiveCBridging.swift -----------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import TestsUtils
import Foundation

#if _runtime(_ObjC)
@inline(never)
public func forcedCast<NS, T>(_ ns: NS) -> T {
  return ns as! T
}

@inline(never)
public func conditionalCast<NS, T>(_ ns: NS) -> T? {
  return ns as? T
}
#endif


// === String === //

#if _runtime(_ObjC)
func createNSString() -> NSString {
  return NSString(cString: "NSString that does not fit in tagged pointer", encoding: String.Encoding.utf8.rawValue)!
}

@inline(never)
func testObjectiveCBridgeFromNSString() {
  let nsString = createNSString()

  var s: String?
  for _ in 0 ..< 10_000 {
    // Call _conditionallyBridgeFromObjectiveC.
    let n : String? = conditionalCast(nsString)
    if n != nil {
      s = n!
    }
  }
  CheckResults(s != nil && s == "NSString that does not fit in tagged pointer", "Expected results did not match")
}
#endif

@inline(never)
public func run_ObjectiveCBridgeFromNSString(_ N: Int) {
#if _runtime(_ObjC)
  for _ in 0 ..< N {
    testObjectiveCBridgeFromNSString()
  }
#endif
}

#if _runtime(_ObjC)
@inline(never)
func testObjectiveCBridgeFromNSStringForced() {
  let nsString = createNSString()

  var s: String?
  for _ in 0 ..< 10_000 {
    // Call _forceBridgeFromObjectiveC
    s = forcedCast(nsString)
  }
  CheckResults(s != nil && s == "NSString that does not fit in tagged pointer", "Expected results did not match")
}
#endif

@inline(never)
public func run_ObjectiveCBridgeFromNSStringForced(_ N: Int) {
#if _runtime(_ObjC)
  for _ in 0 ..< N {
    testObjectiveCBridgeFromNSStringForced()
  }
#endif
}

#if _runtime(_ObjC)
@inline(never)
func testObjectiveCBridgeToNSString() {
  let nativeString = "Native"

  var s: NSString?
  for _ in 0 ..< 10_000 {
    // Call _BridgedToObjectiveC
    s = nativeString as NSString
  }
  CheckResults(s != nil && s == "Native", "Expected results did not match")
}
#endif

@inline(never)
public func run_ObjectiveCBridgeToNSString(_ N: Int) {
#if _runtime(_ObjC)
  for _ in 0 ..< N {
    testObjectiveCBridgeToNSString()
  }
#endif
}

// === Array === //

#if _runtime(_ObjC)
func createNSArray() -> NSArray {
  let nsMutableArray = NSMutableArray()
  let nsString = NSString(cString: "NSString that does not fit in tagged pointer", encoding: String.Encoding.utf8.rawValue)!
  nsMutableArray.add(nsString)
  nsMutableArray.add(nsString)
  nsMutableArray.add(nsString)
  nsMutableArray.add(nsString)
  nsMutableArray.add(nsString)
  nsMutableArray.add(nsString)
  nsMutableArray.add(nsString)
  nsMutableArray.add(nsString)
  nsMutableArray.add(nsString)
  nsMutableArray.add(nsString)
  nsMutableArray.add(nsString)
  return nsMutableArray.copy() as! NSArray
}

@inline(never)
func testObjectiveCBridgeFromNSArrayAnyObject() {
  let nsArray = createNSArray()

  var nativeString : String?
  for _ in 0 ..< 10_000 {
    if let nativeArray : [NSString] = conditionalCast(nsArray) {
       nativeString = forcedCast(nativeArray[0])
    }
  }
  CheckResults(nativeString != nil && nativeString! == "NSString that does not fit in tagged pointer", "Expected results did not match")
}
#endif

@inline(never)
public func run_ObjectiveCBridgeFromNSArrayAnyObject(_ N: Int) {
#if _runtime(_ObjC)
  for _ in 0 ..< N {
    testObjectiveCBridgeFromNSArrayAnyObject()
  }
#endif
}

#if _runtime(_ObjC)
@inline(never)
func testObjectiveCBridgeFromNSArrayAnyObjectForced() {
  let nsArray = createNSArray()

  var nativeString : String?
  for _ in 0 ..< 10_000 {
    let nativeArray : [NSString] = forcedCast(nsArray)
    nativeString = forcedCast(nativeArray[0])
  }
  CheckResults(nativeString != nil && nativeString! == "NSString that does not fit in tagged pointer", "Expected results did not match")
}
#endif

@inline(never)
public func run_ObjectiveCBridgeFromNSArrayAnyObjectForced(_ N: Int) {
#if _runtime(_ObjC)
  for _ in 0 ..< N {
    testObjectiveCBridgeFromNSArrayAnyObjectForced()
  }
#endif
}

#if _runtime(_ObjC)
@inline(never)
func testObjectiveCBridgeToNSArray() {
  let nativeArray = ["abcde", "abcde", "abcde", "abcde", "abcde",
    "abcde", "abcde", "abcde", "abcde", "abcde"]

  var nsString : Any?
  for _ in 0 ..< 10_000 {
    let nsArray = nativeArray as NSArray
    nsString = nsArray.object(at: 0)
  }
  CheckResults(nsString != nil && (nsString! as! NSString).isEqual("abcde"), "Expected results did not match")
}
#endif

@inline(never)
public func run_ObjectiveCBridgeToNSArray(_ N: Int) {
#if _runtime(_ObjC)
  for _ in 0 ..< N {
    testObjectiveCBridgeToNSArray()
  }
#endif
}

#if _runtime(_ObjC)
@inline(never)
func testObjectiveCBridgeFromNSArrayAnyObjectToString() {
  let nsArray = createNSArray()

  var nativeString : String?
  for _ in 0 ..< 10_000 {
    if let nativeArray : [String] = conditionalCast(nsArray) {
      nativeString = nativeArray[0]
    }
  }
  CheckResults(nativeString != nil && nativeString == "NSString that does not fit in tagged pointer", "Expected results did not match")
}
#endif

@inline(never)
public func run_ObjectiveCBridgeFromNSArrayAnyObjectToString(_ N: Int) {
#if _runtime(_ObjC)
  for _ in 0 ..< N {
    testObjectiveCBridgeFromNSArrayAnyObjectToString()
  }
#endif
}

#if _runtime(_ObjC)
@inline(never)
func testObjectiveCBridgeFromNSArrayAnyObjectToStringForced() {
  let nsArray = createNSArray()

  var nativeString : String?
  for _ in 0 ..< 10_000 {
    let nativeArray : [String] = forcedCast(nsArray)
    nativeString = nativeArray[0]
  }
  CheckResults(nativeString != nil && nativeString == "NSString that does not fit in tagged pointer", "Expected results did not match")
}
#endif

@inline(never)
public func run_ObjectiveCBridgeFromNSArrayAnyObjectToStringForced(_ N: Int) {
#if _runtime(_ObjC)
  for _ in 0 ..< N {
    testObjectiveCBridgeFromNSArrayAnyObjectToStringForced()
  }
#endif
}

// === Dictionary === //

#if _runtime(_ObjC)
func createNSDictionary() -> NSDictionary {
  let nsMutableDictionary = NSMutableDictionary()
  let nsString = NSString(cString: "NSString that does not fit in tagged pointer", encoding: String.Encoding.utf8.rawValue)!
  let nsString2 = NSString(cString: "NSString that does not fit in tagged pointer 2", encoding: String.Encoding.utf8.rawValue)!
  let nsString3 = NSString(cString: "NSString that does not fit in tagged pointer 3", encoding: String.Encoding.utf8.rawValue)!
  let nsString4 = NSString(cString: "NSString that does not fit in tagged pointer 4", encoding: String.Encoding.utf8.rawValue)!
  let nsString5 = NSString(cString: "NSString that does not fit in tagged pointer 5", encoding: String.Encoding.utf8.rawValue)!
  let nsString6 = NSString(cString: "NSString that does not fit in tagged pointer 6", encoding: String.Encoding.utf8.rawValue)!
  let nsString7 = NSString(cString: "NSString that does not fit in tagged pointer 7", encoding: String.Encoding.utf8.rawValue)!
  let nsString8 = NSString(cString: "NSString that does not fit in tagged pointer 8", encoding: String.Encoding.utf8.rawValue)!
  let nsString9 = NSString(cString: "NSString that does not fit in tagged pointer 9", encoding: String.Encoding.utf8.rawValue)!
  let nsString10 = NSString(cString: "NSString that does not fit in tagged pointer 10", encoding: String.Encoding.utf8.rawValue)!
  nsMutableDictionary.setObject(1, forKey: nsString)
  nsMutableDictionary.setObject(2, forKey: nsString2)
  nsMutableDictionary.setObject(3, forKey: nsString3)
  nsMutableDictionary.setObject(4, forKey: nsString4)
  nsMutableDictionary.setObject(5, forKey: nsString5)
  nsMutableDictionary.setObject(6, forKey: nsString6)
  nsMutableDictionary.setObject(7, forKey: nsString7)
  nsMutableDictionary.setObject(8, forKey: nsString8)
  nsMutableDictionary.setObject(9, forKey: nsString9)
  nsMutableDictionary.setObject(10, forKey: nsString10)

  return nsMutableDictionary.copy() as! NSDictionary
}

@inline(never)
func testObjectiveCBridgeFromNSDictionaryAnyObject() {
  let nsDictionary = createNSDictionary()
  let nsString = NSString(cString: "NSString that does not fit in tagged pointer", encoding: String.Encoding.utf8.rawValue)!

  var nativeInt : Int?
  for _ in 0 ..< 10_000 {
    if let nativeDictionary : [NSString : NSNumber] = conditionalCast(nsDictionary) {
       nativeInt = forcedCast(nativeDictionary[nsString])
    }
  }
  CheckResults(nativeInt != nil && nativeInt == 1, "Expected results did not match")
}
#endif

@inline(never)
public func run_ObjectiveCBridgeFromNSDictionaryAnyObject(_ N: Int) {
#if _runtime(_ObjC)
  for _ in 0 ..< N {
    testObjectiveCBridgeFromNSDictionaryAnyObject()
  }
#endif
}

#if _runtime(_ObjC)
@inline(never)
func testObjectiveCBridgeFromNSDictionaryAnyObjectForced() {
  let nsDictionary = createNSDictionary()
  let nsString = NSString(cString: "NSString that does not fit in tagged pointer", encoding: String.Encoding.utf8.rawValue)!

  var nativeInt : Int?
  for _ in 0 ..< 10_000 {
    if let nativeDictionary : [NSString : NSNumber] = forcedCast(nsDictionary) {
       nativeInt = forcedCast(nativeDictionary[nsString])
    }
  }
  CheckResults(nativeInt != nil && nativeInt == 1, "Expected results did not match")
}
#endif

@inline(never)
public func run_ObjectiveCBridgeFromNSDictionaryAnyObjectForced(_ N: Int) {
#if _runtime(_ObjC)
  for _ in 0 ..< N {
    testObjectiveCBridgeFromNSDictionaryAnyObjectForced()
  }
#endif
}

#if _runtime(_ObjC)
@inline(never)
func testObjectiveCBridgeToNSDictionary() {
  let nativeDictionary = ["abcde1": 1, "abcde2": 2, "abcde3": 3, "abcde4": 4,
    "abcde5": 5, "abcde6": 6, "abcde7": 7, "abcde8": 8, "abcde9": 9,
    "abcde10": 10]
  let key = "abcde1" as NSString

  var nsNumber : Any?
  for _ in 0 ..< 10_000 {
    let nsDict = nativeDictionary as NSDictionary
    nsNumber = nsDict.object(forKey: key)
  }
  CheckResults(nsNumber != nil && (nsNumber as! Int) == 1, "Expected results did not match")
}
#endif

@inline(never)
public func run_ObjectiveCBridgeToNSDictionary(_ N: Int) {
#if _runtime(_ObjC)
  for _ in 0 ..< N {
    testObjectiveCBridgeToNSDictionary()
  }
#endif
}

#if _runtime(_ObjC)
@inline(never)
func testObjectiveCBridgeFromNSDictionaryAnyObjectToString() {
  let nsDictionary = createNSDictionary()
  let nsString = NSString(cString: "NSString that does not fit in tagged pointer", encoding: String.Encoding.utf8.rawValue)!
  let nativeString = nsString as String

  var nativeInt : Int?
  for _ in 0 ..< 10_000 {
    if let nativeDictionary : [String : Int] = conditionalCast(nsDictionary) {
       nativeInt = nativeDictionary[nativeString]
    }
  }
  CheckResults(nativeInt != nil && nativeInt == 1, "Expected results did not match")
}
#endif

@inline(never)
public func run_ObjectiveCBridgeFromNSDictionaryAnyObjectToString(_ N: Int) {
#if _runtime(_ObjC)
  for _ in 0 ..< N {
    testObjectiveCBridgeFromNSDictionaryAnyObjectToString()
  }
#endif
}

#if _runtime(_ObjC)
@inline(never)
func testObjectiveCBridgeFromNSDictionaryAnyObjectToStringForced() {
  let nsDictionary = createNSDictionary()
  let nsString = NSString(cString: "NSString that does not fit in tagged pointer", encoding: String.Encoding.utf8.rawValue)!
  let nativeString = nsString as String

  var nativeInt : Int?
  for _ in 0 ..< 10_000 {
    if let nativeDictionary : [String : Int] = forcedCast(nsDictionary) {
       nativeInt = nativeDictionary[nativeString]
    }
  }
  CheckResults(nativeInt != nil && nativeInt == 1, "Expected results did not match")
}
#endif

@inline(never)
public func run_ObjectiveCBridgeFromNSDictionaryAnyObjectToStringForced(_ N: Int) {
#if _runtime(_ObjC)
  for _ in 0 ..< N {
    testObjectiveCBridgeFromNSDictionaryAnyObjectToStringForced()
  }
#endif
}

// === Set === //

#if _runtime(_ObjC)
func createNSSet() -> NSSet {
  let nsMutableSet = NSMutableSet()
  let nsString = NSString(cString: "NSString that does not fit in tagged pointer", encoding: String.Encoding.utf8.rawValue)!
  let nsString2 = NSString(cString: "NSString that does not fit in tagged pointer 2", encoding: String.Encoding.utf8.rawValue)!
  let nsString3 = NSString(cString: "NSString that does not fit in tagged pointer 3", encoding: String.Encoding.utf8.rawValue)!
  let nsString4 = NSString(cString: "NSString that does not fit in tagged pointer 4", encoding: String.Encoding.utf8.rawValue)!
  let nsString5 = NSString(cString: "NSString that does not fit in tagged pointer 5", encoding: String.Encoding.utf8.rawValue)!
  let nsString6 = NSString(cString: "NSString that does not fit in tagged pointer 6", encoding: String.Encoding.utf8.rawValue)!
  let nsString7 = NSString(cString: "NSString that does not fit in tagged pointer 7", encoding: String.Encoding.utf8.rawValue)!
  let nsString8 = NSString(cString: "NSString that does not fit in tagged pointer 8", encoding: String.Encoding.utf8.rawValue)!
  let nsString9 = NSString(cString: "NSString that does not fit in tagged pointer 9", encoding: String.Encoding.utf8.rawValue)!
  let nsString10 = NSString(cString: "NSString that does not fit in tagged pointer 10", encoding: String.Encoding.utf8.rawValue)!
  nsMutableSet.add(nsString)
  nsMutableSet.add(nsString2)
  nsMutableSet.add(nsString3)
  nsMutableSet.add(nsString4)
  nsMutableSet.add(nsString5)
  nsMutableSet.add(nsString6)
  nsMutableSet.add(nsString7)
  nsMutableSet.add(nsString8)
  nsMutableSet.add(nsString9)
  nsMutableSet.add(nsString10)

  return nsMutableSet.copy() as! NSSet
}


@inline(never)
func testObjectiveCBridgeFromNSSetAnyObject() {
  let nsSet = createNSSet()
  let nsString = NSString(cString: "NSString that does not fit in tagged pointer", encoding: String.Encoding.utf8.rawValue)!

  var result : Bool?
  for _ in 0 ..< 10_000 {
    if let nativeSet : Set<NSString> = conditionalCast(nsSet) {
       result = nativeSet.contains(nsString)
    }
  }
  CheckResults(result != nil && result!, "Expected results did not match")
}
#endif

@inline(never)
public func run_ObjectiveCBridgeFromNSSetAnyObject(_ N: Int) {
#if _runtime(_ObjC)
  for _ in 0 ..< N {
    testObjectiveCBridgeFromNSSetAnyObject()
  }
#endif
}

#if _runtime(_ObjC)
@inline(never)
func testObjectiveCBridgeFromNSSetAnyObjectForced() {
  let nsSet = createNSSet()
  let nsString = NSString(cString: "NSString that does not fit in tagged pointer", encoding: String.Encoding.utf8.rawValue)!

  var result : Bool?
  for _ in 0 ..< 10_000 {
    if let nativeSet : Set<NSString> = forcedCast(nsSet) {
       result = nativeSet.contains(nsString)
    }
  }
  CheckResults(result != nil && result!, "Expected results did not match")
}
#endif

@inline(never)
public func run_ObjectiveCBridgeFromNSSetAnyObjectForced(_ N: Int) {
#if _runtime(_ObjC)
  for _ in 0 ..< N {
    testObjectiveCBridgeFromNSSetAnyObjectForced()
  }
#endif
}

#if _runtime(_ObjC)
@inline(never)
func testObjectiveCBridgeToNSSet() {
  let nativeSet = Set<String>(["abcde1", "abcde2", "abcde3", "abcde4", "abcde5",
      "abcde6", "abcde7", "abcde8", "abcde9", "abcde10"])
  let key = "abcde1" as NSString

  var nsString : Any?
  for _ in 0 ..< 10_000 {
    let nsDict = nativeSet as NSSet
    nsString = nsDict.member(key)
  }
  CheckResults(nsString != nil && (nsString as! String) == "abcde1", "Expected results did not match")
}
#endif

@inline(never)
public func run_ObjectiveCBridgeToNSSet(_ N: Int) {
#if _runtime(_ObjC)
  for _ in 0 ..< N {
    testObjectiveCBridgeToNSSet()
  }
#endif
}

#if _runtime(_ObjC)
@inline(never)
func testObjectiveCBridgeFromNSSetAnyObjectToString() {
  let nsString = NSString(cString: "NSString that does not fit in tagged pointer", encoding: String.Encoding.utf8.rawValue)!
  let nativeString = nsString as String
  let nsSet = createNSSet()

  var result : Bool?
  for _ in 0 ..< 10_000 {
    if let nativeSet : Set<String> = conditionalCast(nsSet) {
       result = nativeSet.contains(nativeString)
    }
  }
  CheckResults(result != nil && result!, "Expected results did not match")
}
#endif

@inline(never)
public func run_ObjectiveCBridgeFromNSSetAnyObjectToString(_ N: Int) {
#if _runtime(_ObjC)
  for _ in 0 ..< N {
    testObjectiveCBridgeFromNSSetAnyObjectToString()
  }
#endif
}

#if _runtime(_ObjC)
@inline(never)
func testObjectiveCBridgeFromNSSetAnyObjectToStringForced() {
  let nsSet = createNSSet()
  let nsString = NSString(cString: "NSString that does not fit in tagged pointer", encoding: String.Encoding.utf8.rawValue)!
  let nativeString = nsString as String

  var result : Bool?
  for _ in 0 ..< 10_000 {
    if let nativeSet : Set<String> = forcedCast(nsSet) {
       result = nativeSet.contains(nativeString)
    }
  }
  CheckResults(result != nil && result!, "Expected results did not match")
}
#endif

@inline(never)
public func run_ObjectiveCBridgeFromNSSetAnyObjectToStringForced(_ N: Int) {
#if _runtime(_ObjC)
  for _ in 0 ..< N {
    testObjectiveCBridgeFromNSSetAnyObjectToStringForced()
  }
#endif
}
